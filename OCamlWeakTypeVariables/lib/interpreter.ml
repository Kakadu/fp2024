open Ast

type error =
  | Type_error
  | No_variable of string

let pp_error ppf : error -> _ = function
  | Type_error -> Format.fprintf ppf "Type error"
  | No_variable s -> Format.fprintf ppf "No variable with name %s" s
;;

type value =
  | Val_integer of int
  | Val_string of string
  | Val_boolean of bool
  | Val_fun of pattern * expression * env
  | Val_rec_fun of id * value
  | Val_function of case list * env
  | Val_tuple of value list
  | Val_construct of id * value option

and env = (string, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | Val_integer int -> fprintf ppf "%i" int
  | Val_boolean bool -> fprintf ppf "'%b'" bool
  | Val_string str -> fprintf ppf "%S" str
  | Val_tuple vls ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
      vls
  | Val_fun _ | Val_rec_fun _ -> fprintf ppf "<fun>"
  | Val_function _ -> fprintf ppf "<function>"
  | Val_construct (tag, None) -> fprintf ppf "%s" tag
  | Val_construct ("Some", Some value) -> fprintf ppf "Some %a" pp_value value
  | Val_construct (tag, Some v) -> fprintf ppf "[%s] %a" tag pp_value v
;;

module Res = struct
  open Base

  type 'a t = ('a, error) Result.t

  let fail = Result.fail
  let return = Result.return

  let ( >>= ) (monad : 'a t) (f : 'a -> 'b t) : 'b t =
    match monad with
    | Ok result -> f result
    | Error x -> fail x
  ;;

  let ( >>| ) (monad : 'a t) (f : 'a -> 'b) : 'b t =
    match monad with
    | Ok result -> return (f result)
    | Error x -> fail x
  ;;

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
end

module EvalEnv = struct
  open Base

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let compose env1 env2 =
    Map.fold env2 ~f:(fun ~key ~data env_acc -> extend env_acc key data) ~init:env1
  ;;

  let find_exn env key =
    match Map.find env key with
    | Some value -> Res.return value
    | None -> Res.fail (No_variable key)
  ;;

  let find_exn1 env key =
    let val' = Map.find_exn env key in
    val'
  ;;
end

module Inter = struct
  open Res
  open EvalEnv

  let rec match_pattern env = function
    | Ppat_any, _ -> return env
    | Ppat_var name, value -> return (extend env name value)
    | Ppat_constant _, _ -> return env
    | Ppat_tuple pts, Val_tuple values ->
      List.fold_left2
        (fun env pat value ->
          let* env = env in
          match_pattern env (pat, value))
        (return env)
        pts
        values
    | Ppat_construct (pat_name, None), Val_construct (val_name, None)
      when pat_name = val_name -> return env
    | Ppat_construct (pat_name, Some cnstr), Val_construct (val_name, Some value) ->
      if pat_name <> val_name then fail Type_error else match_pattern env (cnstr, value)
    | _ -> fail Type_error
  ;;

  let eval_const = function
    | Pconst_int c -> return (Val_integer c)
    | Pconst_string c -> return (Val_string c)
    | Pconst_boolean c -> return (Val_boolean c)
  ;;

  let eval_non_rec_let eval_expr env (NonRecursive, vbs, expr) =
    let* homka_env =
      Base.List.fold_left vbs ~init:(return env) ~f:(fun env vb ->
        let* env = env in
        let* homka_expr = eval_expr env vb.pvb_expr in
        match_pattern env (vb.pvb_pat, homka_expr))
    in
    eval_expr homka_env expr
  ;;

  let rec eval_expr env = function
    | Pexp_ident id -> find_exn env id
    | Pexp_constant const -> eval_const const
    | Pexp_apply (e0, es) ->
      let rec helper value0 es =
        match es with
        | e1 :: es ->
          let* value1 = eval_expr env e1 in
          let* result =
            match value0 with
            | Val_fun (fun_pat, fun_expr, fun_env) ->
              let* fun_env = match_pattern fun_env (fun_pat, value1) in
              let* res = eval_expr fun_env fun_expr in
              return res
            | Val_rec_fun (id, Val_fun (fun_pat, fun_expr, fun_env)) ->
              let fun_env = extend fun_env id value0 in
              let* fun_env = match_pattern fun_env (fun_pat, value1) in
              let* res = eval_expr fun_env fun_expr in
              return res
            | _ -> fail Type_error
          in
          helper result es
        | [] -> return value0
      in
      let* value0 = eval_expr env e0 in
      helper value0 es
    | Pexp_let (NonRecursive, vbs, expr) ->
      eval_non_rec_let eval_expr env (NonRecursive, vbs, expr)
    | Pexp_ifthenelse (e0, _, None) ->
      let* value_e0 = eval_expr env e0 in
      (* Without else branch return type must be unit *)
      (match value_e0 with
       | Val_construct ("()", None) as v -> return v
       | _ -> fail Type_error)
    | Pexp_ifthenelse (e0, e1, Some e2) ->
      let* value_e0 = eval_expr env e0 in
      (match value_e0 with
       | Val_boolean true -> eval_expr env e1
       | Val_boolean false -> eval_expr env e2
       | _ -> fail Type_error)
    | Pexp_fun (pat, expr) -> Val_fun (pat, expr, env) |> return
    | Pexp_tuple exprs ->
      let rec helper (acc : value list) = function
        | e0 :: es ->
          let* value = eval_expr env e0 in
          helper (value :: acc) es
        | [] -> return acc
      in
      let+ values = helper [] exprs in
      Val_tuple values
    | Pexp_construct ("Some", Some expr) ->
      let+ value = eval_expr env expr in
      Val_construct ("Some", Some value)
    | Pexp_construct ("None", None) -> return (Val_construct ("None", None))
    | _ -> failwith "homka"
  ;;

  let eval_structure env = function
    | Pstr_eval expr ->
      let* value = eval_expr env expr in
      return value
  ;;

  let run expr =
    match Parser.parse expr with
    | Ok str ->
      (match str with
       | Pstr_eval expr ->
         (match eval_expr EvalEnv.empty expr with
          | Ok v -> Format.printf "%a" pp_value v
          | Error e -> Format.printf "%a" pp_error e))
    | Error e -> Format.printf "AA %s" e
  ;;

  let%expect_test _ =
    run {|let homka = (fun x -> x) (fun x -> x) (fun x -> x) 122 in homka|};
    [%expect {| 122 |}]
  ;;
end

let empty_env = EvalEnv.empty
let interpret = Inter.eval_structure
let run_interpret = interpret empty_env

let run_interpret_exn str =
  match interpret empty_env str with
  | Ok value -> Ok value
  | Error e -> Error (Format.asprintf "%a" pp_error e)
;;
