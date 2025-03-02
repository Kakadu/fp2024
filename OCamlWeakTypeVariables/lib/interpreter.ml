open Ast

type error =
  | Type_error
  | Pattern_error of pattern
  | Eval_expr_error of expression
  | No_variable of string
  | Match_error

module Res : sig
  type 'a t

  val fail : error -> 'a t
  val return : 'a -> 'a t
  val run : 'a t -> ('a, error) result
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end = struct
  open Base

  type 'a t = ('a, error) Result.t

  let fail = Result.fail
  let return = Result.return
  let run m = m

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

  let ( <|> ) (first : 'a t) (second : 'a t) : 'a t =
    match first with
    | Ok result -> return result
    | Error _ ->
      (match second with
       | Ok result -> return result
       | Error e -> fail e)
  ;;

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
end

module rec Value : sig
  type value =
    | Val_integer of int
    | Val_string of string
    | Val_boolean of bool
    | Val_fun of pattern * expression * EvalEnv.t
    | Val_rec_fun of id * value
    | Val_function of case list * EvalEnv.t
    | Val_tuple of value list
    | Val_construct of id * value option
end = struct
  type value =
    | Val_integer of int
    | Val_string of string
    | Val_boolean of bool
    | Val_fun of pattern * expression * EvalEnv.t
    | Val_rec_fun of id * value
    | Val_function of case list * EvalEnv.t
    | Val_tuple of value list
    | Val_construct of id * value option

  let rec pp ppf =
    let open Stdlib.Format in
    function
    | Val_integer int -> fprintf ppf "%i" int
    | Val_boolean bool -> fprintf ppf "'%b'" bool
    | Val_string str -> fprintf ppf "%S" str
    | Val_tuple vls ->
      fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp) vls
    | Val_fun _ | Val_rec_fun _ -> fprintf ppf "<fun>"
    | Val_function _ -> fprintf ppf "<function>"
    | Val_construct (tag, None) -> fprintf ppf "%s" tag
    | Val_construct ("Some", Some value) -> fprintf ppf "Some %a" pp value
    | Val_construct (tag, Some v) -> fprintf ppf "[%s] %a" tag pp v
  ;;
end

and EvalEnv : sig
  type t

  val empty : t
  val extend : t -> string -> Value.value -> t
  val compose : t -> t -> t
  val find_exn : t -> string -> Value.value Res.t
  val find_exn1 : t -> string -> Value.value
end = struct
  open Base

  type t = (id, Value.value, String.comparator_witness) Map.t

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

let pp_error ppf : error -> _ = function
  | Type_error -> Format.fprintf ppf "Type error"
  | Pattern_error pat ->
    Format.fprintf ppf "Error while interpret pattern (%a)" pp_pattern pat
  | Eval_expr_error expr ->
    Format.printf "Error while interpret expression (%a)" pp_expression expr
  | No_variable s -> Format.fprintf ppf "No variable with name %s" s
  | Match_error -> Format.fprintf ppf "Match failure"
;;

module Inter = struct
  open Value
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
    | o, _ -> fail (Pattern_error o)
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

  let eval_rec_let eval_expr env (Recursive, vbs, expr) =
    let* homka_env =
      Base.List.fold_left vbs ~init:(return env) ~f:(fun env vb ->
        let* env = env in
        let* homka_expr = eval_expr env vb.pvb_expr in
        let* homka_expr =
          match vb.pvb_pat with
          | Ppat_var name ->
            (match homka_expr with
             | Val_fun _ as v -> return (Val_rec_fun (name, v))
             | v -> return v)
          | _ -> fail Type_error
        in
        match_pattern env (vb.pvb_pat, homka_expr))
    in
    eval_expr homka_env expr
  ;;

  let eval_cases eval_expr env cases init_value =
    let rec helper cases =
      match cases with
      | [] -> fail Match_error
      | case :: tl ->
        let* res = match_pattern env (case.pc_lhs, init_value) <|> helper tl in
        return res
    in
    helper cases
  ;;

  let rec eval_expr env = function
    | Pexp_ident id -> find_exn env id
    | Pexp_constant const -> eval_const const
    | Pexp_apply (Pexp_ident "print_int", [ e1 ]) ->
      let* value = eval_expr env e1 in
      (match value with
       | Val_integer i ->
         (* There is must no be newline, but without that manytests work poorly *)
         Format.printf "%d\n" i;
         return (Val_construct ("()", None))
       | _ -> fail Type_error)
    | Pexp_apply (Pexp_ident "+", [ e1; e2 ]) ->
      let* first = eval_expr env e1 in
      let* second = eval_expr env e2 in
      (match first, second with
       | Val_integer f, Val_integer s -> return (Val_integer (f + s))
       | _ -> fail Type_error)
    | Pexp_apply (Pexp_ident "-", [ e1; e2 ]) ->
      let* first = eval_expr env e1 in
      let* second = eval_expr env e2 in
      (match first, second with
       | Val_integer f, Val_integer s -> return (Val_integer (f - s))
       | _ -> fail Type_error)
    | Pexp_apply (Pexp_ident "*", [ e1; e2 ]) ->
      let* first = eval_expr env e1 in
      let* second = eval_expr env e2 in
      (match first, second with
       | Val_integer f, Val_integer s -> return (Val_integer (f * s))
       | _ -> fail Type_error)
    | Pexp_apply (Pexp_ident "/", [ e1; e2 ]) ->
      let* first = eval_expr env e1 in
      let* second = eval_expr env e2 in
      (match first, second with
       | Val_integer f, Val_integer s -> return (Val_integer (f / s))
       | _ -> fail Type_error)
    | Pexp_apply (Pexp_ident "<=", [ e1; e2 ]) ->
      let* first = eval_expr env e1 in
      let* second = eval_expr env e2 in
      (match first, second with
       | Val_integer f, Val_integer s -> return (Val_boolean (f <= s))
       | _ -> fail Type_error)
    | Pexp_apply (Pexp_ident "<", [ e1; e2 ]) ->
      let* first = eval_expr env e1 in
      let* second = eval_expr env e2 in
      (match first, second with
       | Val_integer f, Val_integer s -> return (Val_boolean (f < s))
       | _ -> fail Type_error)
    | Pexp_apply (Pexp_ident "=", [ e1; e2 ]) ->
      let* first = eval_expr env e1 in
      let* second = eval_expr env e2 in
      (match first, second with
       | Val_integer f, Val_integer s -> return (Val_boolean (f = s))
       | Val_boolean f, Val_boolean s -> return (Val_boolean (f = s))
       | Val_string f, Val_string s -> return (Val_boolean (f = s))
       | _ -> fail Type_error)
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
            | Val_function (cases, fun_env) ->
              let rec helper cases =
                match cases with
                | [] -> fail Match_error
                | case :: tl ->
                  (let* env = match_pattern env (case.pc_lhs, value1) in
                   eval_expr env case.pc_rhs)
                  <|> helper tl
              in
              helper cases
            | _ -> fail Type_error
          in
          helper result es
        | [] -> return value0
      in
      let* value0 = eval_expr env e0 in
      helper value0 es
    | Pexp_let (NonRecursive, vbs, expr) ->
      eval_non_rec_let eval_expr env (NonRecursive, vbs, expr)
    | Pexp_let (Recursive, vbs, expr) -> eval_rec_let eval_expr env (Recursive, vbs, expr)
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
      Val_tuple (List.rev values)
    | Pexp_construct ("Some", Some expr) ->
      let+ value = eval_expr env expr in
      Val_construct ("Some", Some value)
    | Pexp_construct ("Some", None) -> fail Type_error
    | Pexp_construct ("None", None) -> return (Val_construct ("None", None))
    | Pexp_construct ("None", Some _) -> fail Type_error
    | Pexp_construct (name, expr) -> fail Type_error
    | Pexp_constraint (expr, _) -> eval_expr env expr
    | Pexp_match (expr, cases) ->
      let* value_match = eval_expr env expr in
      let rec helper cases =
        match cases with
        | [] -> fail Match_error
        | case :: tl ->
          (let* env = match_pattern env (case.pc_lhs, value_match) in
           eval_expr env case.pc_rhs)
          <|> helper tl
      in
      helper cases
    | Pexp_function cases -> return (Val_function (cases, env))
  ;;

  let eval_structure env = function
    | Pstr_eval expr ->
      let* value = eval_expr env expr in
      return env
    | Pstr_value (NonRecursive, vbs) ->
      let* homka_env =
        Base.List.fold_left vbs ~init:(return env) ~f:(fun env vb ->
          let* env = env in
          let* homka_expr = eval_expr env vb.pvb_expr in
          match_pattern env (vb.pvb_pat, homka_expr))
      in
      return homka_env
    | Pstr_value (Recursive, vbs) ->
      let* homka_env =
        Base.List.fold_left vbs ~init:(return env) ~f:(fun env vb ->
          let* env = env in
          let* homka_expr = eval_expr env vb.pvb_expr in
          let* homka_expr =
            match vb.pvb_pat with
            | Ppat_var name ->
              (match homka_expr with
               | Val_fun _ as v -> return (Val_rec_fun (name, v))
               | v -> return v)
            | _ -> fail Type_error
          in
          match_pattern env (vb.pvb_pat, homka_expr))
      in
      return homka_env
  ;;

  let eval_program env program =
    let rec helper env = function
      | hd :: tl ->
        let* env = eval_structure env hd in
        helper env tl
      | [] -> return env
    in
    let* res = helper env program in
    return res
  ;;
end

let empty_env = EvalEnv.empty
let interpret = Inter.eval_program
let run_interpret = interpret empty_env

let run_interpret_exn str =
  let res = interpret empty_env str |> Res.run in
  match res with
  | Ok v -> Ok v
  | Error e -> Error (Format.asprintf "%a" pp_error e)
;;
