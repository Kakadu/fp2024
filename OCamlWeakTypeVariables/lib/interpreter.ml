(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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
    | Val_builtin of string

  val pp : Format.formatter -> value -> unit
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
    | Val_builtin of string

  let rec pp ppf =
    let open Stdlib.Format in
    function
    | Val_integer int -> fprintf ppf "%i" int
    | Val_boolean bool -> fprintf ppf "'%b'" bool
    | Val_string str -> fprintf ppf "%S" str
    | Val_tuple vls ->
      fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp) vls
    | Val_fun _ | Val_rec_fun _ | Val_builtin _ -> fprintf ppf "<fun>"
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

let%expect_test "pp value" =
  Format.printf
    "Value: %a"
    Value.pp
    (Val_tuple
       [ Val_string "Homka"
       ; Val_integer 122
       ; Val_boolean true
       ; Val_fun (Ppat_var "x", Pexp_ident "homka", EvalEnv.empty)
       ; Val_rec_fun ("damir", Val_fun (Ppat_var "x", Pexp_ident "homka", EvalEnv.empty))
       ; Val_construct ("Some", Some (Val_integer 52))
       ; Val_construct ("[]", None)
       ; Val_builtin "print_int"
       ]);
  [%expect {| Value: ("Homka", 122, 'true', <fun>, <fun>, Some 52, [], <fun>) |}]
;;

let%expect_test "pp error" =
  Format.printf
    "Errors:\n %a\n %a\n %a\n %a\n %a\n"
    pp_error
    Type_error
    pp_error
    (Pattern_error Ppat_any)
    pp_error
    (Eval_expr_error (Pexp_ident "damir"))
    pp_error
    (No_variable "homka")
    pp_error
    Match_error;
  [%expect
    {|
    Errors:
     Type error
     Error while interpret pattern (Ppat_any)
     Error while interpret expression ((
    Pexp_ident "damir"))
     No variable with name homka
     Match failure |}]
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
    | Ppat_construct (pat_name, Some constr), Val_construct (val_name, Some value) ->
      if pat_name <> val_name then fail Type_error else match_pattern env (constr, value)
    | o, _ -> fail (Pattern_error o)
  ;;

  let eval_const = function
    | Pconst_int c -> return (Val_integer c)
    | Pconst_string c -> return (Val_string c)
    | Pconst_boolean c -> return (Val_boolean c)
  ;;

  let eval_non_rec_vbs eval_expr env vbs =
    let+ homka_env =
      Base.List.fold_left vbs ~init:(return env) ~f:(fun env vb ->
        let* env = env in
        let* homka_expr = eval_expr env vb.pvb_expr in
        match_pattern env (vb.pvb_pat, homka_expr))
    in
    homka_env
  ;;

  let eval_rec_vbs eval_expr env vbs =
    let eval_vb env vb =
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
      match_pattern env (vb.pvb_pat, homka_expr)
    in
    let* homka_env = Base.List.fold_left vbs ~init:(return env) ~f:eval_vb in
    let* homka_env = Base.List.fold_left vbs ~init:(return homka_env) ~f:eval_vb in
    return homka_env
  ;;

  let eval_cases eval_expr env cases init_value =
    let rec helper = function
      | [] -> fail Match_error
      | case :: tl ->
        (let* env = match_pattern env (case.pc_lhs, init_value) in
         let* res = eval_expr env case.pc_rhs in
         return res)
        <|> helper tl
    in
    helper cases
  ;;

  let binary_operators_int_arith = [ "+", ( + ); "-", ( - ); "*", ( * ); "/", ( / ) ]

  let binary_operators_compare =
    [ "<=", ( <= ); "<", ( < ); ">", ( > ); ">=", ( >= ); "=", ( = ); "<>", ( <> ) ]
  ;;

  let is_binary_op_int name =
    List.exists (fun (list_op, _) -> name = list_op) binary_operators_int_arith
  ;;

  let is_binary_op_compare name =
    List.exists (fun (list_op, _) -> name = list_op) binary_operators_compare
  ;;

  let eval_binary_op_int eval_expr env op_name e1 e2 =
    let _, op =
      List.find (fun (list_op, _) -> list_op = op_name) binary_operators_int_arith
    in
    let* first = eval_expr env e1 in
    let* second = eval_expr env e2 in
    match first, second with
    | Val_integer f, Val_integer s -> return (Val_integer (op f s))
    | _ -> fail Type_error
  ;;

  let eval_binary_op_compare eval_expr env op_name e1 e2 =
    let get_op () =
      snd (List.find (fun (list_op, _) -> list_op = op_name) binary_operators_compare)
    in
    let* first = eval_expr env e1 in
    let* second = eval_expr env e2 in
    match first, second with
    | Val_integer f, Val_integer s -> return (Val_boolean ((get_op ()) f s))
    | Val_boolean f, Val_boolean s -> return (Val_boolean ((get_op ()) f s))
    | Val_string f, Val_string s -> return (Val_boolean ((get_op ()) f s))
    | _ -> fail Type_error
  ;;

  let rec eval_expr env = function
    | Pexp_ident id -> find_exn env id
    | Pexp_constant const -> eval_const const
    | Pexp_apply (Pexp_ident op_name, [ e1; e2 ]) when is_binary_op_int op_name ->
      eval_binary_op_int eval_expr env op_name e1 e2
    | Pexp_apply (Pexp_ident op_name, [ e1; e2 ]) when is_binary_op_compare op_name ->
      eval_binary_op_compare eval_expr env op_name e1 e2
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
            | Val_function (cases, fun_env) -> eval_cases eval_expr fun_env cases value1
            | Val_builtin "print_int" ->
              (match value1 with
               | Val_integer i ->
                 (* There is must no be newline, but without that manytests work poorly *)
                 Format.printf "%d\n" i;
                 return (Val_construct ("()", None))
               | _ -> fail Type_error)
            | _ -> fail Type_error
          in
          helper result es
        | [] -> return value0
      in
      let* value0 = eval_expr env e0 in
      helper value0 es
    | Pexp_let (NonRecursive, vbs, expr) ->
      let* homka_env = eval_non_rec_vbs eval_expr env vbs in
      eval_expr homka_env expr
    | Pexp_let (Recursive, vbs, expr) ->
      let* homka_env = eval_rec_vbs eval_expr env vbs in
      eval_expr homka_env expr
    | Pexp_ifthenelse (e0, e1, None) ->
      let* value_e0 = eval_expr env e0 in
      (match value_e0 with
       | Val_boolean true ->
         let* value_e1 = eval_expr env e1 in
         (* Without else branch return type must be unit *)
         (match value_e1 with
          | Val_construct ("()", None) as v -> return v
          | _ -> fail Type_error)
       | Val_boolean false -> return (Val_construct ("()", None))
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
    | Pexp_construct (name, None) -> return (Val_construct (name, None))
    | Pexp_construct (name, Some expr) ->
      let+ value = eval_expr env expr in
      Val_construct (name, Some value)
    | Pexp_constraint (expr, _) -> eval_expr env expr
    | Pexp_match (expr, cases) ->
      let* value_match = eval_expr env expr in
      eval_cases eval_expr env cases value_match
    | Pexp_function cases -> return (Val_function (cases, env))
  ;;

  let eval_structure env = function
    | Pstr_eval expr ->
      let* _ = eval_expr env expr in
      return env
    | Pstr_value (NonRecursive, vbs) -> eval_non_rec_vbs eval_expr env vbs
    | Pstr_value (Recursive, vbs) -> eval_rec_vbs eval_expr env vbs
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

let initial_env =
  let empty = EvalEnv.empty in
  EvalEnv.extend empty "print_int" (Val_builtin "print_int")
;;

let interpret = Inter.eval_program
let run_interpret = interpret initial_env

let run_interpret_exn str =
  let res = interpret initial_env str |> Res.run in
  match res with
  | Ok v -> Ok v
  | Error e -> Error (Format.asprintf "%a" pp_error e)
;;
