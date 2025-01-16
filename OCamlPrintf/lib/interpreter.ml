(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  [ `Not_implemented
  | `Type_error
  | `Division_by_zero
  | `No_variable of string
  ]

let pp_error ppf : error -> _ = function
  | `Type_error -> Format.fprintf ppf "Type error"
  | `Division_by_zero -> Format.fprintf ppf "Division by zero"
  | `No_variable id -> Format.fprintf ppf "Undefined variable '%s'" id
  | `Not_implemented -> Format.fprintf ppf "In progress..."
;;

type value =
  | Val_integer of int
  | Val_char of char
  | Val_string of string
  | Val_tuple of value list
  | Val_construct of ident * value option
  | Val_fun of string option * pattern list * Expression.t * env

and env = (string, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | Val_integer int -> fprintf ppf "%i" int
  | Val_char char -> fprintf ppf "'%c'" char
  | Val_string str -> fprintf ppf "%S" str
  | Val_tuple val_list ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
      val_list
  | Val_fun _ -> fprintf ppf "<fun>"
  | Val_construct (tag, None) -> fprintf ppf "%s" tag
  | Val_construct ("Some", Some value) -> fprintf ppf "Some %a" pp_value value
  | _ -> fprintf ppf "<construct>"
;;

module Res = struct
  open Base

  type 'a t = ('a, error) Result.t

  let fail = Result.fail
  let return = Result.return

  let ( >>= ) x f =
    match x with
    | Error x -> Error x
    | Ok a -> f a
  ;;

  let ( let* ) = ( >>= )
end

module EvalEnv = struct
  open Base

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let find_exn env key =
    match Map.find env key with
    | Some value -> Res.return value
    | None -> Res.fail (`No_variable key)
  ;;

  let update_exn env key ~f =
    let value = find_exn env key in
    extend env key (f value)
  ;;
end

module Inter = struct
  open Ast.Expression
  open Res
  open EvalEnv

  let eval_arith op val1 val2 = return (Val_integer (op val1 val2))
  let eval_eq op val1 val2 = return (Val_construct (Bool.to_string (op val1 val2), None))

  let eval_bool op val1 val2 =
    return
      (Val_construct
         (Bool.to_string (op (bool_of_string val1) (bool_of_string val2)), None))
  ;;

  let eval_bin_op = function
    | "*", Val_integer val1, Val_integer val2 -> eval_arith ( * ) val1 val2
    | "/", Val_integer val1, Val_integer val2 when val2 <> 0 -> eval_arith ( / ) val1 val2
    | "/", _, Val_integer 0 -> fail `Division_by_zero
    | "+", Val_integer val1, Val_integer val2 -> eval_arith ( + ) val1 val2
    | "-", Val_integer val1, Val_integer val2 -> eval_arith ( - ) val1 val2
    | ">=", val1, val2 -> eval_eq ( >= ) val1 val2
    | "<=", val1, val2 -> eval_eq ( <= ) val1 val2
    | "<>", val1, val2 -> eval_eq ( <> ) val1 val2
    | "=", val1, val2 -> eval_eq ( = ) val1 val2
    | ">", val1, val2 -> eval_eq ( > ) val1 val2
    | "<", val1, val2 -> eval_eq ( < ) val1 val2
    | "&&", Val_construct (val1, None), Val_construct (val2, None) ->
      eval_bool ( && ) val1 val2
    | "||", Val_construct (val1, None), Val_construct (val2, None) ->
      eval_bool ( || ) val1 val2
    | _ -> fail `Type_error
  ;;

  let rec extract_names_from_pat id_list_acc val_list_acc = function
    | Pat_any, _ -> return (id_list_acc, val_list_acc)
    | Pat_var id, value -> return (id :: id_list_acc, value :: val_list_acc)
    | Pat_tuple (fst, snd, pat_list), Val_tuple val_list ->
      (match
         Base.List.fold2
           (fst :: snd :: pat_list)
           val_list
           ~init:(return (id_list_acc, val_list_acc))
           ~f:(fun acc pat value ->
             let* id_list, value_list = acc in
             extract_names_from_pat id_list value_list (pat, value))
       with
       | Ok acc -> acc
       | _ -> fail `Type_error)
    (* | Pat_construct ("::", Some exp), value -> *)
    | (Pat_construct ("Some", Some pat) | Pat_constraint (pat, _)), value ->
      extract_names_from_pat id_list_acc val_list_acc (pat, value)
    | _ -> fail `Type_error
  ;;

  let rec eval_expression env = function
    | Exp_ident id -> find_exn env id
    | Exp_constant const ->
      (match const with
       | Const_integer int -> return (Val_integer int)
       | Const_char char -> return (Val_char char)
       | Const_string str -> return (Val_string str))
    | Exp_let (Nonrecursive, value_binding, value_binding_list, exp) ->
      let* env = eval_value_binding_list env (value_binding :: value_binding_list) in
      eval_expression env exp
    | Exp_apply (Exp_ident opr, Exp_apply (exp1, exp2)) when is_operator opr ->
      let* value1 = eval_expression env exp1 in
      let* value2 = eval_expression env exp2 in
      eval_bin_op (opr, value1, value2)
    | Exp_fun (pat, pat_list, e) -> return (Val_fun (None, pat :: pat_list, e, env))
    | Exp_apply (exp1, exp2) ->
      (match exp1 with
       | Exp_ident opr when is_negative_op opr ->
         let* value = eval_expression env exp2 in
         (match value with
          | Val_integer v -> return (Val_integer (-v))
          | _ -> fail `Type_error)
       | _ ->
         let* fun_val = eval_expression env exp1 in
         let* arg_val = eval_expression env exp2 in
         fail `Not_implemented)
    | Exp_tuple (fst, snd, rest_list) ->
      let* values =
        Base.List.fold_right
          ~f:(fun exp acc ->
            let* acc = acc in
            let* value = eval_expression env exp in
            return (value :: acc))
          ~init:(return [])
          (fst :: snd :: rest_list)
      in
      return (Val_tuple values)
    | Exp_construct (id, None) when id = "true" || id = "false" ->
      return (Val_construct (id, None))
    | Exp_construct ("()", None) -> return (Val_construct ("()", None))
    | Exp_construct ("Some", Some pat) ->
      let* value = eval_expression env pat in
      return (Val_construct ("Some", Some value))
    | Exp_ifthenelse (if_exp, then_exp, else_exp) ->
      let* if_value = eval_expression env if_exp in
      (match if_value with
       | Val_construct ("true", None) -> eval_expression env then_exp
       | Val_construct ("false", None) ->
         Base.Option.value_map
           else_exp
           ~f:(eval_expression env)
           ~default:(return (Val_construct ("()", None)))
       | _ -> fail `Type_error)
    | _ -> fail `Not_implemented

  and eval_value_binding_list env let_binds =
    let* env, names, values =
      Base.List.fold_left
        ~f:(fun acc { pat; exp } ->
          let* env, names, values = acc in
          let* value = eval_expression env exp in
          match pat with
          | Pat_var name | Pat_constraint (Pat_var name, _) ->
            let value =
              match value with
              | Val_fun (None, pat_list, exp, env) ->
                Val_fun (Some name, pat_list, exp, env)
              | other -> other
            in
            return (env, name :: names, value :: values)
          | _ ->
            let* names, value_list = extract_names_from_pat names [] (pat, value) in
            return (env, names, value_list))
        ~init:(return (env, [], []))
        let_binds
    in
    let rec extend_many env names values =
      match names, values with
      | [], _ | _, [] -> return env
      | name :: rest_names, value :: rest_values ->
        let env = extend env name value in
        extend_many env rest_names rest_values
    in
    extend_many env names values
  ;;

  let eval_structure_item env = function
    | Struct_eval exp ->
      let* val_exp = eval_expression env exp in
      return (env, Some val_exp)
    | Struct_value (Nonrecursive, value_binding, value_binding_list) ->
      let* env = eval_value_binding_list env (value_binding :: value_binding_list) in
      return (env, None)
    | _ -> fail `Not_implemented
  ;;

  let eval_structure =
    Base.List.fold_left
      ~f:(fun acc item ->
        let* env, val_list = acc in
        let* env, val_opt = eval_structure_item env item in
        match val_opt with
        | Some val_exp -> return (env, val_list @ [ val_exp ])
        | None -> return (env, val_list))
      ~init:(return (empty, []))
  ;;
end
