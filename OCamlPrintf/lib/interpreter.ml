(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  [ `Type_error
  | `Division_by_zero
  | `Match_failure
  | `No_variable of string
  ]

let pp_error ppf : error -> _ = function
  | `Type_error -> Format.fprintf ppf "Type error"
  | `Division_by_zero -> Format.fprintf ppf "Division by zero"
  | `Match_failure -> Format.fprintf ppf "Matching failure"
  | `No_variable id -> Format.fprintf ppf "Undefined variable '%s'" id
;;

type value =
  | Val_integer of int
  | Val_char of char
  | Val_string of string
  | Val_fun of rec_flag * pattern * pattern list * Expression.t * env
  | Val_function of Expression.t case list * env
  | Val_tuple of value * value * value list
  | Val_construct of ident * value option
  | Val_builtin of string

and env = (string, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | Val_integer int -> fprintf ppf "%i" int
  | Val_char char -> fprintf ppf "'%c'" char
  | Val_string str -> fprintf ppf "%S" str
  | Val_tuple (fst_val, snd_val, val_list) ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
      (fst_val :: snd_val :: val_list)
  | Val_fun _ -> fprintf ppf "<fun>"
  | Val_function _ -> fprintf ppf "<function>"
  | Val_construct ("::", Some (Val_tuple (head, tail, []))) ->
    fprintf ppf "@[<hv>[ %a" pp_value head;
    let rec pp_tail = function
      | Val_construct (_, None) -> fprintf ppf "@ ]@]"
      | Val_construct (_, Some (Val_tuple (next_head, next_tail, []))) ->
        fprintf ppf "@,; %a" pp_value next_head;
        pp_tail next_tail
      | Val_construct (_, Some _) -> ()
      | value -> fprintf ppf ";@ %a@ ]@]" pp_value value
    in
    pp_tail tail
  | Val_construct (tag, None) -> fprintf ppf "%s" tag
  | Val_construct ("Some", Some value) -> fprintf ppf "Some %a" pp_value value
  | Val_construct _ -> ()
  | Val_builtin _ -> fprintf ppf "<builtin>"
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

  let ( let* ) = ( >>= )
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
    | None -> Res.fail (`No_variable key)
  ;;

  let find_exn1 env key =
    let val' = Map.find_exn env key in
    val'
  ;;
end

module Inter = struct
  open Ast.Expression
  open Res
  open EvalEnv

  let eval_arith opr val1 val2 = return (Val_integer (opr val1 val2))
  let eval_concat opr val1 val2 = return (Val_string (opr val1 val2))

  let eval_eq opr val1 val2 =
    return (Val_construct (Bool.to_string (opr val1 val2), None))
  ;;

  let eval_bool opr val1 val2 =
    return
      (Val_construct
         (Bool.to_string (opr (bool_of_string val1) (bool_of_string val2)), None))
  ;;

  let eval_bin_op = function
    | "*", Val_integer val1, Val_integer val2 -> eval_arith ( * ) val1 val2
    | "/", Val_integer val1, Val_integer val2 when val2 <> 0 -> eval_arith ( / ) val1 val2
    | "/", _, Val_integer 0 -> fail `Division_by_zero
    | "+", Val_integer val1, Val_integer val2 -> eval_arith ( + ) val1 val2
    | "-", Val_integer val1, Val_integer val2 -> eval_arith ( - ) val1 val2
    | "^", Val_string val1, Val_string val2 -> eval_concat ( ^ ) val1 val2
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

  let rec match_pattern env = function
    | Pat_any, _ -> Some env
    | Pat_var name, value -> Some (extend env name value)
    | Pat_constant (Const_integer pat), Val_integer value when pat = value -> Some env
    | Pat_constant (Const_char pat), Val_char value when pat = value -> Some env
    | Pat_constant (Const_string pat), Val_string value when pat = value -> Some env
    | Pat_tuple (fst_pat, snd_pat, pat_list), Val_tuple (fst_val, snd_val, val_list) ->
      let env =
        Base.List.fold2
          ~f:(fun env pat value ->
            match env with
            | Some env -> match_pattern env (pat, value)
            | None -> None)
          ~init:(Some env)
          (fst_pat :: snd_pat :: pat_list)
          (fst_val :: snd_val :: val_list)
      in
      (match env with
       | Ok env -> env
       | _ -> None)
    | ( Pat_construct ("::", Some (Pat_tuple (head_pat, tail_pat, [])))
      , Val_construct ("::", Some (Val_tuple (head_val, tail_val, []))) ) ->
      let env = match_pattern env (head_pat, head_val) in
      (match env with
       | Some env -> match_pattern env (tail_pat, tail_val)
       | None -> None)
    | Pat_construct (id_pat, None), Val_construct (id_val, None) when id_pat = id_val ->
      Some env
    | Pat_construct ("Some", Some pat), Val_construct ("Some", Some value) ->
      match_pattern env (pat, value)
    | Pat_constraint (pat, _), value -> match_pattern env (pat, value)
    | _ -> None
  ;;

  let rec extend_names_from_pat env = function
    | (Pat_any | Pat_construct ("()", None)), _ -> return env
    | Pat_var id, value -> return (extend env id value)
    | Pat_tuple (fst_pat, snd_pat, pat_list), Val_tuple (fst_val, snd_val, val_list) ->
      (match
         Base.List.fold2
           (fst_pat :: snd_pat :: pat_list)
           (fst_val :: snd_val :: val_list)
           ~init:(return env)
           ~f:(fun acc pat value ->
             let* env = acc in
             extend_names_from_pat env (pat, value))
       with
       | Ok acc -> acc
       | _ -> fail `Type_error)
    | Pat_construct ("[]", None), Val_construct ("[]", None) -> return env
    | ( Pat_construct ("::", Some (Pat_tuple (head_pat, tail_pat, [])))
      , Val_construct ("::", Some (Val_tuple (head_val, tail_val, []))) ) ->
      let* env = extend_names_from_pat env (head_pat, head_val) in
      let* env = extend_names_from_pat env (tail_pat, tail_val) in
      return env
    | (Pat_construct ("Some", Some pat) | Pat_constraint (pat, _)), value ->
      extend_names_from_pat env (pat, value)
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
    | Exp_let (Recursive, value_binding, value_binding_list, exp) ->
      let* env = eval_rec_value_binding_list env (value_binding :: value_binding_list) in
      eval_expression env exp
    | Exp_fun (pat, pat_list, exp) ->
      return (Val_fun (Nonrecursive, pat, pat_list, exp, env))
    | Exp_apply (Exp_ident opr, Exp_apply (exp1, exp2)) when is_operator opr ->
      let* value1 = eval_expression env exp1 in
      let* value2 = eval_expression env exp2 in
      eval_bin_op (opr, value1, value2)
    | Exp_apply (exp1, exp2) ->
      (match exp1 with
       | Exp_ident opr when is_negative_op opr ->
         let* value = eval_expression env exp2 in
         (match value with
          | Val_integer value -> return (Val_integer (-value))
          | _ -> fail `Type_error)
       | _ ->
         let* fun_val = eval_expression env exp1 in
         let* arg_val = eval_expression env exp2 in
         (match fun_val with
          | Val_fun (rec_flag, pat, pat_list, exp, fun_env) ->
            let* new_env =
              match rec_flag, match_pattern fun_env (pat, arg_val) with
              | Recursive, Some extended_env -> return (compose env extended_env)
              | Nonrecursive, Some extended_env -> return extended_env
              | _, None -> fail `Match_failure
            in
            (match pat_list with
             | [] -> eval_expression new_env exp
             | first_pat :: rest_pat_list ->
               return (Val_fun (Recursive, first_pat, rest_pat_list, exp, new_env)))
          | Val_function (case_list, env) -> find_and_eval_case env arg_val case_list
          | Val_builtin builtin ->
            (match builtin, arg_val with
             | "print_int", Val_integer integer ->
               print_int integer;
               return (Val_construct ("()", None))
             | "print_endline", Val_string str ->
               print_endline str;
               return (Val_construct ("()", None))
             | _ -> fail `Type_error)
          | _ -> fail `Type_error))
    | Exp_function (case, case_list) -> return (Val_function (case :: case_list, env))
    | Exp_match (exp, case, case_list) ->
      let* match_value = eval_expression env exp in
      find_and_eval_case env match_value (case :: case_list)
    | Exp_tuple (fst_exp, snd_exp, exp_list) ->
      let* fst_val = eval_expression env fst_exp in
      let* snd_val = eval_expression env snd_exp in
      let* val_list =
        Base.List.fold_right
          ~f:(fun exp acc ->
            let* acc = acc in
            let* value = eval_expression env exp in
            return (value :: acc))
          ~init:(return [])
          exp_list
      in
      return (Val_tuple (fst_val, snd_val, val_list))
    | Exp_construct ("::", Some (Exp_tuple (head, tail, []))) ->
      let* val1 = eval_expression env head in
      let* val2 = eval_expression env tail in
      return (Val_construct ("::", Some (Val_tuple (val1, val2, []))))
    | Exp_construct (id, None) -> return (Val_construct (id, None))
    | Exp_construct ("Some", Some pat) ->
      let* value = eval_expression env pat in
      return (Val_construct ("Some", Some value))
    | Exp_construct _ -> fail `Type_error
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
    | Exp_sequence (exp1, exp2) ->
      let* _ = eval_expression env exp1 in
      let* value = eval_expression env exp2 in
      return value
    | Exp_constraint (exp, _) -> eval_expression env exp

  and find_and_eval_case env value = function
    | [] -> fail `Match_failure
    | { left; right } :: tail ->
      let env_temp = match_pattern env (left, value) in
      (match env_temp with
       | Some env -> eval_expression env right
       | None -> find_and_eval_case env value tail)

  and eval_value_binding_list env value_binding_list =
    Base.List.fold_left
      ~f:(fun acc { pat; exp } ->
        let* env = acc in
        let* value = eval_expression env exp in
        match pat with
        | Pat_var name | Pat_constraint (Pat_var name, _) ->
          let env = extend env name value in
          return env
        | _ ->
          let* env = extend_names_from_pat env (pat, value) in
          return env)
      ~init:(return env)
      value_binding_list

  and eval_rec_value_binding_list env value_binding_list =
    Base.List.fold_left
      ~f:(fun acc { pat; exp } ->
        let* env = acc in
        let* value = eval_expression env exp in
        match pat with
        | Pat_var name | Pat_constraint (Pat_var name, _) ->
          let value =
            match value with
            | Val_fun (_, pat, pat_list, exp, env) ->
              Val_fun (Recursive, pat, pat_list, exp, env)
            | other -> other
          in
          let env = extend env name value in
          return env
        | _ -> fail `Type_error)
      ~init:(return env)
      value_binding_list
  ;;

  let eval_structure_item env out_list =
    let rec extract_names_from_pat env acc = function
      | Pat_var id -> acc @ [ Some id, EvalEnv.find_exn1 env id ]
      | Pat_tuple (fst_pat, snd_pat, pat_list) ->
        Base.List.fold_left
          (fst_pat :: snd_pat :: pat_list)
          ~init:acc
          ~f:(extract_names_from_pat env)
      | Pat_construct ("::", Some exp) ->
        (match exp with
         | Pat_tuple (head, tail, []) ->
           let acc = extract_names_from_pat env acc head in
           extract_names_from_pat env acc tail
         | _ -> acc)
      | Pat_construct ("Some", Some pat) -> extract_names_from_pat env acc pat
      | Pat_constraint (pat, _) -> extract_names_from_pat env acc pat
      | _ -> acc
    in
    let get_names_from_let_binds env =
      Base.List.fold_left ~init:[] ~f:(fun acc { pat; _ } ->
        extract_names_from_pat env acc pat)
    in
    function
    | Struct_eval exp ->
      let* val' = eval_expression env exp in
      return (env, out_list @ [ None, val' ])
    | Struct_value (Nonrecursive, value_binding, value_binding_list) ->
      let value_binding_list = value_binding :: value_binding_list in
      let* env = eval_value_binding_list env value_binding_list in
      let eval_list = get_names_from_let_binds env value_binding_list in
      return (env, out_list @ eval_list)
    | Struct_value (Recursive, value_binding, value_binding_list) ->
      let value_binding_list = value_binding :: value_binding_list in
      let* env = eval_rec_value_binding_list env value_binding_list in
      let eval_list = get_names_from_let_binds env value_binding_list in
      return (env, out_list @ eval_list)
  ;;

  let eval_structure env ast =
    let* env, out_list =
      Base.List.fold_left
        ~f:(fun acc item ->
          let* env, out_list = acc in
          let* env, out_list = eval_structure_item env out_list item in
          return (env, out_list))
        ~init:(return (env, []))
        ast
    in
    let remove_duplicates =
      let fun_equal el1 el2 =
        match el1, el2 with
        | (Some id1, _), (Some id2, _) -> String.equal id1 id2
        | _ -> false
      in
      function
      | x :: xs when not (Base.List.mem xs x ~equal:fun_equal) -> x :: xs
      | _ :: xs -> xs
      | [] -> []
    in
    return (env, remove_duplicates out_list)
  ;;
end

let empty_env = EvalEnv.empty

let env_with_print_funs =
  let env = EvalEnv.extend empty_env "print_int" (Val_builtin "print_int") in
  EvalEnv.extend env "print_endline" (Val_builtin "print_endline")
;;

let run_interpreter = Inter.eval_structure
