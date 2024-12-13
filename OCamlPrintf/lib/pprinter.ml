(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 **)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ast.Expression
open Format

let op_list =
  [ "*", 1
  ; "/", 1
  ; "+", 2
  ; "-", 2
  ; ">=", 3
  ; "<=", 3
  ; "<>", 3
  ; "=", 3
  ; ">", 3
  ; "<", 3
  ; "&&", 4
  ; "||", 5
  ]
;;

let is_operator op = List.exists (fun (str, _) -> str = op) op_list
let get_priority op = List.assoc op op_list

let is_type_arrow = function
  | Type_arrow (_, _) -> true
  | _ -> false
;;

let is_type_list = function
  | Type_list _ -> true
  | _ -> false
;;

let compare_priority op1 op2 =
  let priority1 = get_priority op1 in
  let priority2 = get_priority op2 in
  priority1 <= priority2
;;

let pp_rec_flag ppf = function
  | Recursive -> fprintf ppf "let rec "
  | Nonrecursive -> fprintf ppf "let "
;;

let pp_escape_sequence ppf () = fprintf ppf "\n"
let pp_space ppf () = fprintf ppf " "
let pp_comma ppf () = fprintf ppf ", "
let pp_and ppf () = fprintf ppf " and "
let pp_ident ppf = fprintf ppf "%s"

let pp_constant ppf = function
  | Const_integer n -> fprintf ppf "%d" n
  | Const_char c -> fprintf ppf "'%c'" c
  | Const_string s -> fprintf ppf "%S" s
;;

let rec pp_core_type ppf = function
  | Type_any -> fprintf ppf "_"
  | Type_unit -> fprintf ppf "unit"
  | Type_int -> fprintf ppf "int"
  | Type_char -> fprintf ppf "char"
  | Type_string -> fprintf ppf "string"
  | Type_bool -> fprintf ppf "bool"
  (* The id obtained from parser is stored without first char ',
     while the id from inferencer is stored with ', so that there is no confusion when inferring types. *)
  | Type_name id ->
    let pp_type_name = fprintf ppf "'%s" in
    if String.length id > 1
    then (
      match String.get id 0 with
      | '\'' -> pp_ident ppf id
      | _ -> pp_type_name id)
    else pp_type_name id
  | Type_list type' ->
    (match type' with
     | type' when is_type_arrow type' || is_type_list type' ->
       fprintf ppf "(%a) list" pp_core_type type'
     | type' -> fprintf ppf "%a list" pp_core_type type')
  | Type_tuple (first_type, second_type, type_list) ->
    let pp_with_condition_on_arrow type' =
      match type' with
      | type' when is_type_arrow type' -> fprintf ppf "(%a)" pp_core_type type'
      | _ -> fprintf ppf "%a" pp_core_type type'
    in
    fprintf ppf "(";
    pp_with_condition_on_arrow first_type;
    List.iter
      (fun type' ->
        fprintf ppf " * ";
        pp_with_condition_on_arrow type')
      (second_type :: type_list);
    fprintf ppf ")"
  | Type_arrow (first_type, second_type) ->
    (match first_type with
     | first_type when is_type_arrow first_type ->
       fprintf ppf "(%a) -> %a" pp_core_type first_type pp_core_type second_type
     | first_type ->
       fprintf ppf "%a -> %a" pp_core_type first_type pp_core_type second_type)
;;

let rec pp_pattern ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var id -> pp_ident ppf id
  | Pat_constant const -> pp_constant ppf const
  | Pat_tuple (first_pat, second_pat, pat_list) ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:pp_comma pp_pattern)
      (first_pat :: second_pat :: pat_list)
  | Pat_construct ("::", None) -> fprintf ppf "[]"
  | Pat_construct ("::", Some pat) ->
    (match pat with
     | Pat_tuple (head, tail, []) ->
       fprintf ppf "[%a" pp_pattern head;
       let rec pp_tail = function
         | Pat_construct (_, None) -> fprintf ppf "]"
         | Pat_construct (_, Some pat_tail) ->
           (match pat_tail with
            | Pat_tuple (next_head, next_tail, []) ->
              fprintf ppf "; %a" pp_pattern next_head;
              pp_tail next_tail
            | _ -> ())
         | _ -> ()
       in
       pp_tail tail
     | _ -> ())
  | Pat_construct (tag, None) -> fprintf ppf "%s" tag
  | Pat_construct ("Some", Some pat) -> fprintf ppf "Some (%a)" pp_pattern pat
  | Pat_construct (_, _) -> ()
  | Pat_constraint (pat, core_type) ->
    fprintf ppf "(%a : %a)" pp_pattern pat pp_core_type core_type
;;

let rec pp_expression ppf = function
  | Exp_ident id -> pp_ident ppf id
  | Exp_constant const -> pp_constant ppf const
  | Exp_let (rec_flag, first_value_binding, value_binding_list, exp) ->
    fprintf ppf "(";
    pp_rec_flag ppf rec_flag;
    fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:pp_and pp_value_binding)
      (first_value_binding :: value_binding_list);
    fprintf ppf " in %a)" pp_expression exp
  | Exp_fun (first_pat, pat_list, exp) ->
    let pp_exp_constraint = function
      | Exp_constraint (exp', core_type) ->
        fprintf ppf ": %a -> %a)" pp_core_type core_type pp_expression exp'
      | exp' -> fprintf ppf "-> %a)" pp_expression exp'
    in
    fprintf
      ppf
      "(fun %a "
      (pp_print_list ~pp_sep:pp_space pp_pattern)
      (first_pat :: pat_list);
    pp_exp_constraint exp
  | Exp_apply (exp_fn, exp) -> pp_exp_apply ppf (exp_fn, exp)
  | Exp_match (exp, first_case, case_list) ->
    fprintf ppf "(match %a with" pp_expression exp;
    fprintf ppf " %a)" (pp_print_list ~pp_sep:pp_space pp_case) (first_case :: case_list)
  | Exp_tuple (first_exp, second_exp, exp_list) ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:pp_comma pp_expression)
      (first_exp :: second_exp :: exp_list)
  | Exp_construct ("::", None) -> fprintf ppf "[]"
  | Exp_construct ("::", Some exp) ->
    (match exp with
     | Exp_tuple (head, tail, []) ->
       fprintf ppf "[%a" pp_expression head;
       let rec print_tail = function
         | Exp_construct (_, None) -> fprintf ppf "]"
         | Exp_construct (_, Some exp_tail) ->
           (match exp_tail with
            | Exp_tuple (next_head, next_tail, []) ->
              fprintf ppf "; %a" pp_expression next_head;
              print_tail next_tail
            | _ -> ())
         | _ -> ()
       in
       print_tail tail
     | _ -> ())
  | Exp_construct (tag, None) -> fprintf ppf "%s" tag
  | Exp_construct ("Some", Some exp) -> fprintf ppf "Some (%a)" pp_expression exp
  | Exp_construct (_, _) -> ()
  | Exp_ifthenelse (exp1, exp2, None) ->
    fprintf ppf "(if %a then %a)" pp_expression exp1 pp_expression exp2
  | Exp_ifthenelse (exp1, exp2, Some exp3) ->
    fprintf
      ppf
      "(if %a then %a else %a)"
      pp_expression
      exp1
      pp_expression
      exp2
      pp_expression
      exp3
  | Exp_sequence (exp1, exp2) ->
    fprintf ppf "(%a); (%a)" pp_expression exp1 pp_expression exp2
  | Exp_constraint (exp, core_type) ->
    fprintf ppf "(%a : %a)" pp_expression exp pp_core_type core_type

(* TODO: add compare_priority *)
and pp_exp_apply ppf (exp_fn, exp) =
  match exp_fn with
  | Exp_ident exp_opr when is_operator exp_opr ->
    (match exp with
     | Exp_apply (opn, Exp_apply (Exp_ident opr, exp)) when is_operator opr ->
       fprintf ppf "%a %s " pp_expression opn exp_opr;
       (match get_priority exp_opr with
        | 1 ->
          if get_priority exp_opr <= get_priority opr
          then fprintf ppf "(%a)" pp_exp_apply (Exp_ident opr, exp)
          else fprintf ppf "%a" pp_exp_apply (Exp_ident opr, exp)
        | _ -> fprintf ppf "%a" pp_exp_apply (Exp_ident opr, exp))
     | Exp_apply (Exp_apply (Exp_ident opr, exp), opn) when is_operator opr ->
       (match get_priority exp_opr with
        | 2 ->
          if get_priority exp_opr <= get_priority opr
          then fprintf ppf "(%a)" pp_exp_apply (Exp_ident opr, exp)
          else fprintf ppf "%a" pp_exp_apply (Exp_ident opr, exp)
        | _ ->
          fprintf ppf "%a" pp_exp_apply (Exp_ident opr, exp);
          fprintf ppf " %s %a" exp_opr pp_expression opn)
     | Exp_apply (opn1, opn2) ->
       fprintf ppf "%a %s %a" pp_expression opn1 exp_opr pp_expression opn2
     | _ -> pp_expression ppf exp)
  | _ ->
    (match exp with
     | Exp_apply (_, Exp_apply _) ->
       fprintf ppf "%a (%a)" pp_expression exp_fn pp_expression exp
     | Exp_apply (_, _) -> fprintf ppf "%a %a " pp_expression exp_fn pp_expression exp
     | _ -> fprintf ppf "%a %a" pp_expression exp_fn pp_expression exp)

and pp_exp_constraint ppf = function
  | Exp_constraint (exp', core_type) ->
    fprintf ppf ": %a = %a" pp_core_type core_type pp_expression exp'
  | exp' -> fprintf ppf "= %a" pp_expression exp'

and pp_value_binding ppf = function
  | { pat; exp } ->
    fprintf ppf "%a " pp_pattern pat;
    pp_exp_constraint ppf exp

and pp_case ppf = function
  | { left; right } -> fprintf ppf "| %a -> %a" pp_pattern left pp_expression right
;;

let pp_structure_item ppf = function
  | Struct_eval exp -> fprintf ppf "%a;;" pp_expression exp
  | Struct_value (rec_flag, first_value_binding, value_binding_list) ->
    pp_rec_flag ppf rec_flag;
    fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:pp_and pp_value_binding)
      (first_value_binding :: value_binding_list);
    fprintf ppf ";;"
;;

let pp_structure ppf ast =
  if Base.List.is_empty ast
  then fprintf ppf ";;"
  else fprintf ppf "%a" (pp_print_list ~pp_sep:pp_escape_sequence pp_structure_item) ast
;;
