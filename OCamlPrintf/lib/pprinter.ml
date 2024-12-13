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

let is_operator_in_exp = function
  | Exp_apply (Exp_ident op, _, _) when is_operator op -> true
  | _ -> false
;;

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

let let_flag_str = function
  | Recursive -> "let rec"
  | Nonrecursive -> "let"
;;

let pp_escape_sequence ppf () = fprintf ppf "\n"
let pp_space ppf () = fprintf ppf " "
let pp_comma ppf () = fprintf ppf ", "
let pp_and ppf () = fprintf ppf " and "
let pp_ident ppf id = fprintf ppf "%s" id

let pp_constant ppf = function
  | Const_integer n -> fprintf ppf "%d" n
  | Const_char c -> fprintf ppf "'%c'" c
  | Const_string s -> fprintf ppf "%S" s
;;

let rec pp_type ppf = function
  | Type_any -> fprintf ppf "_"
  | Type_int -> fprintf ppf "int"
  | Type_char -> fprintf ppf "char"
  | Type_string -> fprintf ppf "string"
  | Type_bool -> fprintf ppf "bool"
  | Type_list type' ->
    (match type' with
     | type' when is_type_arrow type' || is_type_list type' ->
       fprintf ppf "(%a) list" pp_type type'
     | type' -> fprintf ppf "%a list" pp_type type')
  | Type_tuple (first_type, second_type, type_list) ->
    let pp_with_condition_on_arrow type' =
      match type' with
      | type' when is_type_arrow type' -> fprintf ppf "(%a)" pp_type type'
      | _ -> fprintf ppf "%a" pp_type type'
    in
    fprintf ppf "(";
    pp_with_condition_on_arrow first_type;
    fprintf ppf " * ";
    pp_with_condition_on_arrow second_type;
    List.iter
      (fun type' ->
        fprintf ppf " * ";
        pp_with_condition_on_arrow type')
      type_list;
    fprintf ppf ")"
  | Type_arrow (first_type, second_type) ->
    (match first_type with
     | first_type when is_type_arrow first_type ->
       fprintf ppf "(%a) -> %a" pp_type first_type pp_type second_type
     | first_type -> fprintf ppf "%a -> %a" pp_type first_type pp_type second_type)
;;

let rec pp_pattern ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var id -> pp_ident ppf id
  | Pat_constant const -> pp_constant ppf const
  | Pat_tuple (first_pat, second_pat, pat_list) ->
    fprintf ppf "(%a, %a" pp_pattern first_pat pp_pattern second_pat;
    if Base.List.is_empty pat_list
    then fprintf ppf ")"
    else fprintf ppf ", %a)" (pp_print_list ~pp_sep:pp_comma pp_pattern) pat_list
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
    fprintf ppf "(%a : %a)" pp_pattern pat pp_type core_type
;;

let rec pp_expression ppf = function
  | Exp_ident id -> pp_ident ppf id
  | Exp_constant const -> pp_constant ppf const
  | Exp_let (rec_flag, first_value_binding, value_binding_list, exp) ->
    let pp_exp_constraint = function
      | Exp_constraint (exp', core_type) ->
        fprintf ppf ": %a = %a" pp_type core_type pp_expression exp'
      | exp' -> fprintf ppf "= %a" pp_expression exp'
    in
    fprintf ppf "(%s " (let_flag_str rec_flag);
    fprintf
      ppf
      "%a"
      (fun ppf value ->
        fprintf ppf "%a " pp_pattern value.pat;
        pp_exp_constraint value.exp)
      first_value_binding;
    if not (Base.List.is_empty value_binding_list)
    then
      fprintf
        ppf
        " and %a"
        (pp_print_list ~pp_sep:pp_and (fun ppf value ->
           fprintf ppf "%a " pp_pattern value.pat;
           pp_exp_constraint value.exp))
        value_binding_list;
    fprintf ppf " in %a)" pp_expression exp
  | Exp_fun (first_pat, pat_list, exp) ->
    let pp_exp_constraint = function
      | Exp_constraint (exp', core_type) ->
        fprintf ppf ": %a -> %a)" pp_type core_type pp_expression exp'
      | exp' -> fprintf ppf "-> %a)" pp_expression exp'
    in
    fprintf ppf "(fun %a " pp_pattern first_pat;
    if not (Base.List.is_empty pat_list)
    then fprintf ppf "%a " (pp_print_list ~pp_sep:pp_space pp_pattern) pat_list;
    pp_exp_constraint exp
  | Exp_apply (exp, first_exp, exp_list) ->
    let expression = asprintf "%a" pp_expression exp in
    let fprintf_with_parens_condition ppf exp =
      if is_operator_in_exp exp
      then fprintf ppf " (%a)" pp_expression exp
      else fprintf ppf " %a" pp_expression exp
    in
    let handle_exp_list = function
      | exp, [] ->
        fprintf ppf "(%s" expression;
        fprintf_with_parens_condition ppf exp;
        fprintf ppf ")"
      | first_exp, rest_exp_list ->
        let needs_parens_by_priority = function
          | Exp_apply (Exp_ident op, _, _) when is_operator op ->
            compare_priority expression op
          | _ -> false
        in
        if is_operator expression
        then
          List.iter
            (fun exp ->
              if needs_parens_by_priority exp
              then
                if needs_parens_by_priority first_exp
                then
                  fprintf
                    ppf
                    "(%a) %s (%a)"
                    pp_expression
                    first_exp
                    expression
                    pp_expression
                    exp
                else
                  fprintf
                    ppf
                    "%a %s (%a)"
                    pp_expression
                    first_exp
                    expression
                    pp_expression
                    exp
              else if needs_parens_by_priority first_exp
              then
                fprintf
                  ppf
                  "(%a) %s %a"
                  pp_expression
                  first_exp
                  expression
                  pp_expression
                  exp
              else
                fprintf
                  ppf
                  "%a %s %a"
                  pp_expression
                  first_exp
                  expression
                  pp_expression
                  exp)
            rest_exp_list
        else (
          fprintf ppf "(%s" expression;
          fprintf_with_parens_condition ppf first_exp;
          List.iter (fun arg -> fprintf_with_parens_condition ppf arg) rest_exp_list;
          fprintf ppf ")")
    in
    handle_exp_list (first_exp, exp_list)
  | Exp_match (exp, first_case, case_list) ->
    fprintf
      ppf
      "(match %a with | %a -> %a"
      pp_expression
      exp
      pp_pattern
      first_case.left
      pp_expression
      first_case.right;
    if Base.List.is_empty case_list
    then fprintf ppf ")"
    else
      fprintf
        ppf
        " %a)"
        (pp_print_list ~pp_sep:pp_space (fun ppf case ->
           fprintf ppf "| %a -> %a" pp_pattern case.left pp_expression case.right))
        case_list
  | Exp_tuple (first_exp, second_exp, exp_list) ->
    fprintf ppf "(%a, %a" pp_expression first_exp pp_expression second_exp;
    if Base.List.is_empty exp_list
    then fprintf ppf ")"
    else fprintf ppf ", %a)" (pp_print_list ~pp_sep:pp_comma pp_expression) exp_list
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
    fprintf ppf "(%a : %a)" pp_expression exp pp_type core_type
;;

let pp_structure_item ppf = function
  | Struct_eval exp -> fprintf ppf "%a;;" pp_expression exp
  | Struct_value (rec_flag, first_value_binding, value_binding_list) ->
    let pp_exp_constraint = function
      | Exp_constraint (exp', core_type) ->
        fprintf ppf ": %a = %a" pp_type core_type pp_expression exp'
      | exp' -> fprintf ppf "= %a" pp_expression exp'
    in
    fprintf ppf "%s " (let_flag_str rec_flag);
    fprintf
      ppf
      "%a"
      (fun ppf value ->
        fprintf ppf "%a " pp_pattern value.pat;
        pp_exp_constraint value.exp)
      first_value_binding;
    if Base.List.is_empty value_binding_list
    then fprintf ppf ";;"
    else
      fprintf
        ppf
        " and %a;;"
        (pp_print_list ~pp_sep:pp_and (fun ppf value ->
           fprintf ppf "%a " pp_pattern value.pat;
           pp_exp_constraint value.exp))
        value_binding_list
;;

let pp_structure ppf str =
  if Base.List.is_empty str
  then fprintf ppf ";;"
  else fprintf ppf "%a" (pp_print_list ~pp_sep:pp_escape_sequence pp_structure_item) str
;;
