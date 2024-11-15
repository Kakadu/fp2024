(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 **)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

let is_operator = function
  | "*" | "/" | "+" | "-" | ">=" | "<=" | "<>" | "=" | ">" | "<" | "&&" | "||" -> true
  | _ -> false
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

let rec pp_pattern ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var id -> pp_ident ppf id
  | Pat_constant const -> pp_constant ppf const
  | Pat_tuple pat_list ->
    fprintf ppf "(%a)" (pp_print_list ~pp_sep:pp_comma pp_pattern) pat_list
  | Pat_construct (_, None) -> fprintf ppf "[]"
  | Pat_construct (_, Some pat) ->
    (match pat with
     | Pat_tuple [ head; tail ] ->
       fprintf ppf "[%a" pp_pattern head;
       let rec pp_tail = function
         | Pat_construct (_, None) -> fprintf ppf "]"
         | Pat_construct (_, Some pat_tail) ->
           (match pat_tail with
            | Pat_tuple [ next_head; next_tail ] ->
              fprintf ppf "; %a" pp_pattern next_head;
              pp_tail next_tail
            | _ -> ())
         | _ -> ()
       in
       pp_tail tail
     | _ -> ())
;;

let rec pp_expression ppf = function
  | Exp_ident id -> pp_ident ppf id
  | Exp_constant const -> pp_constant ppf const
  | Exp_let (rec_flag, value_binding_list, exp) ->
    fprintf
      ppf
      "(%s %a in %a)"
      (let_flag_str rec_flag)
      (pp_print_list ~pp_sep:pp_and (fun ppf value ->
         fprintf ppf "%a = %a" pp_pattern value.pat pp_expression value.exp))
      value_binding_list
      pp_expression
      exp
  | Exp_fun (pat_list, exp) ->
    fprintf
      ppf
      "(fun %a -> %a)"
      (pp_print_list ~pp_sep:pp_space pp_pattern)
      pat_list
      pp_expression
      exp
  | Exp_apply (exp, exp_list) ->
    let expression = asprintf "%a" pp_expression exp in
    let handle_exp_list = function
      | [ exp ] -> fprintf ppf "(%s %a)" expression pp_expression exp
      | _ ->
        let first_exp = List.hd exp_list in
        let rest_exp = List.tl exp_list in
        let needs_parens = function
          | Exp_apply (Exp_ident op, _) when is_operator op -> true
          | _ -> false
        in
        if is_operator expression
        then (
          fprintf ppf "%a %s" pp_expression first_exp expression;
          List.iter
            (fun arg ->
              if needs_parens arg
              then fprintf ppf " (%a)" pp_expression arg
              else fprintf ppf " %a" pp_expression arg)
            rest_exp)
        else (
          fprintf ppf "(%s %a" expression pp_expression first_exp;
          List.iter (fun arg -> fprintf ppf " %a" pp_expression arg) rest_exp;
          fprintf ppf ")")
    in
    handle_exp_list exp_list
  | Exp_match (exp, case_list) ->
    fprintf
      ppf
      "(match %a with %a)"
      pp_expression
      exp
      (pp_print_list ~pp_sep:pp_space (fun ppf case ->
         fprintf ppf "| %a -> %a" pp_pattern case.left pp_expression case.right))
      case_list
  | Exp_tuple exp_list ->
    fprintf ppf "(%a)" (pp_print_list ~pp_sep:pp_comma pp_expression) exp_list
  | Exp_construct ("::", None) -> fprintf ppf "[]"
  | Exp_construct ("::", Some exp) ->
    (match exp with
     | Exp_tuple [ head; tail ] ->
       fprintf ppf "[%a" pp_expression head;
       let rec print_tail = function
         | Exp_construct (_, None) -> fprintf ppf "]"
         | Exp_construct (_, Some exp_tail) ->
           (match exp_tail with
            | Exp_tuple [ next_head; next_tail ] ->
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
;;

let pp_structure_item ppf = function
  | Struct_eval exp -> fprintf ppf "%a;;" pp_expression exp
  | Struct_value (rec_flag, value_binding_list) ->
    fprintf
      ppf
      "%s %a;;"
      (let_flag_str rec_flag)
      (pp_print_list ~pp_sep:pp_and (fun ppf value ->
         fprintf ppf "%a = %a" pp_pattern value.pat pp_expression value.exp))
      value_binding_list
;;

let pp_structure ppf =
  fprintf ppf "%a" (pp_print_list ~pp_sep:pp_escape_sequence pp_structure_item)
;;
