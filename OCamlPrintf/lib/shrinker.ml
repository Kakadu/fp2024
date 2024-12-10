(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ast.Expression
open QCheck.Iter
open QCheck.Shrink

let shrink_string = function
  | "a" -> empty
  | _ -> return "a"
;;

let rec shrink_pattern = function
  | Pat_any -> empty
  | Pat_var id -> shrink_string id >|= fun id' -> Pat_var id'
  | Pat_constant const ->
    (match const with
     | Const_integer i -> int i >|= fun i' -> Pat_constant (Const_integer i')
     | Const_char ch -> char ch >|= fun ch' -> Pat_constant (Const_char ch')
     | Const_string str ->
       shrink_string str >|= fun str' -> Pat_constant (Const_string str'))
  | Pat_tuple (first_pat, second_pat, pat_list) ->
    of_list [ first_pat; second_pat ]
    <+> of_list pat_list
    <+> (shrink_pattern first_pat
         >|= fun first_pat' -> Pat_tuple (first_pat', second_pat, pat_list))
    <+> (shrink_pattern second_pat
         >|= fun second_pat' -> Pat_tuple (first_pat, second_pat', pat_list))
    <+> (list ~shrink:shrink_pattern pat_list
         >|= fun pat_list' -> Pat_tuple (first_pat, second_pat, pat_list'))
  | Pat_construct (_, None) -> empty
  | Pat_construct (id, Some pat) ->
    return pat <+> (shrink_pattern pat >|= fun pat' -> Pat_construct (id, Some pat'))
  | Pat_constraint (pat, core_type) ->
    return pat <+> (shrink_pattern pat >|= fun pat' -> Pat_constraint (pat', core_type))
;;

let rec shrink_expression = function
  | Exp_ident id -> shrink_string id >|= fun id' -> Exp_ident id'
  | Exp_constant const ->
    (match const with
     | Const_integer i -> int i >|= fun i' -> Exp_constant (Const_integer i')
     | Const_char ch -> char ch >|= fun ch' -> Exp_constant (Const_char ch')
     | Const_string str ->
       shrink_string str >|= fun str' -> Exp_constant (Const_string str'))
  | Exp_let (rec_flag, first_value_binding, value_binding_list, exp) ->
    return exp
    <+> (shrink_expression exp
         >|= fun exp' -> Exp_let (rec_flag, first_value_binding, value_binding_list, exp')
        )
    <+> (shrink_value_binding first_value_binding
         >|= fun first_value_binding' ->
         Exp_let (rec_flag, first_value_binding', value_binding_list, exp))
    <+> (list ~shrink:shrink_value_binding value_binding_list
         >|= fun value_binding_list' ->
         Exp_let (rec_flag, first_value_binding, value_binding_list', exp))
    <+> (shrink_expression exp
         >|= fun exp' -> Exp_let (rec_flag, first_value_binding, value_binding_list, exp')
        )
  | Exp_fun (first_pat, pat_list, exp) ->
    return exp
    <+> (shrink_pattern first_pat >|= fun first_pat' -> Exp_fun (first_pat', pat_list, exp)
        )
    <+> (list ~shrink:shrink_pattern pat_list
         >|= fun pat_list' -> Exp_fun (first_pat, pat_list', exp))
    <+> (shrink_expression exp >|= fun exp' -> Exp_fun (first_pat, pat_list, exp'))
  | Exp_apply (exp, first_exp, exp_list) ->
    return first_exp
    <+> of_list exp_list
    <+> (shrink_expression exp >|= fun exp' -> Exp_apply (exp', first_exp, exp_list))
    <+> (shrink_expression first_exp
         >|= fun first_exp' -> Exp_apply (exp, first_exp', exp_list))
    <+> (list ~shrink:shrink_expression exp_list
         >|= fun exp_list' -> Exp_apply (exp, first_exp, exp_list'))
  | Exp_match (exp, first_case, case_list) ->
    return exp
    <+> (shrink_expression exp >|= fun exp' -> Exp_match (exp', first_case, case_list))
    <+> (shrink_case first_case
         >|= fun first_case' -> Exp_match (exp, first_case', case_list))
    <+> (list ~shrink:shrink_case case_list
         >|= fun case_list' -> Exp_match (exp, first_case, case_list'))
  | Exp_tuple (first_exp, second_exp, exp_list) ->
    of_list [ first_exp; second_exp ]
    <+> of_list exp_list
    <+> (shrink_expression first_exp
         >|= fun first_exp' -> Exp_tuple (first_exp', second_exp, exp_list))
    <+> (shrink_expression second_exp
         >|= fun second_exp' -> Exp_tuple (first_exp, second_exp', exp_list))
    <+> (list ~shrink:shrink_expression exp_list
         >|= fun exp_list' -> Exp_tuple (first_exp, second_exp, exp_list'))
  | Exp_construct (_, None) -> empty
  | Exp_construct (id, Some exp) ->
    return exp <+> (shrink_expression exp >|= fun exp' -> Exp_construct (id, Some exp'))
  | Exp_ifthenelse (if_exp, then_exp, None) ->
    of_list [ if_exp; then_exp ]
    <+> (shrink_expression if_exp
         >|= fun if_exp' -> Exp_ifthenelse (if_exp', then_exp, None))
    <+> (shrink_expression then_exp
         >|= fun then_exp' -> Exp_ifthenelse (if_exp, then_exp', None))
  | Exp_ifthenelse (if_exp, then_exp, Some else_exp) ->
    of_list [ if_exp; then_exp; else_exp ]
    <+> (shrink_expression if_exp
         >|= fun if_exp' -> Exp_ifthenelse (if_exp', then_exp, Some else_exp))
    <+> (shrink_expression then_exp
         >|= fun then_exp' -> Exp_ifthenelse (if_exp, then_exp', Some else_exp))
    <+> (shrink_expression else_exp
         >|= fun else_exp' -> Exp_ifthenelse (if_exp, then_exp, Some else_exp'))
  | Exp_sequence (exp1, exp2) ->
    of_list [ exp1; exp2 ]
    <+> (shrink_expression exp1 >|= fun exp1' -> Exp_sequence (exp1', exp2))
    <+> (shrink_expression exp2 >|= fun exp2' -> Exp_sequence (exp1, exp2'))
  | Exp_constraint (exp, core_type) ->
    return exp <+> (shrink_expression exp >|= fun exp' -> Exp_constraint (exp', core_type))

and shrink_value_binding value_binding =
  shrink_pattern value_binding.pat
  >|= (fun pat' -> { value_binding with pat = pat' })
  <+> (shrink_expression value_binding.exp
       >|= fun exp' -> { value_binding with exp = exp' })

and shrink_case case =
  shrink_pattern case.left
  >|= (fun left' -> { case with left = left' })
  <+> (shrink_expression case.right >|= fun right' -> { case with right = right' })
;;

let shrink_structure_item = function
  | Struct_eval exp -> shrink_expression exp >|= fun exp' -> Struct_eval exp'
  | Struct_value (rec_flag, first_value_binding, value_binding_list) ->
    shrink_value_binding first_value_binding
    >|= (fun first_value_binding' ->
          Struct_value (rec_flag, first_value_binding', value_binding_list))
    <+> (list ~shrink:shrink_value_binding value_binding_list
         >|= fun value_binding_list' ->
         Struct_value (rec_flag, first_value_binding, value_binding_list'))
;;

let shrink_structure = list ~shrink:shrink_structure_item
