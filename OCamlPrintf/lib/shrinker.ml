(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ast.Expression
open QCheck.Iter
open QCheck.Shrink

let shrink_type_ident = function
  | "'a" -> empty
  | _ -> return "'a"
;;

let shrink_ident = function
  | "a" -> empty
  | _ -> return "a"
;;

let rec shrink_core_type = function
  | Type_unit | Type_bool | Type_char | Type_int | Type_string -> empty
  | Type_var id -> shrink_type_ident id >|= fun id' -> Type_var id'
  | Type_list type' ->
    return type' <+> (shrink_core_type type' >|= fun type'' -> Type_list type'')
  | Type_option type' ->
    return type' <+> (shrink_core_type type' >|= fun type'' -> Type_option type'')
  | Type_tuple (fst_type, snd_type, type_list) ->
    of_list [ fst_type; snd_type ]
    <+> of_list type_list
    <+> (shrink_core_type fst_type
         >|= fun fst_type' -> Type_tuple (fst_type', snd_type, type_list))
    <+> (shrink_core_type snd_type
         >|= fun snd_type' -> Type_tuple (fst_type, snd_type', type_list))
    <+> (list ~shrink:shrink_core_type type_list
         >|= fun type_list' -> Type_tuple (fst_type, snd_type, type_list'))
  | Type_arrow (fst_type, snd_type) ->
    of_list [ fst_type; snd_type ]
    <+> (shrink_core_type fst_type >|= fun fst_type' -> Type_arrow (fst_type', snd_type))
    <+> (shrink_core_type snd_type >|= fun snd_type' -> Type_arrow (fst_type, snd_type'))
;;

let rec shrink_pattern = function
  | Pat_any -> empty
  | Pat_var var -> shrink_ident var >|= fun var' -> Pat_var var'
  | Pat_constant const ->
    (match const with
     | Const_integer i -> int i >|= fun i' -> Pat_constant (Const_integer i')
     | Const_char ch -> char ch >|= fun ch' -> Pat_constant (Const_char ch')
     | Const_string str ->
       shrink_ident str >|= fun str' -> Pat_constant (Const_string str'))
  | Pat_tuple (fst_pat, snd_pat, pat_list) ->
    of_list [ fst_pat; snd_pat ]
    <+> of_list pat_list
    <+> (shrink_pattern fst_pat >|= fun fst_pat' -> Pat_tuple (fst_pat', snd_pat, pat_list)
        )
    <+> (shrink_pattern snd_pat >|= fun snd_pat' -> Pat_tuple (fst_pat, snd_pat', pat_list)
        )
    <+> (list ~shrink:shrink_pattern pat_list
         >|= fun pat_list' -> Pat_tuple (fst_pat, snd_pat, pat_list'))
  | Pat_construct (_, None) -> empty
  | Pat_construct (tag, Some pat) ->
    return pat <+> (shrink_pattern pat >|= fun pat' -> Pat_construct (tag, Some pat'))
  | Pat_constraint (pat, type') ->
    return pat
    <+> (shrink_pattern pat >|= fun pat' -> Pat_constraint (pat', type'))
    <+> (shrink_core_type type' >|= fun type'' -> Pat_constraint (pat, type''))
;;

let rec shrink_expression = function
  | Exp_ident id -> shrink_ident id >|= fun id' -> Exp_ident id'
  | Exp_constant const ->
    (match const with
     | Const_integer i -> int i >|= fun i' -> Exp_constant (Const_integer i')
     | Const_char ch -> char ch >|= fun ch' -> Exp_constant (Const_char ch')
     | Const_string str ->
       shrink_ident str >|= fun str' -> Exp_constant (Const_string str'))
  | Exp_let (rec_flag, fst_value_binding, value_binding_list, exp) ->
    return exp
    <+> (shrink_expression exp
         >|= fun exp' -> Exp_let (rec_flag, fst_value_binding, value_binding_list, exp'))
    <+> (shrink_value_binding fst_value_binding
         >|= fun fst_value_binding' ->
         Exp_let (rec_flag, fst_value_binding', value_binding_list, exp))
    <+> (list ~shrink:shrink_value_binding value_binding_list
         >|= fun value_binding_list' ->
         Exp_let (rec_flag, fst_value_binding, value_binding_list', exp))
    <+> (shrink_expression exp
         >|= fun exp' -> Exp_let (rec_flag, fst_value_binding, value_binding_list, exp'))
  | Exp_fun (fst_pat, pat_list, exp) ->
    return exp
    <+> (shrink_pattern fst_pat >|= fun fst_pat' -> Exp_fun (fst_pat', pat_list, exp))
    <+> (list ~shrink:shrink_pattern pat_list
         >|= fun pat_list' -> Exp_fun (fst_pat, pat_list', exp))
    <+> (shrink_expression exp >|= fun exp' -> Exp_fun (fst_pat, pat_list, exp'))
  | Exp_apply (exp_fn, exp) ->
    shrink_expression exp
    >|= (fun exp_fn' -> Exp_apply (exp_fn', exp))
    <+> (shrink_expression exp >|= fun exp' -> Exp_apply (exp_fn, exp'))
  | Exp_function (fst_case, case_list) ->
    shrink_case fst_case
    >|= (fun fst_case' -> Exp_function (fst_case', case_list))
    <+> (list ~shrink:shrink_case case_list
         >|= fun case_list' -> Exp_function (fst_case, case_list'))
  | Exp_match (exp, fst_case, case_list) ->
    return exp
    <+> (shrink_expression exp >|= fun exp' -> Exp_match (exp', fst_case, case_list))
    <+> (shrink_case fst_case >|= fun fst_case' -> Exp_match (exp, fst_case', case_list))
    <+> (list ~shrink:shrink_case case_list
         >|= fun case_list' -> Exp_match (exp, fst_case, case_list'))
  | Exp_tuple (fst_exp, snd_exp, exp_list) ->
    of_list [ fst_exp; snd_exp ]
    <+> of_list exp_list
    <+> (shrink_expression fst_exp
         >|= fun fst_exp' -> Exp_tuple (fst_exp', snd_exp, exp_list))
    <+> (shrink_expression snd_exp
         >|= fun snd_exp' -> Exp_tuple (fst_exp, snd_exp', exp_list))
    <+> (list ~shrink:shrink_expression exp_list
         >|= fun exp_list' -> Exp_tuple (fst_exp, snd_exp, exp_list'))
  | Exp_construct (_, None) -> empty
  | Exp_construct (tag, Some exp) ->
    return exp <+> (shrink_expression exp >|= fun exp' -> Exp_construct (tag, Some exp'))
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
  | Exp_constraint (exp, type') ->
    return exp
    <+> (shrink_expression exp >|= fun exp' -> Exp_constraint (exp', type'))
    <+> (shrink_core_type type' >|= fun type'' -> Exp_constraint (exp, type''))

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
  | Struct_value (rec_flag, fst_value_binding, value_binding_list) ->
    return (Struct_value (rec_flag, fst_value_binding, []))
    <+> of_list (List.map (fun vb -> Struct_value (rec_flag, vb, [])) value_binding_list)
    <+> (shrink_value_binding fst_value_binding
         >|= fun fst_value_binding' ->
         Struct_value (rec_flag, fst_value_binding', value_binding_list))
    <+> (list ~shrink:shrink_value_binding value_binding_list
         >|= fun value_binding_list' ->
         Struct_value (rec_flag, fst_value_binding, value_binding_list'))
;;

let shrink_structure = list ~shrink:shrink_structure_item
