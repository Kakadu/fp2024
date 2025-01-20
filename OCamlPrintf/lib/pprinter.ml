(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 **)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ast.Expression
open Format

let pp_rec_flag ppf = function
  | Recursive -> fprintf ppf "let rec "
  | Nonrecursive -> fprintf ppf "let "
;;

let pp_comma ppf () = fprintf ppf "@,, "

let pp_and indent ppf () =
  pp_force_newline ppf ();
  pp_open_hovbox ppf indent;
  fprintf ppf "and "
;;

let pp_ident ppf = fprintf ppf "%s"

let pp_constant ppf = function
  | Const_integer n -> fprintf ppf "%d" n
  | Const_char c -> fprintf ppf "'%c'" c
  | Const_string s -> fprintf ppf "%S" s
;;

let rec pp_core_type_deep n ppf = function
  | Type_unit -> fprintf ppf "unit"
  | Type_int -> fprintf ppf "int"
  | Type_char -> fprintf ppf "char"
  | Type_string -> fprintf ppf "string"
  | Type_bool -> fprintf ppf "bool"
  | Type_option type' -> fprintf ppf "%a option" (pp_core_type_deep 2) type'
  | Type_var type' -> pp_ident ppf type'
  | Type_list type' -> fprintf ppf "%a list" (pp_core_type_deep 2) type'
  | Type_tuple (fst_type, snd_type, type_list) ->
    if n = 2 then fprintf ppf "(";
    fprintf ppf "%a" (pp_core_type_deep 2) fst_type;
    List.iter
      (fun type' ->
        fprintf ppf " * ";
        fprintf ppf "%a" (pp_core_type_deep 2) type')
      (snd_type :: type_list);
    if n = 2 then fprintf ppf ")"
  | Type_arrow (fst_type, snd_type) ->
    if n <> 0 then fprintf ppf "(";
    fprintf ppf "%a -> %a" (pp_core_type_deep 1) fst_type (pp_core_type_deep 0) snd_type;
    if n <> 0 then fprintf ppf ")"
;;

let pp_core_type = pp_core_type_deep 0

let rec pp_pattern_deep need_parens ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var var -> pp_ident ppf var
  | Pat_constant const -> pp_constant ppf const
  | Pat_tuple (fst_pat, snd_pat, pat_list) ->
    pp_open_hvbox ppf 0;
    if need_parens then fprintf ppf "( ";
    fprintf
      ppf
      "%a@]"
      (pp_print_list ~pp_sep:pp_comma (pp_pattern_deep true))
      (fst_pat :: snd_pat :: pat_list);
    if need_parens then fprintf ppf " )"
  | Pat_construct ("::", Some (Pat_tuple (head, tail, []))) ->
    fprintf ppf "@[<hv>[ %a" (pp_pattern_deep true) head;
    let rec pp_tail = function
      | Pat_construct (_, None) -> fprintf ppf "@ ]@]"
      | Pat_construct (_, Some (Pat_tuple (next_head, next_tail, []))) ->
        fprintf ppf "@,; %a" (pp_pattern_deep true) next_head;
        pp_tail next_tail
      | Pat_construct (_, Some _) -> ()
      | pat -> fprintf ppf ";@ %a@ ]@]" (pp_pattern_deep true) pat
    in
    pp_tail tail
  | Pat_construct (tag, None) -> fprintf ppf "%s" tag
  | Pat_construct ("Some", Some pat) -> fprintf ppf "Some (%a)" (pp_pattern_deep true) pat
  | Pat_construct _ -> ()
  | Pat_constraint (pat, core_type) ->
    if need_parens then fprintf ppf "(";
    fprintf ppf "@[%a@ :@ %a@]" (pp_pattern_deep true) pat pp_core_type core_type;
    if need_parens then fprintf ppf ")"
;;

let pp_pattern = pp_pattern_deep false

let rec pp_expression_deep need_cut need_parens ppf = function
  | Exp_ident id -> pp_ident ppf id
  | Exp_constant const -> pp_constant ppf const
  | Exp_let (rec_flag, fst_value_binding, value_binding_list, exp) ->
    if need_parens then fprintf ppf "(";
    pp_open_hvbox ppf 0;
    (pp_value_binding_list 0) ppf (rec_flag, fst_value_binding :: value_binding_list);
    fprintf ppf " in@ %a" (pp_expression_deep true true) exp;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_fun (fst_pat, pat_list, exp) ->
    if need_parens then fprintf ppf "(";
    pp_open_box ppf 2;
    fprintf
      ppf
      "fun@ %a@ "
      (pp_print_list ~pp_sep:pp_print_space (pp_pattern_deep true))
      (fst_pat :: pat_list);
    fprintf ppf "->@ %a" (pp_expression_deep false true) exp;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_apply (exp1, exp2) ->
    pp_open_box ppf 2;
    (pp_exp_apply ~need_parens) ppf (exp1, exp2);
    pp_close_box ppf ()
  | Exp_function (fst_case, case_list) ->
    if need_cut then pp_force_newline ppf ();
    if need_parens then fprintf ppf "(";
    pp_open_vbox ppf 0;
    fprintf ppf "function@ ";
    fprintf ppf "%a" (pp_print_list pp_case) (fst_case :: case_list);
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_match (exp, fst_case, case_list) ->
    if need_cut then pp_force_newline ppf ();
    if need_parens then fprintf ppf "(";
    pp_open_vbox ppf 0;
    pp_open_hvbox ppf 0;
    if need_parens then pp_open_vbox ppf 1 else pp_open_vbox ppf 2;
    fprintf ppf "match %a@]@ with@]@ " (pp_expression_deep true false) exp;
    fprintf ppf "%a" (pp_print_list pp_case) (fst_case :: case_list);
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_tuple (fst_exp, snd_exp, exp_list) ->
    pp_open_hvbox ppf 0;
    if need_parens then fprintf ppf "( ";
    fprintf
      ppf
      "%a@]"
      (pp_print_list ~pp_sep:pp_comma (pp_expression_deep false true))
      (fst_exp :: snd_exp :: exp_list);
    if need_parens then fprintf ppf " )"
  | Exp_construct ("::", Some (Exp_tuple (head, tail, []))) ->
    fprintf ppf "@[<hv>[ %a" (pp_expression_deep false true) head;
    let rec pp_tail = function
      | Exp_construct (_, None) -> fprintf ppf "@ ]@]"
      | Exp_construct (_, Some (Exp_tuple (next_head, next_tail, []))) ->
        fprintf ppf "@,; %a" (pp_expression_deep false true) next_head;
        pp_tail next_tail
      | Exp_construct (_, Some _) -> ()
      | exp -> fprintf ppf ";@ %a@ ]@]" (pp_expression_deep false true) exp
    in
    pp_tail tail
  | Exp_construct (tag, None) -> fprintf ppf "%s" tag
  | Exp_construct ("Some", Some exp) ->
    fprintf ppf "Some (%a)" (pp_expression_deep false true) exp
  | Exp_construct _ -> ()
  | Exp_ifthenelse (exp1, exp2, None) ->
    if need_parens then fprintf ppf "(";
    pp_open_box ppf 0;
    fprintf ppf "if %a@ " (pp_expression_deep false false) exp1;
    fprintf ppf "@[<v 2>then %a@]" (pp_expression_deep true true) exp2;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_ifthenelse (exp1, exp2, Some exp3) ->
    if need_parens then fprintf ppf "(";
    pp_open_box ppf 0;
    fprintf ppf "if %a@ " (pp_expression_deep false false) exp1;
    fprintf ppf "@[<v 2>then %a@]@ " (pp_expression_deep true true) exp2;
    fprintf ppf "@[<v 2>else %a@]" (pp_expression_deep true true) exp3;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_sequence (exp1, exp2) ->
    if need_parens then fprintf ppf "(";
    pp_open_box ppf 0;
    fprintf ppf "%a; " (pp_expression_deep need_cut true) exp1;
    fprintf ppf "%a" (pp_expression_deep need_cut true) exp2;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_constraint (exp, core_type) ->
    fprintf
      ppf
      "@[(%a@ :@ %a)@]"
      (pp_expression_deep false false)
      exp
      pp_core_type
      core_type

and pp_exp_apply ?(need_parens = false) ppf (exp1, exp2) =
  let ( < ) opr1 opr2 = get_priority opr1 < get_priority opr2
  and ( <= ) opr1 opr2 = get_priority opr1 <= get_priority opr2 in
  let pp condition opr exp =
    if condition
    then fprintf ppf "(%a)" (pp_exp_apply ~need_parens) (Exp_ident opr, exp)
    else fprintf ppf "%a" (pp_exp_apply ~need_parens) (Exp_ident opr, exp)
  in
  match exp1 with
  | Exp_ident exp_opr when is_operator exp_opr ->
    (match exp2 with
     | Exp_apply (Exp_apply (Exp_ident opr1, exp1), opn) when is_operator opr1 ->
       (match get_priority exp_opr with
        | 3 | 5 | 6 -> pp (exp_opr <= opr1) opr1 exp1
        | _ -> pp (exp_opr < opr1) opr1 exp1);
       fprintf ppf " %s@ " exp_opr;
       (match opn with
        | Exp_apply (Exp_ident opr2, exp2) when is_operator opr2 ->
          pp (opr1 <= opr2 && exp_opr < opr2) opr2 exp2
        | _ -> fprintf ppf "%a" (pp_expression_deep false true) opn)
     | Exp_apply (opn, Exp_apply (Exp_ident opr2, exp2)) when is_operator opr2 ->
       (match opn with
        | Exp_apply (Exp_ident opr1, exp1) when is_operator opr1 ->
          pp (opr2 <= opr1 && exp_opr < opr1) opr1 exp1
        | _ -> fprintf ppf "%a" (pp_expression_deep false true) opn);
       fprintf ppf " %s@ " exp_opr;
       (match get_priority exp_opr with
        | 1 | 2 | 4 -> pp (exp_opr <= opr2) opr2 exp2
        | _ -> pp (exp_opr < opr2) opr2 exp2)
     | Exp_apply (opn1, opn2) ->
       fprintf
         ppf
         "%a %s@ %a"
         (pp_expression_deep false true)
         opn1
         exp_opr
         (pp_expression_deep false true)
         opn2
     | _ -> (pp_expression_deep false true) ppf exp2)
  | Exp_ident exp_opr when is_negative_op exp_opr ->
    (match exp2 with
     | Exp_ident _ | Exp_constant _ ->
       fprintf ppf "-%a" (pp_expression_deep false need_parens) exp2
     | Exp_apply _ -> fprintf ppf "-(%a)" (pp_expression_deep false need_parens) exp2
     | _ -> fprintf ppf "-%a" (pp_expression_deep false true) exp2)
  | _ ->
    fprintf ppf "%a " (pp_expression_deep false true) exp1;
    (match exp2 with
     | Exp_apply _ -> fprintf ppf "(%a)" (pp_expression_deep false true) exp2
     | _ -> fprintf ppf "%a" (pp_expression_deep false true) exp2)

and pp_value_binding ppf =
  pp_open_hvbox ppf 0;
  function
  | { pat = pat_var; exp = Exp_fun (pat, pat_list, exp) } ->
    let pp_pattern_arg () =
      fprintf
        ppf
        "%a"
        (pp_print_list ~pp_sep:pp_print_space (pp_pattern_deep true))
        (pat :: pat_list)
    in
    (match pat_var with
     | Pat_constraint (pat, type') ->
       fprintf ppf "%a@ " pp_pattern pat;
       pp_pattern_arg ();
       fprintf ppf "@ : %a" pp_core_type type'
     | _ ->
       fprintf ppf "%a@ " pp_pattern pat_var;
       pp_pattern_arg ());
    fprintf ppf "@ =@]@ ";
    fprintf ppf "@[<hv>%a@]@]" (pp_expression_deep false false) exp
  | { pat; exp = Exp_let _ as exp } ->
    fprintf ppf "%a =@]@\n" pp_pattern pat;
    fprintf ppf "@[<hv>%a@]@]" (pp_expression_deep false false) exp
  | { pat; exp } ->
    fprintf ppf "%a =@]@ " pp_pattern pat;
    fprintf ppf "@[<hv>%a@]@]" (pp_expression_deep false false) exp

and pp_case ppf = function
  | { left; right } ->
    fprintf
      ppf
      "@[<hov 2>| %a ->@ %a@]"
      (pp_pattern_deep true)
      left
      (pp_expression_deep true true)
      right

and pp_value_binding_list indent ppf = function
  | rec_flag, value_binding_list ->
    pp_open_hovbox ppf indent;
    pp_rec_flag ppf rec_flag;
    fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:(pp_and indent) pp_value_binding)
      value_binding_list
;;

let pp_expression = pp_expression_deep false false

let pp_structure_item ppf = function
  | Struct_eval exp ->
    fprintf ppf "@[<hv>%a@];;" pp_expression exp;
    pp_print_flush ppf ()
  | Struct_value (rec_flag, fst_value_binding, value_binding_list) ->
    (pp_value_binding_list 2) ppf (rec_flag, fst_value_binding :: value_binding_list);
    pp_print_if_newline ppf ();
    pp_print_cut ppf ();
    fprintf ppf ";;";
    pp_print_flush ppf ()
;;

let pp_structure ppf ast =
  if Base.List.is_empty ast
  then fprintf ppf ";;"
  else fprintf ppf "@[%a@]" (pp_print_list ~pp_sep:pp_force_newline pp_structure_item) ast;
  pp_print_flush ppf ()
;;
