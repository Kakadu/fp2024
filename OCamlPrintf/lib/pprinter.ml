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

let is_negative_op = function
  | "~-" -> true
  | _ -> false
;;

let is_type_arrow = function
  | Type_arrow (_, _) -> true
  | _ -> false
;;

let is_type_list_or_option = function
  | Type_list _ | Type_option _ -> true
  | _ -> false
;;

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

let rec pp_core_type_deep need_parens ppf = function
  | Type_any -> fprintf ppf "_"
  | Type_unit -> fprintf ppf "unit"
  | Type_int -> fprintf ppf "int"
  | Type_char -> fprintf ppf "char"
  | Type_string -> fprintf ppf "string"
  | Type_bool -> fprintf ppf "bool"
  | Type_option type' ->
    (match type' with
     | type' when is_type_arrow type' || is_type_list_or_option type' ->
       fprintf ppf "(%a) option" (pp_core_type_deep true) type'
     | type' -> fprintf ppf "%a option" (pp_core_type_deep true) type')
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
     | type' when is_type_arrow type' || is_type_list_or_option type' ->
       fprintf ppf "(%a) list" (pp_core_type_deep true) type'
     | type' -> fprintf ppf "%a list" (pp_core_type_deep true) type')
  | Type_tuple (first_type, second_type, type_list) ->
    let pp_with_condition_on_arrow type' =
      match type' with
      | type' when is_type_arrow type' ->
        fprintf ppf "(%a)" (pp_core_type_deep true) type'
      | _ -> fprintf ppf "%a" (pp_core_type_deep true) type'
    in
    if need_parens then fprintf ppf "(";
    pp_with_condition_on_arrow first_type;
    List.iter
      (fun type' ->
        fprintf ppf " * ";
        pp_with_condition_on_arrow type')
      (second_type :: type_list);
    if need_parens then fprintf ppf ")"
  | Type_arrow (first_type, second_type) ->
    (match first_type with
     | first_type when is_type_arrow first_type ->
       fprintf
         ppf
         "(%a) -> %a"
         (pp_core_type_deep true)
         first_type
         (pp_core_type_deep true)
         second_type
     | first_type ->
       fprintf
         ppf
         "%a -> %a"
         (pp_core_type_deep true)
         first_type
         (pp_core_type_deep true)
         second_type)
;;

let pp_core_type = pp_core_type_deep false

let rec pp_pattern_deep need_parens ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var id -> pp_ident ppf id
  | Pat_constant const -> pp_constant ppf const
  | Pat_tuple (first_pat, second_pat, pat_list) ->
    pp_open_hvbox ppf 0;
    if need_parens then fprintf ppf "( ";
    fprintf
      ppf
      "%a@]"
      (pp_print_list ~pp_sep:pp_comma (pp_pattern_deep true))
      (first_pat :: second_pat :: pat_list);
    if need_parens then fprintf ppf " )"
  | Pat_construct ("::", Some pat) ->
    (match pat with
     | Pat_tuple (head, tail, []) ->
       fprintf ppf "@[<hv>[ %a" (pp_pattern_deep true) head;
       let rec pp_tail pat =
         match pat with
         | Pat_construct (_, None) -> fprintf ppf "@ ]@]"
         | Pat_construct (_, Some pat_tail) ->
           (match pat_tail with
            | Pat_tuple (next_head, next_tail, []) ->
              fprintf ppf "@,; %a" (pp_pattern_deep true) next_head;
              pp_tail next_tail
            | _ -> ())
         | _ -> fprintf ppf ";@ %a@ ]@]" (pp_pattern_deep true) pat
       in
       pp_tail tail
     | _ -> ())
  | Pat_construct (tag, None) -> fprintf ppf "%s" tag
  | Pat_construct ("Some", Some pat) -> fprintf ppf "Some (%a)" (pp_pattern_deep true) pat
  | Pat_construct (_, _) -> ()
  | Pat_constraint (pat, core_type) ->
    if need_parens then fprintf ppf "(";
    fprintf ppf "@[%a@ :@ %a@]" (pp_pattern_deep true) pat pp_core_type core_type;
    if need_parens then fprintf ppf ")"
;;

let pp_pattern = pp_pattern_deep false

let rec pp_expression_deep need_cut need_parens ppf = function
  | Exp_ident id -> pp_ident ppf id
  | Exp_constant const -> pp_constant ppf const
  | Exp_let (rec_flag, first_value_binding, value_binding_list, exp) ->
    if need_parens then fprintf ppf "(";
    pp_open_hvbox ppf 0;
    (pp_value_binding_list 0) ppf (rec_flag, first_value_binding :: value_binding_list);
    fprintf ppf " in@ %a" (pp_expression_deep true true) exp;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_fun (first_pat, pat_list, exp) ->
    if need_parens then fprintf ppf "(";
    pp_open_box ppf 2;
    fprintf
      ppf
      "fun@ %a@ "
      (pp_print_list ~pp_sep:pp_print_space (pp_pattern_deep true))
      (first_pat :: pat_list);
    fprintf ppf "->@ %a" (pp_expression_deep false true) exp;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_apply (exp1, exp2) ->
    pp_open_box ppf 2;
    (pp_exp_apply ~need_parens) ppf (exp1, exp2);
    pp_close_box ppf ()
  | Exp_function (first_case, case_list) ->
    if need_cut then pp_force_newline ppf ();
    if need_parens then fprintf ppf "(";
    pp_open_vbox ppf 0;
    fprintf ppf "function@ ";
    fprintf ppf "%a" (pp_print_list pp_case) (first_case :: case_list);
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_match (exp, first_case, case_list) ->
    if need_cut then pp_force_newline ppf ();
    if need_parens then fprintf ppf "(";
    pp_open_vbox ppf 0;
    pp_open_hvbox ppf 0;
    if need_parens then pp_open_vbox ppf 1 else pp_open_vbox ppf 2;
    fprintf ppf "match %a@]@ with@]@ " (pp_expression_deep true false) exp;
    fprintf ppf "%a" (pp_print_list pp_case) (first_case :: case_list);
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | Exp_tuple (first_exp, second_exp, exp_list) ->
    pp_open_hvbox ppf 0;
    if need_parens then fprintf ppf "( ";
    fprintf
      ppf
      "%a@]"
      (pp_print_list ~pp_sep:pp_comma (pp_expression_deep false true))
      (first_exp :: second_exp :: exp_list);
    if need_parens then fprintf ppf " )"
  | Exp_construct ("::", Some exp) ->
    (match exp with
     | Exp_tuple (head, tail, []) ->
       fprintf ppf "@[<hv>[ %a" (pp_expression_deep false true) head;
       let rec pp_tail exp =
         match exp with
         | Exp_construct (_, None) -> fprintf ppf "@ ]@]"
         | Exp_construct (_, Some exp_tail) ->
           (match exp_tail with
            | Exp_tuple (next_head, next_tail, []) ->
              fprintf ppf "@,; %a" (pp_expression_deep false true) next_head;
              pp_tail next_tail
            | _ -> ())
         | _ -> fprintf ppf ";@ %a@ ]@]" (pp_expression_deep false true) exp
       in
       pp_tail tail
     | _ -> ())
  | Exp_construct (tag, None) -> fprintf ppf "%s" tag
  | Exp_construct ("Some", Some exp) ->
    fprintf ppf "Some (%a)" (pp_expression_deep false true) exp
  | Exp_construct (_, _) -> ()
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
        | 4 | 5 -> pp (get_priority exp_opr <= get_priority opr1) opr1 exp1
        | _ -> pp (get_priority exp_opr < get_priority opr1) opr1 exp1);
       fprintf ppf " %s@ " exp_opr;
       (match opn with
        | Exp_apply (Exp_ident opr2, exp2) when is_operator opr2 ->
          pp
            (get_priority opr1 <= get_priority opr2
             && get_priority exp_opr < get_priority opr2)
            opr2
            exp2
        | _ -> fprintf ppf "%a" (pp_expression_deep false true) opn)
     | Exp_apply (opn, Exp_apply (Exp_ident opr2, exp2)) when is_operator opr2 ->
       (match opn with
        | Exp_apply (Exp_ident opr1, exp1) when is_operator opr1 ->
          pp
            (get_priority opr2 <= get_priority opr1
             && get_priority exp_opr < get_priority opr1)
            opr1
            exp1
        | _ -> fprintf ppf "%a" (pp_expression_deep false true) opn);
       fprintf ppf " %s@ " exp_opr;
       (match get_priority exp_opr with
        | 1 | 2 | 3 -> pp (get_priority exp_opr <= get_priority opr2) opr2 exp2
        | _ -> pp (get_priority exp_opr < get_priority opr2) opr2 exp2)
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
    (match exp1 with
     | Exp_apply _ -> fprintf ppf " "
     | _ -> ());
    fprintf ppf "%a " (pp_expression_deep false true) exp1;
    (match exp2 with
     | Exp_apply _ -> fprintf ppf "(%a)" (pp_expression_deep false true) exp2
     | _ -> fprintf ppf "%a" (pp_expression_deep false true) exp2)

and pp_value_binding ppf =
  pp_open_hvbox ppf 0;
  function
  | { pat = pat_fun; exp = Exp_fun (pat, pat_list, exp) } ->
    let pp_pattern_arg () =
      fprintf
        ppf
        "%a"
        (pp_print_list ~pp_sep:pp_print_space (pp_pattern_deep true))
        (pat :: pat_list)
    in
    (match pat_fun with
     | Pat_constraint (name, type') ->
       fprintf ppf "%a@ " pp_pattern name;
       pp_pattern_arg ();
       fprintf ppf "@ : %a" pp_core_type type'
     | _ ->
       fprintf ppf "%a@ " pp_pattern pat_fun;
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
  | Struct_value (rec_flag, first_value_binding, value_binding_list) ->
    (pp_value_binding_list 2) ppf (rec_flag, first_value_binding :: value_binding_list);
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
