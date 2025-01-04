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

let rec pp_core_type_deep n ppf = function
  | Type_any -> fprintf ppf "_"
  | Type_unit -> fprintf ppf "unit"
  | Type_int -> fprintf ppf "int"
  | Type_char -> fprintf ppf "char"
  | Type_string -> fprintf ppf "string"
  | Type_bool -> fprintf ppf "bool"
  | Type_option type' ->
    (match type' with
     | type' when is_type_arrow type' || is_type_list_or_option type' ->
       fprintf ppf "(%a) option" (pp_core_type_deep (n + 1)) type'
     | type' -> fprintf ppf "%a option" (pp_core_type_deep (n + 1)) type')
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
       fprintf ppf "(%a) list" (pp_core_type_deep (n + 1)) type'
     | type' -> fprintf ppf "%a list" (pp_core_type_deep (n + 1)) type')
  | Type_tuple (first_type, second_type, type_list) ->
    let pp_with_condition_on_arrow type' =
      match type' with
      | type' when is_type_arrow type' ->
        fprintf ppf "(%a)" (pp_core_type_deep (n + 1)) type'
      | _ -> fprintf ppf "%a" (pp_core_type_deep (n + 1)) type'
    in
    if n <> 0 then fprintf ppf "(";
    pp_with_condition_on_arrow first_type;
    List.iter
      (fun type' ->
        fprintf ppf " * ";
        pp_with_condition_on_arrow type')
      (second_type :: type_list);
    if n <> 0 then fprintf ppf ")"
  | Type_arrow (first_type, second_type) ->
    (match first_type with
     | first_type when is_type_arrow first_type ->
       fprintf
         ppf
         "(%a) -> %a"
         (pp_core_type_deep (n + 1))
         first_type
         (pp_core_type_deep (n + 1))
         second_type
     | first_type ->
       fprintf
         ppf
         "%a -> %a"
         (pp_core_type_deep (n + 1))
         first_type
         (pp_core_type_deep (n + 1))
         second_type)
;;

let pp_core_type = pp_core_type_deep 0

let rec pp_pattern_deep n ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var id -> pp_ident ppf id
  | Pat_constant const -> pp_constant ppf const
  | Pat_tuple (first_pat, second_pat, pat_list) ->
    if n <> 0 then fprintf ppf "(";
    fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:pp_comma (pp_pattern_deep (n + 1)))
      (first_pat :: second_pat :: pat_list);
    if n <> 0 then fprintf ppf ")"
  | Pat_construct ("::", Some pat) ->
    (match pat with
     | Pat_tuple (head, tail, []) ->
       fprintf ppf "[ %a" (pp_pattern_deep (n + 1)) head;
       let rec pp_tail pat =
         match pat with
         | Pat_construct (_, None) -> fprintf ppf " ]"
         | Pat_construct (_, Some pat_tail) ->
           (match pat_tail with
            | Pat_tuple (next_head, next_tail, []) ->
              fprintf ppf "; %a" (pp_pattern_deep (n + 1)) next_head;
              pp_tail next_tail
            | _ -> ())
         | _ -> fprintf ppf "; %a ]" (pp_pattern_deep (n + 1)) pat
       in
       pp_tail tail
     | _ -> ())
  | Pat_construct (tag, None) -> fprintf ppf "%s" tag
  | Pat_construct ("Some", Some pat) ->
    fprintf ppf "Some (%a)" (pp_pattern_deep (n + 1)) pat
  | Pat_construct (_, _) -> ()
  | Pat_constraint (pat, core_type) ->
    if n <> 0 then fprintf ppf "(";
    fprintf ppf "%a : %a" (pp_pattern_deep (n + 1)) pat pp_core_type core_type;
    if n <> 0 then fprintf ppf ")"
;;

let pp_pattern = pp_pattern_deep 0

let rec pp_expression_deep n ppf = function
  | Exp_ident id -> pp_ident ppf id
  | Exp_constant const -> pp_constant ppf const
  | Exp_let (rec_flag, first_value_binding, value_binding_list, exp) ->
    if n <> 0 then fprintf ppf "(";
    pp_rec_flag ppf rec_flag;
    fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:pp_and pp_value_binding)
      (first_value_binding :: value_binding_list);
    fprintf ppf " in %a" (pp_expression_deep (n + 1)) exp;
    if n <> 0 then fprintf ppf ")"
  | Exp_fun (first_pat, pat_list, exp) ->
    if n <> 0 then fprintf ppf "(";
    fprintf
      ppf
      "fun %a "
      (pp_print_list ~pp_sep:pp_space (pp_pattern_deep 1))
      (first_pat :: pat_list);
    fprintf ppf "-> %a" (pp_expression_deep (n + 1)) exp;
    if n <> 0 then fprintf ppf ")"
  | Exp_apply (exp1, exp2) -> (pp_exp_apply n) ppf (exp1, exp2)
  | Exp_function (first_case, case_list) ->
    if n <> 0 then fprintf ppf "(";
    fprintf
      ppf
      "function %a"
      (pp_print_list ~pp_sep:pp_space pp_case)
      (first_case :: case_list);
    if n <> 0 then fprintf ppf ")"
  | Exp_match (exp, first_case, case_list) ->
    if n <> 0 then fprintf ppf "(";
    fprintf ppf "match %a with" (pp_expression_deep (n + 1)) exp;
    fprintf ppf " %a" (pp_print_list ~pp_sep:pp_space pp_case) (first_case :: case_list);
    if n <> 0 then fprintf ppf ")"
  | Exp_tuple (first_exp, second_exp, exp_list) ->
    if n <> 0 then fprintf ppf "(";
    fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:pp_comma (pp_expression_deep (n + 1)))
      (first_exp :: second_exp :: exp_list);
    if n <> 0 then fprintf ppf ")"
  | Exp_construct ("::", Some exp) ->
    (match exp with
     | Exp_tuple (head, tail, []) ->
       fprintf ppf "[ %a" (pp_expression_deep (n + 1)) head;
       let rec pp_tail exp =
         match exp with
         | Exp_construct (_, None) -> fprintf ppf " ]"
         | Exp_construct (_, Some exp_tail) ->
           (match exp_tail with
            | Exp_tuple (next_head, next_tail, []) ->
              fprintf ppf "; %a" (pp_expression_deep (n + 1)) next_head;
              pp_tail next_tail
            | _ -> ())
         | _ -> fprintf ppf "; %a ]" (pp_expression_deep (n + 1)) exp
       in
       pp_tail tail
     | _ -> ())
  | Exp_construct (tag, None) -> fprintf ppf "%s" tag
  | Exp_construct ("Some", Some exp) ->
    fprintf ppf "Some (%a)" (pp_expression_deep (n + 1)) exp
  | Exp_construct (_, _) -> ()
  | Exp_ifthenelse (exp1, exp2, None) ->
    if n <> 0 then fprintf ppf "(";
    fprintf
      ppf
      "if %a then %a"
      (pp_expression_deep (n + 1))
      exp1
      (pp_expression_deep (n + 1))
      exp2;
    if n <> 0 then fprintf ppf ")"
  | Exp_ifthenelse (exp1, exp2, Some exp3) ->
    if n <> 0 then fprintf ppf "(";
    fprintf
      ppf
      "if %a then %a else %a"
      (pp_expression_deep (n + 1))
      exp1
      (pp_expression_deep (n + 1))
      exp2
      (pp_expression_deep (n + 1))
      exp3;
    if n <> 0 then fprintf ppf ")"
  | Exp_sequence (exp1, exp2) ->
    if n <> 0 then fprintf ppf "(";
    fprintf
      ppf
      "%a; %a"
      (pp_expression_deep (n + 1))
      exp1
      (pp_expression_deep (n + 1))
      exp2;
    if n <> 0 then fprintf ppf ")"
  | Exp_constraint (exp, core_type) ->
    fprintf ppf "(%a : %a)" (pp_expression_deep (n + 1)) exp pp_core_type core_type

and pp_exp_apply n ppf (exp1, exp2) =
  match exp1 with
  | Exp_ident exp_opr when is_operator exp_opr ->
    (match exp2 with
     | Exp_apply (Exp_apply (Exp_ident opr1, exp1), opn) when is_operator opr1 ->
       (match get_priority exp_opr with
        | 4 | 5 ->
          if get_priority exp_opr <= get_priority opr1
          then fprintf ppf "(%a)" (pp_exp_apply n) (Exp_ident opr1, exp1)
          else fprintf ppf "%a" (pp_exp_apply n) (Exp_ident opr1, exp1)
        | _ ->
          if get_priority exp_opr < get_priority opr1
          then fprintf ppf "(%a)" (pp_exp_apply n) (Exp_ident opr1, exp1)
          else fprintf ppf "%a" (pp_exp_apply n) (Exp_ident opr1, exp1));
       fprintf ppf " %s " exp_opr;
       (match opn with
        | Exp_apply (Exp_ident opr2, exp2) when is_operator opr2 ->
          if get_priority opr1 <= get_priority opr2
             && get_priority exp_opr < get_priority opr2
          then fprintf ppf "(%a)" (pp_exp_apply n) (Exp_ident opr2, exp2)
          else fprintf ppf "%a" (pp_exp_apply n) (Exp_ident opr2, exp2)
        | _ -> fprintf ppf "%a" (pp_expression_deep (n + 1)) opn)
     | Exp_apply (opn, Exp_apply (Exp_ident opr2, exp2)) when is_operator opr2 ->
       (match opn with
        | Exp_apply (Exp_ident opr1, exp1) when is_operator opr1 ->
          if get_priority opr2 <= get_priority opr1
             && get_priority exp_opr < get_priority opr1
          then fprintf ppf "(%a)" (pp_exp_apply n) (Exp_ident opr1, exp1)
          else fprintf ppf "%a" (pp_exp_apply n) (Exp_ident opr1, exp1)
        | _ -> fprintf ppf "%a" (pp_expression_deep (n + 1)) opn);
       fprintf ppf " %s " exp_opr;
       (match get_priority exp_opr with
        | 1 | 2 | 3 ->
          if get_priority exp_opr <= get_priority opr2
          then fprintf ppf "(%a)" (pp_exp_apply n) (Exp_ident opr2, exp2)
          else fprintf ppf "%a" (pp_exp_apply n) (Exp_ident opr2, exp2)
        | _ ->
          if get_priority exp_opr < get_priority opr2
          then fprintf ppf "(%a)" (pp_exp_apply n) (Exp_ident opr2, exp2)
          else fprintf ppf "%a" (pp_exp_apply n) (Exp_ident opr2, exp2))
     | Exp_apply (opn1, opn2) ->
       fprintf
         ppf
         "%a %s %a"
         (pp_expression_deep (n + 1))
         opn1
         exp_opr
         (pp_expression_deep (n + 1))
         opn2
     | _ -> (pp_expression_deep (n + 1)) ppf exp2)
  | Exp_ident exp_opr when is_negative_op exp_opr ->
    (match exp2 with
     | Exp_apply (exp1, exp2) -> fprintf ppf "-(%a)" (pp_exp_apply n) (exp1, exp2)
     | _ -> fprintf ppf "-%a" (pp_expression_deep (n + 1)) exp2)
  | _ ->
    (match exp1 with
     | Exp_apply _ -> fprintf ppf " "
     | _ -> ());
    (match exp2 with
     | Exp_apply (_, _) ->
       fprintf
         ppf
         "%a (%a)"
         (pp_expression_deep (n + 1))
         exp1
         (pp_expression_deep (n + 1))
         exp2
     | _ ->
       fprintf
         ppf
         "%a %a"
         (pp_expression_deep (n + 1))
         exp1
         (pp_expression_deep (n + 1))
         exp2)

and pp_value_binding ppf = function
  | { pat = pat_fun; exp = Exp_fun (pat, pat_list, exp) } ->
    let pp_pattern_arg () =
      fprintf
        ppf
        "%a "
        (pp_print_list ~pp_sep:pp_space (pp_pattern_deep 1))
        (pat :: pat_list)
    in
    (match pat_fun with
     | Pat_constraint (name, type') ->
       fprintf ppf "%a " pp_pattern name;
       pp_pattern_arg ();
       fprintf ppf ": %a " pp_core_type type'
     | _ ->
       fprintf ppf "%a " pp_pattern pat_fun;
       pp_pattern_arg ());
    fprintf ppf "= %a" (pp_expression_deep 0) exp
  | { pat; exp } -> fprintf ppf "%a = %a" pp_pattern pat (pp_expression_deep 0) exp

and pp_case ppf = function
  | { left; right } ->
    fprintf ppf "| %a -> %a" (pp_pattern_deep 1) left (pp_expression_deep 1) right
;;

let pp_expression = pp_expression_deep 0

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
