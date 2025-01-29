(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Stdlib.Format
open Typedtree

let pp_id ppf = function
  | Id name -> fprintf ppf "%s" name
;;

let pp_const ppf = function
  | Int i -> fprintf ppf "%d" i
  | String s -> fprintf ppf "%S" s
  | Bool b -> fprintf ppf "%b" b
  | Unit -> fprintf ppf "()"
;;

let pp_bin_op ppf = function
  | Add -> fprintf ppf "+"
  | Mult -> fprintf ppf "*"
  | Sub -> fprintf ppf "-"
  | Div -> fprintf ppf "/"
  | Gt -> fprintf ppf ">"
  | Lt -> fprintf ppf "<"
  | Eq -> fprintf ppf "="
  | Neq -> fprintf ppf "<>"
  | Gte -> fprintf ppf ">="
  | Lte -> fprintf ppf "<="
  | And -> fprintf ppf "&&"
  | Or -> fprintf ppf "||"
  | Cons -> fprintf ppf "::"
;;

let pp_un_op ppf = function
  | Negative -> fprintf ppf "-"
  | Positive -> fprintf ppf "+"
  | Not -> fprintf ppf "not "
;;

let pp_rec_flag ppf = function
  | Recursive -> fprintf ppf "rec"
  | Non_recursive -> ()
;;

let rec pp_pattern ppf = function
  | PVar id -> pp_id ppf id
  | PConst c -> pp_const ppf c
  | PAny -> fprintf ppf "_"
  | PTuple (p1, p2, rest) ->
    let patterns =
      String.concat ~sep:", " (List.map ~f:(asprintf "%a" pp_pattern) (p1 :: p2 :: rest))
    in
    fprintf ppf "(%s)" patterns
  | PList patterns ->
    let patterns_str =
      String.concat ~sep:"; " (List.map ~f:(asprintf "%a" pp_pattern) patterns)
    in
    fprintf ppf "[%s]" patterns_str
  | PCons (p1, p2) -> fprintf ppf "%a :: %a" pp_pattern p1 pp_pattern p2
  | POption (Some p) -> fprintf ppf "(Some %a)" pp_pattern p
  | POption None -> fprintf ppf "None"
  | PConstraint (p, t) -> fprintf ppf "(%a : %a)" pp_pattern p pp_ty t
;;

let precedence_bin_op = function
  | Mult | Div -> 2
  | Add | Sub -> 1
  | And | Or -> 0
  | Gt | Lt | Eq | Neq | Gte | Lte | Cons -> -1
;;

(* let pp_label ppf = function
   | Label name -> fprintf ppf "%s" name
   ;; *)

let rec pp_expr ppf expr =
  let needs_parens parent_prec child_prec = child_prec < parent_prec || child_prec = -1 in
  match expr with
  | Econst c -> pp_const ppf c
  | Evar id -> pp_id ppf id
  | Eif_then_else (e1, e2, None) -> fprintf ppf "if %a then %a" pp_expr e1 pp_expr e2
  | Eif_then_else (e1, e2, Some e3) ->
    fprintf ppf "if %a then %a else %a" pp_expr e1 pp_expr e2 pp_expr e3
  | Ematch (exp, Ecase (first_pat, first_expr), rest_cases) ->
    let case_to_string (Ecase (pat, expr)) =
      asprintf "| %a -> %a" pp_pattern pat pp_expr expr
    in
    let case_list_str =
      String.concat
        ~sep:" "
        (case_to_string (Ecase (first_pat, first_expr))
         :: List.map ~f:case_to_string rest_cases)
    in
    fprintf ppf "match %a with %s" pp_expr exp case_list_str
  | Efunction (Ecase (first_pat, first_expr), rest_cases) ->
    let case_to_string (Ecase (pat, expr)) =
      asprintf "| %a -> %a" pp_pattern pat pp_expr expr
    in
    let case_list_str =
      String.concat
        ~sep:" "
        (case_to_string (Ecase (first_pat, first_expr))
         :: List.map ~f:case_to_string rest_cases)
    in
    fprintf ppf "function %s" case_list_str
  | Eoption (Some e) -> fprintf ppf "(Some %a)" pp_expr e
  | Eoption None -> fprintf ppf "None"
  | Etuple (e1, e2, es) ->
    fprintf
      ppf
      "(%a, %a%a)"
      pp_expr
      e1
      pp_expr
      e2
      (fun ppf -> List.iter ~f:(fprintf ppf ", %a" pp_expr))
      es
  | Elist es ->
    fprintf
      ppf
      "[%a]"
      (fun ppf ->
        List.iteri ~f:(fun i e ->
          if i > 0 then fprintf ppf "; %a" pp_expr e else pp_expr ppf e))
      es
  | Efun (first_pattern, rest_patterns, e) ->
    fprintf
      ppf
      "(fun %a%a -> %a)"
      pp_pattern
      first_pattern
      (fun ppf patterns ->
        List.iter patterns ~f:(fun pat -> fprintf ppf " %a" pp_pattern pat))
      rest_patterns
      pp_expr
      e
  | Ebin_op (op, e1, e2) ->
    let op_prec = precedence_bin_op op in
    fprintf
      ppf
      "%a %a %a"
      (fun ppf e ->
        if needs_parens op_prec (precedence e)
        then fprintf ppf "(%a)" pp_expr e
        else pp_expr ppf e)
      e1
      pp_bin_op
      op
      (fun ppf e ->
        if needs_parens op_prec (precedence e)
        then fprintf ppf "(%a)" pp_expr e
        else pp_expr ppf e)
      e2
  | Eun_op (op, e) -> fprintf ppf "%a(%a)" pp_un_op op pp_expr e
  | Elet (rec_flag, vb, vb_l, e) ->
    fprintf
      ppf
      "let %a %a in %a"
      pp_rec_flag
      rec_flag
      (fun ppf () ->
        fprintf ppf "%a" pp_value_binding vb;
        List.iter vb_l ~f:(fun vb' -> fprintf ppf " and %a" pp_value_binding vb'))
      ()
      pp_expr
      e
  | Efun_application (e1, e2) ->
    let needs_parens = function
      | Econst _ | Evar _ | Ebin_op _ -> false
      | _ -> true
    in
    fprintf
      ppf
      "%a %a"
      (fun ppf e ->
        if needs_parens e then fprintf ppf "(%a)" pp_expr e else pp_expr ppf e)
      e1
      (fun ppf e ->
        if needs_parens e then fprintf ppf "(%a)" pp_expr e else pp_expr ppf e)
      e2
  | Econstraint (e, t) -> fprintf ppf "(%a : %a)" pp_expr e pp_ty t
(* | Efield_access (e, label) -> fprintf ppf "(%a.%a)" pp_expr e pp_label label
  | Erecord (field, fields) ->
    fprintf
      ppf
      "{ %a }"
      (fun ppf () ->
        fprintf ppf "%a" pp_record_field field;
        List.iter fields ~f:(fun field' -> fprintf ppf " ; %a" pp_record_field field'))
      () *)

and precedence = function
  | Ebin_op (op, _, _) -> precedence_bin_op op
  | _ -> 2

and pp_value_binding ppf = function
  | Evalue_binding (pattern, e) -> fprintf ppf "%a = %a" pp_pattern pattern pp_expr e
;;

(* and pp_record_field ppf = function
   | Erecord_field (label, e) -> fprintf ppf "%a = %a" pp_label label pp_expr e
   ;; *)

let pp_structure_item ppf (item : structure_item) =
  match item with
  | SEval e -> fprintf ppf "%a ;;" pp_expr e
  | SValue (rec_flag, vb, vb_l) ->
    fprintf
      ppf
      "let %a %a ;;"
      pp_rec_flag
      rec_flag
      (fun ppf () ->
        fprintf ppf "%a" pp_value_binding vb;
        List.iter vb_l ~f:(fun vb' -> fprintf ppf " and %a" pp_value_binding vb'))
      ()
;;

(* | SType (name, field_decl, field_decls) ->
    fprintf
      ppf
      "type %s = { %a } ;;"
      name
      (fun ppf () ->
        fprintf ppf "%a" pr_field_decl field_decl;
        List.iter field_decls ~f:(fun field_decl' ->
          fprintf ppf " ; %a" pr_field_decl field_decl'))
      () *)

(* and pr_field_decl ppf = function
   | Sfield_decl (label, ty) -> fprintf ppf "%a : %a" pp_label label pp_ty ty *)

let pp_new_line ppf () = fprintf ppf "\n"

let prpr_structure ppf =
  fprintf ppf "%a" (pp_print_list ~pp_sep:pp_new_line pp_structure_item)
;;
