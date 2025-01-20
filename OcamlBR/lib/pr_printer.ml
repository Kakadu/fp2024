(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Stdlib.Format

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
  | PVar (Id (name, None)) -> fprintf ppf "%s" name
  | PVar (Id (name, Some suffix)) -> fprintf ppf "%s%s" name suffix
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
;;

let rec pp_expr ppf = function
  | Econst c -> pp_const ppf c
  | Evar (Id (name, None)) -> fprintf ppf "%s" name
  | Evar (Id (name, Some suffix)) -> fprintf ppf "%s.%s" name suffix
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
  | Ebin_op (op, e1, e2) -> fprintf ppf "(%a %a %a)" pp_expr e1 pp_bin_op op pp_expr e2
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

and pp_value_binding ppf = function
  | Evalue_binding (Id (name, None), e) -> fprintf ppf "%s = %a" name pp_expr e
  | Evalue_binding (Id (name, Some suffix), e) ->
    fprintf ppf "%s%s = %a" name suffix pp_expr e
;;

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

let pp_new_line ppf () = fprintf ppf "\n"

let prpr_structure ppf =
  fprintf ppf "%a" (pp_print_list ~pp_sep:pp_new_line pp_structure_item)
;;
