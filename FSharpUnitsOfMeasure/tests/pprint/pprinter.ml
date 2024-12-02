(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

let pprint_ident ppf ident = fprintf ppf "%s" ident
let pprint_sep_star ppf () = fprintf ppf " * "
let pprint_sep_colon ppf () = fprintf ppf "; "
let pprint_sep_double_colon ppf () = fprintf ppf ";;\n\n"
let pprint_sep_comma ppf () = fprintf ppf ", "
let pprint_sep_and ppf () = fprintf ppf " and "
let pprint_sep_or ppf () = fprintf ppf " | "
let pprint_sep_by pprint_alpha sep_by = pp_print_list pprint_alpha ~pp_sep:sep_by

let pprint_const ppf = function
  | Const_bool b -> fprintf ppf "%b" b
  | Const_int i -> fprintf ppf "%d" i
  | Const_char c -> fprintf ppf "%C" c
  | Const_string s -> fprintf ppf "%S" s
  | Const_float f -> fprintf ppf "%f" f
  | Const_unit_of_measure _ -> fprintf ppf "unit of measure"
;;

let rec pprint_type ppf = function
  | Type_ident t -> fprintf ppf "%s" t
  | Type_func (arg, ret) ->
    (match arg with
     | Type_func _ -> fprintf ppf "(%a) -> %a" pprint_type arg pprint_type ret
     | _ -> fprintf ppf "%a -> %a" pprint_type arg pprint_type ret)
  | Type_tuple (t1, t2, rest) ->
    let list = t1 :: t2 :: rest in
    fprintf ppf "%a" (pprint_sep_by pprint_type pprint_sep_star) list
;;

let rec pprint_pat ppf = function
  | Pattern_ident_or_op p -> fprintf ppf "%a" pprint_ident p
  | Pattern_const p -> fprintf ppf "%a" pprint_const p
  | Pattern_wild -> fprintf ppf "_"
  | Pattern_typed (p, t) -> fprintf ppf "%a : %a" pprint_pat p pprint_type t
  | Pattern_tuple (p1, p2, rest) ->
    let list = p1 :: p2 :: rest in
    fprintf ppf "(%a)" (pprint_sep_by pprint_pat pprint_sep_comma) list
  | Pattern_list list ->
    fprintf ppf "[%a]" (pprint_sep_by pprint_pat pprint_sep_colon) list
  | Pattern_or (p1, p2) -> fprintf ppf "%a | %a" pprint_pat p1 pprint_pat p2
;;

(* Prints let f x = y in e as let f = fun x -> y in e *)
let rec pprint_binds ppf =
  let pprint_bind ppf (Bind (p, e)) = fprintf ppf "%a = %a" pprint_pat p pprint_expr e in
  pprint_sep_by pprint_bind pprint_sep_and ppf

and pprint_rules ppf =
  let pprint_rule ppf (Rule (p, e)) = fprintf ppf "%a -> %a" pprint_pat p pprint_expr e in
  pprint_sep_by pprint_rule pprint_sep_or ppf

and pprint_expr ppf = function
  | Expr_const e -> fprintf ppf "%a" pprint_const e
  | Expr_ident_or_op e -> fprintf ppf "%a" pprint_ident e
  | Expr_typed (e, t) -> fprintf ppf "%a : %a" pprint_expr e pprint_type t
  | Expr_tuple (e1, e2, rest) ->
    let list = e1 :: e2 :: rest in
    fprintf ppf "(%a)" (pprint_sep_by pprint_expr pprint_sep_comma) list
  | Expr_list list -> fprintf ppf "[%a]" (pprint_sep_by pprint_expr pprint_sep_colon) list
  | Expr_lam (p, e) -> fprintf ppf "fun %a -> %a" pprint_pat p pprint_expr e
  | Expr_let (flag, b1, rest, e) ->
    let flag = if flag = Recursive then " rec " else " " in
    let list = b1 :: rest in
    fprintf ppf "let%s%a in %a" flag pprint_binds list pprint_expr e
  | Expr_ifthenelse (i, t, e) ->
    (match e with
     | Some e ->
       fprintf ppf "if %a then %a else %a" pprint_expr i pprint_expr t pprint_expr e
     | None -> fprintf ppf "if %a then %a" pprint_expr i pprint_expr t)
  | Expr_apply (e1, e2) ->
    let pprint_expr_paren ppf e =
      match e with
      | Expr_lam _ | Expr_let _ | Expr_ifthenelse _ | Expr_match _  | Expr_function _ | Expr_apply _ ->
        fprintf ppf "(%a)" pprint_expr e
      | _ -> fprintf ppf "%a" pprint_expr e
    in
    fprintf ppf "%a %a" pprint_expr_paren e1 pprint_expr_paren e2
  | Expr_match (e, r1, rest) ->
    let list = r1 :: rest in
    fprintf ppf "match %a with %a" pprint_expr e pprint_rules list
  | Expr_function (r1, rest) ->
    let list = r1 :: rest in
    fprintf ppf "function %a" pprint_rules list
;;

let pprint_struct_item ppf = function
  | Str_item_eval e -> fprintf ppf "%a" pprint_expr e
  | Str_item_def (flag, b1, rest) ->
    let flag = if flag = Recursive then " rec " else " " in
    let list = b1 :: rest in
    fprintf ppf "let%s%a" flag pprint_binds list
  | _ -> fprintf ppf "failed to pprint structure item"
;;

let pprint_program ppf =
  fprintf ppf "%a;;\n" (pprint_sep_by pprint_struct_item pprint_sep_double_colon)
;;
