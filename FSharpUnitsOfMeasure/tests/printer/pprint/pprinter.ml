(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Format

let pprint_ident ppf ident = fprintf ppf "%s" ident
let pprint_sep_star ppf () = fprintf ppf " * "
let pprint_sep_colon ppf () = fprintf ppf "; "
let pprint_sep_comma ppf () = fprintf ppf ", "

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
    fprintf ppf "%a" (pp_print_list pprint_type ~pp_sep:pprint_sep_star) list
;;

let rec pprint_pat ppf = function
  | Pattern_ident p -> fprintf ppf "%a" pprint_ident p
  | Pattern_const p -> fprintf ppf "%a" pprint_const p
  | Pattern_wild -> fprintf ppf "_"
  | Pattern_typed (p, t) -> fprintf ppf "%a : %a" pprint_pat p pprint_type t
  | Pattern_tuple (p1, p2, rest) ->
    let list = p1 :: p2 :: rest in
    fprintf ppf "(%a)" (pp_print_list pprint_pat ~pp_sep:pprint_sep_comma) list
  | Pattern_list list ->
    fprintf ppf "[%a]" (pp_print_list pprint_pat ~pp_sep:pprint_sep_colon) list
  | Pattern_or (p1, p2) -> fprintf ppf "%a | %a" pprint_pat p1 pprint_pat p2
;;

let rec pprint_expr ppf = function
  | Expr_ident_or_op e -> fprintf ppf "%a" pprint_ident e
  | Expr_const e -> fprintf ppf "%a" pprint_const e
  | Expr_fun (p, e) -> fprintf ppf "fun %a -> %a" pprint_pat p pprint_expr e
  (* I want to sleep, so else branch is thrown away for now *)
  | Expr_ifthenelse (i, t, _) -> fprintf ppf "if %a then %a" pprint_expr i pprint_expr t
  | Expr_apply (e1, e2) -> fprintf ppf "%a %a" pprint_expr e1 pprint_expr e2
  | _ -> fprintf ppf "failed to print expression"
;;
