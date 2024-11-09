(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Printf

let print_bin_op fmt =
  let open Format in
  function
  | Binary_equal -> fprintf fmt "= "
  | Binary_unequal -> fprintf fmt "<> "
  | Binary_less -> fprintf fmt "< "
  | Binary_less_or_equal -> fprintf fmt "<= "
  | Binary_greater -> fprintf fmt "> "
  | Binary_greater_or_equal -> fprintf fmt ">= "
  | Binary_add -> fprintf fmt "+ "
  | Binary_subtract -> fprintf fmt "- "
  | Binary_multiply -> fprintf fmt "* "
  | Logical_or -> fprintf fmt "|| "
  | Logical_and -> fprintf fmt "&& "
  | Binary_divide -> fprintf fmt "/ "
  | Binary_or_bitwise -> fprintf fmt "||| "
  | Binary_xor_bitwise -> fprintf fmt "^^^ "
  | Binary_and_bitwise -> fprintf fmt "&&& "
;;

let print_unary_op fmt =
  let open Format in
  function
  | Unary_minus -> fprintf fmt "-"
  | Unary_not -> fprintf fmt "not "
;;

let rec print_pattern fmt =
  let open Format in
  function
  | Wild -> fprintf fmt "_ "
  | PCons (hd, tl) -> fprintf fmt "%a :: %a" print_pattern hd print_pattern tl
  | PTuple tuple -> fprintf fmt "TUPLE PAT WIP"
  | PConst literal -> fprintf fmt "%a" print_expr (Const literal)
  | PVar (Ident name) -> fprintf fmt "%s " name
  | Variant variants -> fprintf fmt "VARIANTS PAT WIP"

and print_expr fmt expr =
  let open Format in
  match expr with
  | Const (Int_lt i) -> fprintf fmt "%d " i
  | Const (Bool_lt b) -> fprintf fmt "%b " b
  | Const (String_lt s) -> fprintf fmt "%S " s
  | Const Unit_lt -> fprintf fmt "() "
  | List_expr (expr1, expr2) -> fprintf fmt "[%a; %a]" print_expr expr1 print_expr expr2
  | Tuple t -> fprintf fmt "TUPLE WIP"
  | Match (value, patterns) -> fprintf fmt "MATCH WIP"
  | Variable (Ident name) -> fprintf fmt "%s " name
  | Unary_expr (op, expr) -> fprintf fmt "%a %a" print_unary_op op print_expr expr
  | Bin_expr (op, left, right) ->
    fprintf fmt "(%a) %a (%a)" print_expr left print_bin_op op print_expr right
  | If_then_else (cond, then_body, else_body) ->
    let begin_if = asprintf "if %a then %a " print_expr cond print_expr then_body in
    (match else_body with
     | Some body -> fprintf fmt "%s else %a" begin_if print_expr body
     | None -> fprintf fmt "%s" begin_if)
  | Function_def (args, body) -> fprintf fmt "func TODO"
  | Function_call (func, arg) -> fprintf fmt "func_appl TODO"
  | LetIn (rec_flag, name, args, body, in_expr) -> fprintf fmt "LetIn TODO"
;;

let print_statement fmt =
  let open Format in
  function
  | Let (rec_flag, Ident name, args, body) -> fprintf fmt "Let TODO"
  | ActivePattern (patterns, expr) -> fprintf fmt "Active pattern TODO"
;;

let print_construction fmt =
  let open Format in
  function
  | Expr e -> fprintf fmt "%a\n" print_expr e
  | Statement s -> fprintf fmt "%a\n" print_statement s
;;

let print_p_res fmt =
  let open Format in
  function
  | Some c -> print_construction fmt c
  | None -> fprintf fmt "Error occured\n"
;;
