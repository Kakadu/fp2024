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

let print_rec_flag fmt =
  let open Format in
  function
  | Rec -> fprintf fmt "rec"
  | Nonrec -> ()
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
    fprintf fmt "if %a then %a " print_expr cond print_expr then_body;
    (match else_body with
     | Some body -> fprintf fmt "else %a " print_expr body
     | None -> ())
  | Function_def (args, body) ->
    fprintf fmt "fun ";
    print_args fmt args;
    fprintf fmt "-> %a " print_expr body
  | Function_call (func, arg) -> fprintf fmt "%a %a" print_expr func print_expr arg
  | LetIn (rec_flag, name, args, body, in_expr) ->
    (fprintf fmt "let %a " print_rec_flag rec_flag;
     match name with
     | Some (Ident ident) -> fprintf fmt "%s " ident
     | None -> ());
    print_args fmt args;
    fprintf fmt "= %a in %a " print_expr body print_expr in_expr

and print_args fmt args =
  let open Format in
  pp_print_list ~pp_sep:pp_print_space print_expr fmt args
;;

let print_statement fmt =
  let open Format in
  function
  | Let (rec_flag, Ident ident, args, body) ->
    fprintf fmt "let %a %s " print_rec_flag rec_flag ident;
    print_args fmt args;
    fprintf fmt "= %a " print_expr body
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
