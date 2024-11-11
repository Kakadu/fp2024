(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Printf
open Format

let pp_bin_op fmt = function
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

let pp_unary_op fmt = function
  | Unary_minus -> fprintf fmt "-"
  | Unary_not -> fprintf fmt "not "
;;

let pp_rec_flag fmt = function
  | Rec -> fprintf fmt "rec"
  | Nonrec -> ()
;;

let rec pp_pattern fmt = function
  | Wild -> fprintf fmt "_ "
  | PCons (hd, tl) -> fprintf fmt "%a :: %a" pp_pattern hd pp_pattern tl
  | PTuple tuple -> fprintf fmt "TUPLE PAT WIP"
  | PConst literal -> fprintf fmt "%a" pp_expr (Const literal)
  | PVar (Ident (name, _)) -> fprintf fmt "%s " name
  | Variant variants -> fprintf fmt "VARIANTS PAT WIP"

and pp_expr fmt expr =
  match expr with
  | Const (Int_lt i) -> fprintf fmt "%d " i
  | Const (Bool_lt b) -> fprintf fmt "%b " b
  | Const (String_lt s) -> fprintf fmt "%S " s
  | Const Unit_lt -> fprintf fmt "() "
  | Cons_list (hd, tl) -> fprintf fmt "LIST WIP"
  | Empty_list -> fprintf fmt "[] "
  | Tuple (e1, e2, rest) ->
    fprintf fmt "(";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr fmt (e1 :: e2 :: rest);
    fprintf fmt ")"
  | Match (value, patterns) -> fprintf fmt "MATCH WIP"
  | Variable (Ident (name, _)) -> fprintf fmt "%s " name
  | Unary_expr (op, expr) -> fprintf fmt "%a (%a)" pp_unary_op op pp_expr expr
  | Bin_expr (op, left, right) ->
    fprintf fmt "(%a) %a (%a)" pp_expr left pp_bin_op op pp_expr right
  | If_then_else (cond, then_body, else_body) ->
    fprintf fmt "if (%a) then (%a) " pp_expr cond pp_expr then_body;
    (match else_body with
     | Some body -> fprintf fmt "else %a " pp_expr body
     | None -> ())
  | Function_def (args, body) ->
    fprintf fmt "fun ";
    pp_args fmt args;
    fprintf fmt "-> %a " pp_expr body
  | Function_call (func, arg) -> fprintf fmt "%a (%a)" pp_expr func pp_expr arg
  | LetIn (rec_flag, let_bind, let_bind_list, in_expr) ->
    fprintf fmt "let %a " pp_rec_flag rec_flag;
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "\n\nand ")
      pp_let_bind
      fmt
      (let_bind :: let_bind_list);
    fprintf fmt "in\n";
    fprintf fmt "%a " pp_expr in_expr
  | Option e ->
    (match e with
     | None -> fprintf fmt "None "
     | Some e -> fprintf fmt "Some (%a)" pp_expr e)

and pp_args fmt args =
  let open Format in
  pp_print_list
    ~pp_sep:pp_print_space
    (fun fmt name -> fprintf fmt "%s" name)
    fmt
    (List.map
       (function
         | Ident (s, _) -> s)
       args)

and pp_let_bind fmt = function
  | Let_bind (Ident (name, _), args, body) ->
    fprintf fmt "%s %a = %a " name pp_args args pp_expr body
;;

let pp_statement fmt = function
  | Let (rec_flag, let_bind, let_bind_list) ->
    fprintf fmt "let %a " pp_rec_flag rec_flag;
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "\n\nand ")
      pp_let_bind
      fmt
      (let_bind :: let_bind_list)
  | ActivePattern (patterns, expr) -> fprintf fmt "Active pattern TODO"
;;

let pp_construction fmt = function
  | Expr e -> fprintf fmt "%a\n" pp_expr e
  | Statement s -> fprintf fmt "%a\n" pp_statement s
;;

let pp_p_res fmt = function
  | Some c -> pp_construction fmt c
  | None -> fprintf fmt "Error occured\n"
;;
