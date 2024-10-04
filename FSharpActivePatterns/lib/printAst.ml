(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Printf

let print_bin_op indent bin_op =
  match bin_op with
  | Binary_equal -> printf "%s| Binary Equal\n" (String.make indent '-')
  | Binary_unequal -> printf "%s| Binary Unequal\n" (String.make indent '-')
  | Binary_less -> printf "%s| Binary Less\n" (String.make indent '-')
  | Binary_less_or_equal -> printf "%s| Binary Less Or Equal\n" (String.make indent ' ')
  | Binary_greater -> printf "%s| Binary Greater\n" (String.make indent '-')
  | Binary_greater_or_equal ->
    printf "%s| Binary Greater Or Equal\n" (String.make indent '-')
  | Binary_add -> printf "%s| Binary Add\n" (String.make indent '-')
  | Binary_subtract -> printf "%s| Binary Subtract\n" (String.make indent '-')
  | Binary_multiply -> printf "%s| Binary Multiply\n" (String.make indent '-')
  | Logical_or -> printf "%s| Logical Or\n" (String.make indent '-')
  | Logical_and -> printf "%s| Logical And\n" (String.make indent '-')
  | Binary_divide -> printf "%s| Binary Divide\n" (String.make indent '-')
  | Binary_or_bitwise -> printf "%s| Binary Or Bitwise\n" (String.make indent '-')
  | Binary_xor_bitwise -> printf "%s| Binary Xor Bitwise\n" (String.make indent '-')
  | Binary_and_bitwise -> printf "%s| Binary And Bitwise\n" (String.make indent '-')
;;

let rec print_expr_helper indent expr =
  match expr with
  | Const (Int_lt i) -> printf "%s| Const(Int: %d)\n" (String.make indent '-') i
  | Const (Bool_lt b) -> printf "%s| Const(Bool: %b)\n" (String.make indent '-') b
  | Const (String_lt s) -> printf "%s| Const(String: %S)\n" (String.make indent '-') s
  | Const Unit_lt -> printf "%s| Const(Unit)\n" (String.make indent '-')
  | Const Null_lt -> printf "%s| Const(Null)\n" (String.make indent '-')
  | List_expr (_, _) -> printf "LIST_EXPR TODO"
  | Tuple t -> List.iter (print_expr_helper indent) t
  | Match (_, _) -> printf "MATCH TODO"
  | Variable (Ident name) -> printf "%s| Variable(%s)\n" (String.make indent '-') name
  | Bin_expr (op, left, right) ->
    printf "%s| Binary expr(\n" (String.make indent '-');
    print_bin_op indent op;
    print_expr_helper (indent + 2) left;
    print_expr_helper (indent + 2) right
  | If_then_else (cond, then_body, else_body) ->
    printf "%s| If Then Else(\n" (String.make indent '-');
    printf "%sCONDITION\n" (String.make (indent + 2) ' ');
    print_expr_helper (indent + 2) cond;
    printf "%sTHEN BRANCH\n" (String.make (indent + 2) ' ');
    List.iter (print_expr_helper (indent + 4)) then_body;
    printf "%sELSE BRANCH\n" (String.make (indent + 2) ' ');
    (match else_body with
     | Some body -> List.iter (print_expr_helper (indent + 4)) body
     | None -> printf "No else body")
  | Function_def (flag, name, args, body) ->
    printf
      "%s| %s Function(%s):\n"
      (String.make indent '-')
      (match flag with
       | Nonrec -> ""
       | Rec -> "Rec")
      (match name with
       | Some n -> n
       | None -> "Anonymous");
    printf "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr_helper (indent + 4)) args;
    printf "%sBODY\n" (String.make (indent + 2) ' ');
    print_expr_helper (indent + 4) body
  | Function_call (func, args) ->
    printf "%s| Function Call:\n" (String.make indent '-');
    printf "%sFUNCTION\n" (String.make (indent + 2) ' ');
    print_expr_helper (indent + 2) func;
    printf "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr_helper (indent + 2)) args
  | LetIn (Ident name, value, inner_expr) ->
    printf "%s | LetIn %s =\n" (String.make indent '-') name;
    printf "%sVALUE\n" (String.make (indent + 2) ' ');
    print_expr_helper (indent + 2) value;
    printf "%sINNER EXPRESSION\n" (String.make (indent + 2) ' ');
    print_expr_helper (indent + 2) inner_expr
;;

let print_expr expr = print_expr_helper 0 expr

let print_statement indent statement =
  match statement with
  | Let (Ident name, value) ->
    printf "%s| Let %s =\n" (String.make indent '-') name;
    print_expr_helper (indent + 2) value
  | ActivePattern _ -> printf "ACTIVE PATTERN TODO"
;;

let print_construction = function
  | Expr e -> print_expr e
  | Statement s -> print_statement 0 s
;;
