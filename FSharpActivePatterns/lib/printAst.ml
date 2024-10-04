(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Printf

let print_bin_op indent = function
  | Binary_equal -> printf "%s| Binary Equal\n" (String.make indent '-')
  | Binary_unequal -> printf "%s| Binary Unequal\n" (String.make indent '-')
  | Binary_less -> printf "%s| Binary Less\n" (String.make indent '-')
  | Binary_less_or_equal -> printf "%s| Binary Less Or Equal\n" (String.make indent '-')
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

let rec print_pattern indent = function
  | Wild -> printf "%s| Wild\n" (String.make indent '-')
  | PCons (head, tail) ->
    printf "%s| PCons:\n" (String.make indent '-');
    printf "%sHead:\n" (String.make (indent + 2) '-');
    print_pattern (indent + 4) head;
    printf "%sTail:\n" (String.make (indent + 2) '-');
    print_pattern (indent + 4) tail
  | PTuple tuple ->
    printf "%s| PTuple:\n" (String.make indent '-');
    List.iter (print_pattern (indent + 2)) tuple
  | PConst literal ->
    printf "%s| PConst:\n" (String.make indent '-');
    (match literal with
     | Int_lt i -> printf "%sInt: %d\n" (String.make (indent + 2) '-') i
     | Bool_lt b -> printf "%sBool: %b\n" (String.make (indent + 2) '-') b
     | String_lt s -> printf "%sString: %S\n" (String.make (indent + 2) '-') s
     | Unit_lt -> printf "%sUnit\n" (String.make (indent + 2) '-')
     | Null_lt -> printf "%sNull\n" (String.make (indent + 2) '-'))
  | PVar (Ident name) -> printf "%s| PVar(%s)\n" (String.make indent '-') name
  | Variant variants ->
    printf "%s| Variant:\n" (String.make indent '-');
    List.iter
      (fun (Ident v) -> printf "%s- %s\n" (String.make (indent + 2) '-') v)
      variants
;;

let print_unary_op indent = function
  | Unary_minus -> printf "%s| Unary minus\n" (String.make indent '-')
  | Unary_negative -> printf "%s| Unary negative\n" (String.make indent '-')
;;

let rec print_expr indent expr =
  match expr with
  | Const (Int_lt i) -> printf "%s| Const(Int: %d)\n" (String.make indent '-') i
  | Const (Bool_lt b) -> printf "%s| Const(Bool: %b)\n" (String.make indent '-') b
  | Const (String_lt s) -> printf "%s| Const(String: %S)\n" (String.make indent '-') s
  | Const Unit_lt -> printf "%s| Const(Unit)\n" (String.make indent '-')
  | Const Null_lt -> printf "%s| Const(Null)\n" (String.make indent '-')
  | List_expr (expr1, expr2) ->
    printf "%s| List expr:\n" (String.make indent '-');
    print_expr (indent + 2) expr1;
    print_expr (indent + 2) expr2
  | Tuple t -> List.iter (print_expr indent) t
  | Match (value, patterns) ->
    printf "%s| Match:\n" (String.make indent '-');
    print_expr (indent + 2) value;
    List.iter
      (fun (pat, expr) ->
        printf "%s| Pattern:\n" (String.make (indent + 2) '-');
        print_pattern (indent + 4) pat;
        printf "%s| Inner expr:\n" (String.make (indent + 2) '-');
        print_expr (indent + 4) expr)
      patterns
  | Variable (Ident name) -> printf "%s| Variable(%s)\n" (String.make indent '-') name
  | Unary_expr (op, expr) ->
    printf "%s| Unary expr(\n" (String.make indent '-');
    print_unary_op indent op;
    print_expr (indent + 2) expr
  | Bin_expr (op, left, right) ->
    printf "%s| Binary expr(\n" (String.make indent '-');
    print_bin_op indent op;
    print_expr (indent + 2) left;
    print_expr (indent + 2) right
  | If_then_else (cond, then_body, else_body) ->
    printf "%s| If Then Else(\n" (String.make indent '-');
    printf "%sCONDITION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) cond;
    printf "%sTHEN BRANCH\n" (String.make (indent + 2) ' ');
    print_expr (indent + 4) then_body;
    printf "%sELSE BRANCH\n" (String.make (indent + 2) ' ');
    (match else_body with
     | Some body -> print_expr (indent + 4) body
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
    List.iter (print_expr (indent + 4)) args;
    printf "%sBODY\n" (String.make (indent + 2) ' ');
    print_expr (indent + 4) body
  | Function_call (func, args) ->
    printf "%s| Function Call:\n" (String.make indent '-');
    printf "%sFUNCTION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) func;
    printf "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 2)) args
  | LetIn (Ident name, value, inner_expr) ->
    printf "%s | LetIn %s =\n" (String.make indent '-') name;
    printf "%sVALUE\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) value;
    printf "%sINNER EXPRESSION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) inner_expr
;;

let print_statement indent = function
  | Let (Ident name, value) ->
    printf "%s| Let %s =\n" (String.make indent '-') name;
    print_expr (indent + 2) value
  | ActivePattern (patterns, expr) ->
    printf "%s| ActivePattern:\n" (String.make indent '-');
    List.iter
      (fun (Ident param) -> printf "%s- %s\n" (String.make (indent + 2) '-') param)
      patterns;
    print_expr (indent + 2) expr
;;

let print_construction = function
  | Expr e -> print_expr 0 e
  | Statement s -> print_statement 0 s
;;
