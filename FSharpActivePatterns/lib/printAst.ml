(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Printf

let print_bin_op indent = function
  | Binary_equal -> printf "= " 
  | Binary_unequal -> printf "<> " 
  | Binary_less -> printf "< "
  | Binary_less_or_equal -> printf "<= " 
  | Binary_greater -> printf "> " 
  | Binary_greater_or_equal -> printf ">= " 
  | Binary_add -> printf "+ "
  | Binary_subtract -> printf "- "
  | Binary_multiply -> printf "* "
  | Logical_or -> printf "|| " 
  | Logical_and -> printf "&& " 
  | Binary_divide -> printf "/ "
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
     | Unit_lt -> printf "%sUnit\n" (String.make (indent + 2) '-'))
  | PVar (Ident name) -> printf "%s| PVar(%s)\n" (String.make indent '-') name
  | Variant variants ->
    printf "%s| Variant:\n" (String.make indent '-');
    List.iter
      (fun (Ident v) -> printf "%s- %s\n" (String.make (indent + 2) '-') v)
      variants
;;

let print_unary_op = function
  | Unary_minus -> printf "-"
  | Unary_not -> printf "not "
;;

let rec print_expr indent expr =
  match expr with
  | Const (Int_lt i) -> printf "%d " i
  | Const (Bool_lt b) -> printf "%s| Const(Bool: %b)\n" (String.make indent '-') b
  | Const (String_lt s) -> printf "%s| Const(String: %S)\n" (String.make indent '-') s
  | Const Unit_lt -> printf "%s| Const(Unit)\n" (String.make indent '-')
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
  | Variable (Ident name) -> printf "%s " name
  | Unary_expr (op, expr) ->
    print_unary_op op;
    print_expr (indent + 2) expr
  | Bin_expr (op, left, right) ->
    print_expr (indent + 2) left;
    print_bin_op indent op;
    print_expr (indent + 2) right
  | If_then_else (cond, then_body, else_body) ->
    printf "if ";
    print_expr (indent + 2) cond;
    printf "then ";
    print_expr (indent + 4) then_body;
    (match else_body with
     | Some body -> (
      printf "else ";
      print_expr (indent + 4) body)
     |None -> printf ";\n")
  | Function_def (args, body) ->
    printf "%s| Func:\n" (String.make indent '-');
    printf "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 4)) args;
    printf "%sBODY\n" (String.make (indent + 2) ' ');
    print_expr (indent + 4) body
  | Function_call (func, arg) ->
    print_expr (indent + 2) func;
    print_expr (indent + 2) arg
  | LetIn (rec_flag, name, args, body, in_expr) ->
    printf
      "let %s %s = "
      (match rec_flag with
       | Nonrec -> ""
       | Rec -> "rec")
      (match name with
       | Some (Ident n) -> n
       | None -> "()");
    List.iter (print_expr (indent + 2)) args;
    print_expr (indent + 2) body;
    printf "in\n";
    print_expr (indent + 2) in_expr;
    printf "\n"
;;

let print_statement indent = function
  | Let (rec_flag, Ident name, args, body) ->
    printf
      "let %s %s = "
      (match rec_flag with
       | Nonrec -> ""
       | Rec -> "rec")
      name;
    List.iter (print_expr (indent + 2)) args;
    print_expr (indent + 2) body
  | ActivePattern (patterns, expr) ->
    printf "%s| ActivePattern:\n" (String.make indent '-');
    List.iter
      (fun (Ident param) -> printf "%s- %s\n" (String.make (indent + 2) '-') param)
      patterns;
    print_expr (indent + 2) expr
;;

let print_construction = function
  | Expr e -> 
    (print_expr 0 e;
    printf ";;\n")
  | Statement s -> 
    (print_statement 0 s;
    printf ";;\n")
;;

let print_p_res = function
  | Some expr -> print_construction expr
  | None -> Printf.printf "Error occured"
;;
