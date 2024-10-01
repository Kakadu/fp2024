(** Copyright 2024, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open AST

let rec print_expr indent expr =
  let open Printf in
  match expr with
  | Const (Int_lt i) -> printf "%s| Const(Int: %d)\n" (String.make indent '-') i
  | Const (Bool_lt b) -> printf "%s| Const(Bool: %b)\n" (String.make indent '-') b
  | Const (String_lt s) -> printf "%s| Const(String: \"%s\")\n" (String.make indent '-') s
  | Const Unit_lt -> printf "%s| Const(Unit)\n" (String.make indent '-')
  | Tuple list -> List.iter (print_expr indent) list
  | Variable (Ident name) -> printf "%s| Variable(%s)\n" (String.make indent '-') name
  | Bin_expr (op, left, right) ->
    printf "%s| Binary expr(\n" (String.make indent '-');
    print_bin_op indent op;
    print_expr (indent + 2) left;
    print_expr (indent + 2) right
  | If_then_else (cond, then_body, else_body) ->
    printf "%s| If Then Else(\n" (String.make indent '-');
    printf "%sCONDITION\n" (String.make indent ' ');
    print_expr (indent + 2) cond;
    printf "%sTHEN BRANCH\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 4)) then_body;
    printf "%sELSE BRANCH\n" (String.make (indent + 2) ' ');
    (match else_body with
     | Some body -> List.iter (print_expr (indent + 4)) body
     | None -> printf "No else body")
  | Function_def (flag, name, args, body) ->
    printf
      "%s| %s Function(%s):\n"
      (String.make indent '-')
      (match flag with
       | None -> ""
       | Rec -> "Rec")
      (match name with
       | Some n -> n
       | None -> "Anonymous");
    printf "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 4)) args;
    printf "%sBODY\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 4)) body
  | Function_dec (flag, name, args) ->
    printf
      "%s| %s Function Declaration(%s):\n"
      (String.make indent '-')
      (match flag with
       | None -> ""
       | Rec -> "Rec")
      name;
    printf "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 2)) args
  | Function_call (name, args) ->
    printf "%s| Function Call(%s):\n" (String.make indent '-') name;
    printf "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (print_expr (indent + 2)) args
  | Let (Ident name, value) ->
    printf "%s| Let %s =\n" (String.make indent '-') name;
    print_expr (indent + 2) value

and print_bin_op indent bin_op =
  let open Printf in
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
