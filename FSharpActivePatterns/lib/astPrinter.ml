[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format
open Ast
open TypesPp

let print_bin_op indent fmt = function
  | Binary_equal -> fprintf fmt "%s| Binary Equal\n" (String.make indent '-')
  | Binary_unequal -> fprintf fmt "%s| Binary Unequal\n" (String.make indent '-')
  | Binary_less -> fprintf fmt "%s| Binary Less\n" (String.make indent '-')
  | Binary_less_or_equal ->
    fprintf fmt "%s| Binary Less Or Equal\n" (String.make indent '-')
  | Binary_greater -> fprintf fmt "%s| Binary Greater\n" (String.make indent '-')
  | Binary_greater_or_equal ->
    fprintf fmt "%s| Binary Greater Or Equal\n" (String.make indent '-')
  | Binary_add -> fprintf fmt "%s| Binary Add\n" (String.make indent '-')
  | Binary_subtract -> fprintf fmt "%s| Binary Subtract\n" (String.make indent '-')
  | Binary_multiply -> fprintf fmt "%s| Binary Multiply\n" (String.make indent '-')
  | Logical_or -> fprintf fmt "%s| Logical Or\n" (String.make indent '-')
  | Logical_and -> fprintf fmt "%s| Logical And\n" (String.make indent '-')
  | Binary_divide -> fprintf fmt "%s| Binary Divide\n" (String.make indent '-')
  | Binary_or_bitwise -> fprintf fmt "%s| Binary Or Bitwise\n" (String.make indent '-')
  | Binary_xor_bitwise -> fprintf fmt "%s| Binary Xor Bitwise\n" (String.make indent '-')
  | Binary_and_bitwise -> fprintf fmt "%s| Binary And Bitwise\n" (String.make indent '-')
  | Binary_cons -> fprintf fmt "%s| Binary Cons\n" (String.make indent '-')
;;

let rec print_pattern indent fmt = function
  | Wild -> fprintf fmt "%s| Wild\n" (String.make indent '-')
  | PList l ->
    fprintf fmt "%s| PList:\n" (String.make indent '-');
    List.iter (print_pattern (indent + 2) fmt) l
  | PTuple (p1, p2, rest) ->
    fprintf fmt "%s| PTuple:\n" (String.make indent '-');
    List.iter (print_pattern (indent + 2) fmt) (p1 :: p2 :: rest)
  | PConst literal ->
    fprintf fmt "%s| PConst:\n" (String.make indent '-');
    (match literal with
     | Int_lt i -> fprintf fmt "%sInt: %d\n" (String.make (indent + 2) '-') i
     | Bool_lt b -> fprintf fmt "%sBool: %b\n" (String.make (indent + 2) '-') b
     | String_lt s -> fprintf fmt "%sString: %S\n" (String.make (indent + 2) '-') s
     | Unit_lt -> fprintf fmt "%sUnit\n" (String.make (indent + 2) '-'))
  | PCons (l, r) ->
    fprintf fmt "%s| PCons:\n" (String.make indent '-');
    print_pattern (indent + 2) fmt l;
    print_pattern (indent + 2) fmt r
  | PVar (Ident name) -> fprintf fmt "%s| PVar(%s)\n" (String.make indent '-') name
  | POption p ->
    fprintf fmt "%s| POption " (String.make indent '-');
    (match p with
     | None -> fprintf fmt "None\n"
     | Some p ->
       fprintf fmt "Some:\n";
       print_pattern (indent + 2) fmt p)
  | PConstraint (p, t) ->
    fprintf fmt "%s| PConstraint\n" (String.make indent ' ');
    fprintf fmt "%sPattern:\n" (String.make (indent + 2) ' ');
    print_pattern (indent + 2) fmt p;
    fprintf fmt "%sType:\n" (String.make (indent + 2) ' ');
    fprintf fmt "%s| %a\n" (String.make (indent + 2) '-') pp_typ t
;;

let print_unary_op indent fmt = function
  | Unary_minus -> fprintf fmt "%s| Unary minus\n" (String.make indent '-')
  | Unary_not -> fprintf fmt "%s| Unary negative\n" (String.make indent '-')
;;

let rec print_let_bind indent fmt = function
  | Let_bind (name, args, body) ->
    fprintf fmt "%s| Let_bind:\n" (String.make indent '-');
    fprintf fmt "%sNAME:\n" (String.make (indent + 4) ' ');
    print_pattern (indent + 4) fmt name;
    fprintf fmt "%sARGS:\n" (String.make (indent + 4) ' ');
    List.iter (fun arg -> print_pattern (indent + 2) fmt arg) args;
    fprintf fmt "%sBODY:\n" (String.make (indent + 4) ' ');
    print_expr (indent + 2) fmt body

and print_expr indent fmt expr =
  match expr with
  | Const (Int_lt i) -> fprintf fmt "%s| Const(Int: %d)\n" (String.make indent '-') i
  | Const (Bool_lt b) -> fprintf fmt "%s| Const(Bool: %b)\n" (String.make indent '-') b
  | Const (String_lt s) ->
    fprintf fmt "%s| Const(String: %S)\n" (String.make indent '-') s
  | Const Unit_lt -> fprintf fmt "%s| Const(Unit)\n" (String.make indent '-')
  | List l ->
    fprintf fmt "%s| PList:\n" (String.make indent '-');
    List.iter (print_expr (indent + 2) fmt) l
  | Tuple (e1, e2, rest) ->
    fprintf fmt "%s| Tuple:\n" (String.make indent '-');
    List.iter (print_expr (indent + 2) fmt) (e1 :: e2 :: rest)
  | Function ((pat1, expr1), cases) ->
    fprintf fmt "%s| Function:\n" (String.make indent '-');
    List.iter
      (fun (pat, expr) ->
        fprintf fmt "%s| Pattern:\n" (String.make (indent + 2) '-');
        print_pattern (indent + 4) fmt pat;
        fprintf fmt "%s| Case expr:\n" (String.make (indent + 2) '-');
        print_expr (indent + 4) fmt expr)
      ((pat1, expr1) :: cases)
  | Match (value, (pat1, expr1), cases) ->
    fprintf fmt "%s| Match:\n" (String.make indent '-');
    fprintf fmt "%s| Value:\n" (String.make (indent + 2) '-');
    print_expr (indent + 4) fmt value;
    List.iter
      (fun (pat, expr) ->
        fprintf fmt "%s| Pattern:\n" (String.make (indent + 2) '-');
        print_pattern (indent + 4) fmt pat;
        fprintf fmt "%s| Case expr:\n" (String.make (indent + 2) '-');
        print_expr (indent + 4) fmt expr)
      ((pat1, expr1) :: cases)
  | Variable (Ident name) ->
    fprintf fmt "%s| Variable(%s)\n" (String.make indent '-') name
  | Unary_expr (op, expr) ->
    fprintf fmt "%s| Unary expr(\n" (String.make indent '-');
    print_unary_op indent fmt op;
    print_expr (indent + 2) fmt expr
  | Bin_expr (op, left, right) ->
    fprintf fmt "%s| Binary expr(\n" (String.make indent '-');
    print_bin_op indent fmt op;
    print_expr (indent + 2) fmt left;
    print_expr (indent + 2) fmt right
  | If_then_else (cond, then_body, else_body) ->
    fprintf fmt "%s| If Then Else(\n" (String.make indent '-');
    fprintf fmt "%sCONDITION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt cond;
    fprintf fmt "%sTHEN BRANCH\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt then_body;
    fprintf fmt "%sELSE BRANCH\n" (String.make (indent + 2) ' ');
    (match else_body with
     | Some body -> print_expr (indent + 2) fmt body
     | None -> fprintf fmt "%s| No else body\n" (String.make (indent + 2) '-'))
  | Lambda (arg1, args, body) ->
    fprintf fmt "%s| Lambda:\n" (String.make indent '-');
    fprintf fmt "%sARGS\n" (String.make (indent + 2) ' ');
    List.iter (fun pat -> print_pattern (indent + 4) fmt pat) (arg1 :: args);
    fprintf fmt "%sBODY\n" (String.make (indent + 2) ' ');
    print_expr (indent + 4) fmt body
  | Apply (func, arg) ->
    fprintf fmt "%s| Apply:\n" (String.make indent '-');
    fprintf fmt "%sFUNCTION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt func;
    fprintf fmt "%sARGS\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt arg
  | LetIn (rec_flag, let_bind, let_bind_list, inner_e) ->
    fprintf
      fmt
      "%s| %sLetIn=\n"
      (String.make indent '-')
      (match rec_flag with
       | Nonrec -> ""
       | Rec -> "Rec ");
    fprintf fmt "%sLet_binds\n" (String.make (indent + 2) ' ');
    List.iter (print_let_bind (indent + 2) fmt) (let_bind :: let_bind_list);
    fprintf fmt "%sINNER_EXPRESSION\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt inner_e
  | Option e ->
    (match e with
     | None -> fprintf fmt "%s| Option: None\n" (String.make indent '-')
     | Some e ->
       fprintf fmt "%s| Option: Some\n" (String.make indent '-');
       print_expr (indent + 2) fmt e)
  | EConstraint (e, t) ->
    fprintf fmt "%s| EConstraint\n" (String.make indent ' ');
    fprintf fmt "%sExpr:\n" (String.make (indent + 2) ' ');
    print_expr (indent + 2) fmt e;
    fprintf fmt "%sType:\n" (String.make (indent + 2) ' ');
    fprintf fmt "%s| %a\n" (String.make (indent + 2) '-') pp_typ t
;;

let print_statement indent fmt = function
  | Let (rec_flag, let_bind, let_bind_list) ->
    fprintf
      fmt
      "%s |%s Let:\n"
      (String.make indent '-')
      (match rec_flag with
       | Nonrec -> ""
       | Rec -> "Rec ");
    fprintf fmt "%s Let_binds\n" (String.make (indent + 2) ' ');
    List.iter (print_let_bind (indent + 2) fmt) (let_bind :: let_bind_list)
;;

(* | ActivePattern (patterns, expr) ->
   fprintf fmt "%s| ActivePattern:\n" (String.make indent '-');
   List.iter
   (fun (Ident (param, _)) ->
   fprintf fmt "%s- %s\n" (String.make (indent + 2) '-') param)
   patterns;
   print_expr (indent + 2) fmt expr *)

let print_construction fmt = function
  | Expr e -> print_expr 0 fmt e
  | Statement s -> print_statement 0 fmt s
;;

let print_p_res fmt = function
  | Ok ast -> print_construction fmt ast
  | Error e -> fprintf fmt "%s\n" e
;;
