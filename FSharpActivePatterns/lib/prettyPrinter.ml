(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format
open TypesPp

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
  | Binary_cons -> fprintf fmt "::"
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
  | PList l ->
    fprintf fmt "[";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_pattern fmt l;
    fprintf fmt "]"
  | PCons (l, r) -> fprintf fmt "(%a) :: (%a) " pp_pattern l pp_pattern r
  | PTuple (p1, p2, rest) ->
    fprintf fmt "(";
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ", ")
      pp_pattern
      fmt
      (p1 :: p2 :: rest);
    fprintf fmt ")"
  | PConst literal -> fprintf fmt "%a " pp_expr (Const literal)
  | PVar (Ident name) -> fprintf fmt "%s " name
  | POption p ->
    (match p with
     | None -> fprintf fmt "None "
     | Some p -> fprintf fmt "Some (%a) " pp_pattern p)
  | PConstraint (p, t) -> fprintf fmt "(%a : %a) " pp_pattern p pp_typ t

and pp_expr fmt expr =
  match expr with
  | Const (Int_lt i) -> fprintf fmt "%d " i
  | Const (Bool_lt b) -> fprintf fmt "%b " b
  | Const (String_lt s) -> fprintf fmt "%S" s
  | Const Unit_lt -> fprintf fmt "() "
  | List l ->
    fprintf fmt "[";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_expr fmt l;
    fprintf fmt "]"
  | Tuple (e1, e2, rest) ->
    fprintf fmt "(";
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ", ")
      pp_parens_expr
      fmt
      (e1 :: e2 :: rest);
    fprintf fmt ")"
  | Function ((pat1, expr1), cases) ->
    fprintf fmt "function ";
    List.iter
      (fun (pat, expr) -> fprintf fmt "| %a -> (%a) \n" pp_pattern pat pp_expr expr)
      ((pat1, expr1) :: cases)
  | Match (value, (pat1, expr1), cases) ->
    fprintf fmt "match (%a) with \n" pp_expr value;
    List.iter
      (fun (pat, expr) -> fprintf fmt "| %a -> (%a) \n" pp_pattern pat pp_expr expr)
      ((pat1, expr1) :: cases)
  | Variable (Ident name) -> fprintf fmt "%s " name
  | Unary_expr (op, expr) -> fprintf fmt "%a (%a)" pp_unary_op op pp_expr expr
  | Bin_expr (op, left, right) ->
    fprintf fmt "(%a) %a (%a)" pp_expr left pp_bin_op op pp_expr right
  | If_then_else (cond, then_body, else_body) ->
    fprintf fmt "if (%a) then (%a) " pp_expr cond pp_expr then_body;
    (match else_body with
     | Some body -> fprintf fmt "else %a " pp_expr body
     | None -> ())
  | Lambda (arg1, args, body) ->
    fprintf fmt "fun ";
    List.iter (fun pat -> fprintf fmt "(%a) " pp_pattern pat) (arg1 :: args);
    fprintf fmt "-> %a " pp_expr body
  | Apply (Apply (Variable (Ident op), left), right)
    when String.for_all (fun c -> String.contains "!$%&*+-./:<=>?@^|~" c) op ->
    fprintf fmt "(%a) %s (%a)" pp_expr left op pp_expr right
  | Apply (func, arg) -> fprintf fmt "(%a) %a" pp_expr func pp_expr arg
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
  | EConstraint (e, t) -> fprintf fmt "(%a : %a) " pp_expr e pp_typ t

and pp_args fmt args =
  let open Format in
  pp_print_list
    ~pp_sep:pp_print_space
    (fun fmt arg -> fprintf fmt "%a" pp_pattern arg)
    fmt
    args

and pp_let_bind fmt = function
  | Let_bind (name, args, body) ->
    fprintf fmt "%a %a = %a " pp_pattern name pp_args args pp_expr body

and pp_parens_expr fmt expr = fprintf fmt "(%a)" pp_expr expr

let pp_statement fmt = function
  | Let (rec_flag, let_bind, let_bind_list) ->
    fprintf fmt "let %a " pp_rec_flag rec_flag;
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "\n\nand ")
      pp_let_bind
      fmt
      (let_bind :: let_bind_list)
;;

let pp_construction fmt = function
  | Expr e -> fprintf fmt "%a\n" pp_expr e
  | Statement s -> fprintf fmt "%a\n" pp_statement s
;;

let pp_p_res fmt = function
  | Some c -> pp_construction fmt c
  | None -> fprintf fmt "Error occured\n"
;;
