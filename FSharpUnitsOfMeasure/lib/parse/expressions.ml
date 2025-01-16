(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common
open Patterns
open Constants
open Types

let parse_expr_ident_or_op = parse_ident_or_op >>| fun i -> Expr_ident_or_op i
let parse_expr_const = parse_const_u >>| fun c -> Expr_const c

let parse_expr_ite parse_expr =
  let* cond = skip_ws *> string "if" *> skip_ws *> parse_expr in
  let* branch1 = skip_ws *> string "then" *> skip_ws *> parse_expr in
  let* branch2 =
    option None (skip_ws *> string "else" *> skip_ws *> parse_expr >>| fun e -> Some e)
  in
  return (Expr_ifthenelse (cond, branch1, branch2))
;;

let parse_expr_paren parse_expr =
  string "(" *> skip_ws *> parse_expr <* skip_ws <* string ")"
;;

let parse_prefix_app = skip_ws *> return (fun e1 e2 -> Expr_apply (e1, e2))

let parse_infix_app op =
  skip_ws
  *> string op
  *> return (fun e1 e2 -> Expr_apply (Expr_apply (Expr_ident_or_op op, e1), e2))
;;

(* Need to refactor this with explicit operations priorities *)
let parse_expr_app parse_expr =
  let app = chainl parse_expr parse_prefix_app <|> parse_expr in
  let app =
    chainl
      app
      (parse_infix_app "*."
       <|> parse_infix_app "/."
       <|> parse_infix_app "*"
       <|> parse_infix_app "/")
    <|> app
  in
  let app =
    chainl
      app
      (parse_infix_app "+."
       <|> parse_infix_app "-."
       <|> parse_infix_app "+"
       <|> parse_infix_app "-")
    <|> app
  in
  let app =
    chainl
      app
      (parse_infix_app "<="
       <|> parse_infix_app "<"
       <|> parse_infix_app ">="
       <|> parse_infix_app ">")
    <|> app
  in
  let app = chainr app (parse_infix_app "||" <|> parse_infix_app "&&") <|> app in
  app
;;

(* Makes sense only for + and - *)
let parse_expr_unary_app parse_expr =
  let* op = skip_ws *> string "-" <|> string "+" in
  let* e = parse_expr in
  match op with
  | "+" -> return e
  | _ ->
    (match e with
     | Expr_const (Const_int i) -> return (Expr_const (Const_int (-i)))
     | Expr_const (Const_float f) -> return (Expr_const (Const_float (Float.neg f)))
     | _ -> return (Expr_apply (Expr_ident_or_op op, e)))
;;

let parse_expr_lambda parse_expr =
  skip_token "fun"
  *>
  let* args = many1 parse_pat in
  skip_token "->"
  *>
  let* expr = parse_expr in
  let rec wrap = function
    | h :: tl -> Expr_lam (h, wrap tl)
    | [] -> expr
  in
  return (wrap args)
;;

let parse_binding_val parse_expr =
  let* name = parse_pat in
  skip_token "="
  *>
  let* expr = parse_expr in
  return (Bind (name, expr))
;;

let parse_binding_fun parse_expr =
  let* name = parse_pat_ident_or_op in
  let* args = many1 (skip_ws *> parse_pat) in
  skip_token "="
  *>
  let* expr = parse_expr in
  let rec wrap = function
    | h :: tl -> Expr_lam (h, wrap tl)
    | [] -> expr
  in
  return (Bind (name, wrap args))
;;

let parse_single_binding parse_expr =
  parse_binding_val parse_expr <|> parse_binding_fun parse_expr
;;

let parse_expr_let parse_expr =
  skip_token "let"
  *>
  let* rec_flag = option Nonrecursive (skip_token "rec" *> return Recursive) in
  let* binding_fst = parse_single_binding parse_expr in
  let* binding_rest = many (skip_token "and" *> parse_single_binding parse_expr) in
  skip_token "in"
  *>
  let* last_expr = parse_expr in
  return (Expr_let (rec_flag, binding_fst, binding_rest, last_expr))
;;

(* Parses tuple without parentheses *)
let parse_expr_tuple parse_expr =
  let* tuple_fst = skip_ws *> parse_expr <* skip_ws <* char ',' in
  let* tuple_snd = skip_ws *> parse_expr <* skip_ws in
  let* tuple_rest = many (skip_token "," *> parse_expr) in
  return (Expr_tuple (tuple_fst, tuple_snd, tuple_rest))
;;

let parse_expr_list parse_expr =
  let* list =
    char '[' *> sep_by (char ';') (skip_ws *> parse_expr <* skip_ws) <* char ']'
  in
  return (Expr_list list)
;;

let parse_rules parse_expr =
  let parse_rule =
    let* pat = parse_pat <* skip_token "->" in
    let* expr = parse_expr in
    return (Rule (pat, expr))
  in
  (skip_token "|" <|> skip_ws) *> sep_by1 (skip_token "|") parse_rule
;;

let parse_expr_match parse_expr =
  let* expr = skip_token "match" *> parse_expr <* skip_token "with" in
  let* rules = parse_rules parse_expr in
  match rules with
  | h :: tl -> return (Expr_match (expr, h, tl))
  | _ -> fail "Failed to parse match expression"
;;

let parse_expr_function parse_expr =
  let* rules = skip_token "function" *> parse_rules parse_expr in
  match rules with
  | h :: tl -> return (Expr_function (h, tl))
  | _ -> fail "Failed to parse function expression"
;;

let parse_expr_typed parse_expr =
  let* expr = parse_expr <* skip_token ":" in
  let* core_type = parse_type in
  return (Expr_typed (expr, core_type))
;;

let parse_expr =
  fix (fun parse_expr ->
    let expr =
      choice
        [ parse_expr_paren (parse_expr_typed parse_expr)
        ; parse_expr_paren parse_expr
        ; parse_expr_list parse_expr
        ; parse_expr_ite parse_expr
        ; parse_expr_match parse_expr
        ; parse_expr_function parse_expr
        ; parse_expr_lambda parse_expr
        ; parse_expr_let parse_expr
        ; parse_expr_ident_or_op
        ; parse_expr_const
        ]
    in
    let expr = parse_expr_tuple expr <|> parse_expr_app expr <|> expr in
    let expr = parse_expr_unary_app expr <|> expr in
    skip_ws *> expr <* skip_ws)
;;
