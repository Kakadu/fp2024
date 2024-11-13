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

let parse_expr_ident = parse_ident >>| fun i -> Expr_ident_or_op i
let parse_expr_const = parse_const >>| fun c -> Expr_const c

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

let chainl parse_expr parse_bin_op =
  let rec wrap expr1 =
    let* app_binop = parse_bin_op in
    let* expr2 = skip_ws *> parse_expr in
    let binop = app_binop expr1 expr2 in
    wrap binop <|> return binop
  in
  skip_ws *> parse_expr >>= fun init -> wrap init
;;

let rec chainr e op =
  e >>= fun a -> op >>= (fun f -> chainr (skip_ws *> e) op >>| f a) <|> return a
;;

let parse_bin_op_as_app bin_op =
  skip_ws
  *> string bin_op
  *> return (fun e1 e2 -> Expr_apply (Expr_apply (Expr_ident_or_op bin_op, e1), e2))
;;

let parse_ws_as_app =
  skip_ws1
  *>
  let* char = peek_char in
  match char with
  | None -> fail "cannot apply function to end of input"
  | Some x when is_ident_start_char x || Char.equal x '(' ->
    return (fun e1 e2 -> Expr_apply (e1, e2))
  | _ -> fail "cannot apply function to non-identificator"
;;

let parse_expr_app parse_expr =
  let app = chainl parse_expr parse_ws_as_app <|> parse_expr in
  let app = chainl app (parse_bin_op_as_app "*" <|> parse_bin_op_as_app "/") <|> app in
  let app = chainl app (parse_bin_op_as_app "+" <|> parse_bin_op_as_app "-") <|> app in
  let app =
    chainl
      app
      (parse_bin_op_as_app "<="
       <|> parse_bin_op_as_app "<"
       <|> parse_bin_op_as_app ">="
       <|> parse_bin_op_as_app ">")
    <|> app
  in
  let app = chainr app (parse_bin_op_as_app "||" <|> parse_bin_op_as_app "&&") <|> app in
  app
;;

let parse_expr_lambda parse_expr =
  skip_token "fun"
  *>
  let* args = many1 parse_pat in
  skip_token "->"
  *>
  let* expr = parse_expr in
  let rec wrap = function
    | h :: tl -> Expr_fun (h, wrap tl)
    | [] -> expr
  in
  return (wrap args)
;;

let parse_binding_val parse_expr =
  let* name = parse_pat in
  skip_token "="
  *>
  let* expr = parse_expr in
  return (name, expr)
;;

let parse_binding_fun parse_expr =
  let* name = parse_pat_ident (* ops are not yet supported *) in
  let* args = many1 (skip_ws *> parse_pat) in
  skip_token "="
  *>
  let* expr = parse_expr in
  let rec wrap = function
    | h :: tl -> Expr_fun (h, wrap tl)
    | [] -> expr
  in
  return (name, wrap args)
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

let parse_expr =
  fix (fun parse_expr ->
    let expr =
      choice
        [ parse_expr_paren parse_expr
        ; parse_expr_ite parse_expr
        ; parse_expr_lambda parse_expr
        ; parse_expr_let parse_expr
        ; parse_expr_const
        ; parse_expr_ident
        ]
    in
    let expr = parse_expr_app expr <|> expr in
    skip_ws *> expr <* skip_ws)
;;
