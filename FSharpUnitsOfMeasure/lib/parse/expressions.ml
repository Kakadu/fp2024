(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common

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

let parse_bin_op_as_app bin_op =
  skip_ws *> string bin_op
  *> return (fun e1 e2 -> Expr_apply (Expr_apply (Expr_ident_or_op bin_op, e1), e2))
;;

let parse_ws_as_app = skip_ws1 *> return (fun e1 e2 -> Expr_apply (e1, e2))

let parse_expr_app parse_expr =
  let parse_op =
    choice
      [ parse_bin_op_as_app "*"
      ; parse_bin_op_as_app "/"
      ; parse_bin_op_as_app "+"
      ; parse_bin_op_as_app "-"
      ; parse_bin_op_as_app "<"
      ; parse_ws_as_app
      ]
  in
  chainl parse_expr parse_op
;;

let parse_expr =
  fix (fun parse_expr ->
    let expr =
      choice
        [ parse_expr_paren parse_expr
        ; parse_expr_ite parse_expr
        ; parse_expr_const
        ; parse_expr_ident
        ]
    in
    let expr = parse_expr_app expr <|> expr in
    skip_ws *> expr <* skip_ws)
;;
