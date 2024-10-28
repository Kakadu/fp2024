(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common

let parse_expr_ident = skip_ws *> parse_ident >>| fun i -> Expr_ident i
let parse_expr_const = skip_ws *> parse_const >>| fun c -> Expr_const c

let parse_expr_ite parse_expr =
  let* cond = skip_token "if" *> parse_expr <* skip_ws in
  let* branch1 = skip_token "then" *> parse_expr <* skip_ws in
  let* branch2 =
    option None (skip_token "else" *> parse_expr <* skip_ws >>| fun e -> Some e)
  in
  return (Expr_ifthenelse (cond, branch1, branch2))
;;

let parse_expr_simple_no_ws = choice [ parse_expr_ident; parse_expr_const ]
let parse_expr_simple = skip_ws *> parse_expr_simple_no_ws <* skip_ws

let parse_expr_app =
  let* f = skip_ws *> parse_expr_simple_no_ws in
  let* args = many1 (skip_ws1 *> parse_expr_simple_no_ws) in
  skip_ws *> return (List.fold args ~init:f ~f:(fun acc arg -> Expr_apply (acc, arg)))
;;

let parse_expr_paren parse_expr = skip_token "(" *> parse_expr <* skip_token ")"

let parse_expr =
  fix (fun parse_expr ->
    choice
      [ parse_expr_app
      ; parse_expr_ite parse_expr
      ; parse_expr_paren parse_expr
      ; parse_expr_simple
      ])
;;
