(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common

let parse_expr_ident = parse_ident >>| fun i -> Expr_ident i
let parse_expr_const = parse_const >>| fun c -> Expr_const c

let parse_expr_ite parse_expr =
  let* cond = skip_token "if" *> parse_expr in
  let* branch1 = skip_token "then" *> parse_expr in
  let* branch2 = option None (skip_token "else" *> parse_expr >>| fun e -> Some e) in
  return (Expr_ifthenelse (cond, branch1, branch2))
;;

let parse_expr_app parse_expr =
  let* expr1 = parse_expr in
  let* expr2 = parse_expr in
  return (Expr_apply (expr1, expr2))
;;

let parse_expr =
  fix (fun parse_expr ->
    choice
      [ parse_expr_const
      ; parse_expr_ident
      ; parse_expr_ite parse_expr
      ; parse_expr_app parse_expr
      ])
;;
