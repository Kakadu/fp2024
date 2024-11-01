(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common
open Expressions
open Patterns

let parse_structure_item_expr =
  (skip_token "do" <|> skip_ws)
  *>
  let* expr = parse_expr in
  return (Str_item_eval expr)
;;

let parse_structure_item_def =
  skip_token "let"
  *>
  let* rec_flag = option Nonrecursive (string "rec" *> skip_ws *> return Recursive) in
  let* bindings = sep_by1 (skip_token "and") (parse_single_binding parse_expr) in
  return (Str_item_def (rec_flag, bindings))
;;

let parse_structure_item = choice [ parse_structure_item_expr; parse_structure_item_def ]
let parse_program = sep_by (skip_token ";;") parse_structure_item
