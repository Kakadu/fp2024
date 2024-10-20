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
  skip_token "do"
  *>
  let* expr = parse_expr in
  return (Str_item_eval expr)
;;

let parse_binding_val =
  let* name = parse_pat in
  skip_ws
  *> char '='
  *> skip_ws
  *>
  let* expr = parse_expr in
  return (Binding (name, expr))
;;

let parse_binding_fun =
  let* name = parse_pat_ident (* ops are not yet supported *) in
  let* args = many1 (skip_ws *> parse_pat) in
  skip_ws
  *> char '='
  *> skip_ws
  *>
  let* expr = parse_expr in
  let rec wrap args =
    match args with
    | h :: tl -> Expr_fun (h, wrap tl)
    | [] -> expr
  in
  return (Binding (name, wrap args))
;;

let parse_binding = parse_binding_val <|> parse_binding_fun

let parse_structure_item_def =
  skip_token "let"
  *>
  let* rec_flag = option Nonrecursive (string "rec" *> skip_ws *> return Recursive) in
  let* bindings = sep_by1 (skip_token "and") parse_binding in
  return (Str_item_def (rec_flag, bindings))
;;

let parse_structure_item = choice [ parse_structure_item_expr; parse_structure_item_def ]
let parse_program = sep_by (skip_ws *> string ";;" *> skip_ws) parse_structure_item
