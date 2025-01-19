(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Angstrom
open Ast
open Common
open Expressions
open Units_of_measure

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
  let* binding_fst = parse_single_binding parse_expr in
  let* binding_rest = many (skip_token "and" *> parse_single_binding parse_expr) in
  return (Str_item_def (rec_flag, binding_fst, binding_rest))
;;

let pstritem_td =
  let pstritem_mtd =
    skip_token "[<Measure>]"
    *> skip_token "type"
    *>
    let* name = parse_ident <* skip_ws in
    let* rhs = option None (skip_token "=" *> pm >>| fun m -> Some m) in
    return (Str_item_type_def (Measure_type_def (name, rhs)))
  in
  pstritem_mtd
;;

let parse_structure_item =
  choice [ parse_structure_item_expr; parse_structure_item_def; pstritem_td ]
;;

let parse_program =
  sep_by
    (skip_token ";;" (* <|> skip_ws_no_nl *> char '\n' *> skip_ws*))
    parse_structure_item
  <* (skip_ws *> string ";;" <|> string "" <* skip_ws)
;;
