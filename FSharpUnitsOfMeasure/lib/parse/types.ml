(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common
open Keywords

let parse_type_ident_builtin =
  let* type_ident = parse_ident in
  match type_ident with
  | t when is_builtin_type t -> return (Type_ident t)
  | _ -> fail "Failed to parse built-in type"
;;

let parse_type_tuple parse_type =
  let* tuple_fst = skip_ws *> parse_type <* skip_ws <* char '*' in
  let* tuple_snd = skip_ws *> parse_type <* skip_ws in
  let* tuple_rest = many (skip_token "*" *> parse_type) in
  return (Type_tuple (tuple_fst, tuple_snd, tuple_rest))
;;

let parse_type_paren parse_type =
  string "(" *> skip_ws *> parse_type <* skip_ws <* string ")"
;;

let parse_type_func parse_type =
  let parse_arrow = skip_ws *> string "->" *> return (fun t1 t2 -> Type_func (t1, t2)) in
  chainr parse_type parse_arrow
;;

let parse_type =
  fix (fun parse_type ->
    let core_type = parse_type_paren parse_type <|> parse_type_ident_builtin in
    let core_type = parse_type_tuple core_type <|> core_type in
    let core_type = parse_type_func core_type <|> core_type in
    skip_ws *> core_type <* skip_ws)
;;
