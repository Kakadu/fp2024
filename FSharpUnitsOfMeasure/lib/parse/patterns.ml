(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common
open Constants
open Types

let parse_pat_wild = char '_' *> return Pattern_wild
let parse_pat_ident_or_op = parse_ident_or_op >>| fun i -> Pattern_ident_or_op i
let parse_pat_const = parse_const_s >>| fun c -> Pattern_const c

let parse_pat_paren parse_pat =
  string "(" *> skip_ws *> parse_pat <* skip_ws <* string ")"
;;

(* Parses tuple without parentheses *)
let parse_pat_tuple parse_pat =
  let* tuple_fst = skip_ws *> parse_pat <* skip_ws <* char ',' in
  let* tuple_snd = skip_ws *> parse_pat <* skip_ws in
  let* tuple_rest = many (skip_token "," *> parse_pat) in
  return (Pattern_tuple (tuple_fst, tuple_snd, tuple_rest))
;;

let parse_pat_list parse_pat =
  let* list =
    char '[' *> sep_by (char ';') (skip_ws *> parse_pat <* skip_ws) <* char ']'
  in
  return (Pattern_list list)
;;

let parse_pat_or parse_pat =
  let parse_pipe = skip_ws *> string "|" *> return (fun p1 p2 -> Pattern_or (p1, p2)) in
  chainl parse_pat parse_pipe
;;

let parse_pat_typed parse_pat =
  let* pat = parse_pat in
  let* core_type = skip_token ":" *> parse_type in
  return (Pattern_typed (pat, core_type))
;;

let parse_pat =
  fix (fun parse_pat ->
    let pat =
      choice
        [ parse_pat_paren parse_pat
        ; parse_pat_list parse_pat
        ; parse_pat_const
        ; parse_pat_ident_or_op
        ; parse_pat_wild
        ;
        ]
    in
    let pat = parse_pat_tuple pat <|> pat in
    let pat = parse_pat_or pat <|> pat in
    let pat = parse_pat_typed pat <|> pat in
    skip_ws *> pat <* skip_ws)
;;
