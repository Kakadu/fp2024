(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common
open Constants

let parse_pat_wild = char '_' *> return Pattern_wild
let parse_pat_ident = parse_ident >>| fun i -> Pattern_ident i
let parse_pat_const = parse_const >>| fun c -> Pattern_const c

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

let parse_pat =
  fix (fun parse_pat ->
    let pat =
      choice
        [ parse_pat_paren parse_pat
        ; parse_pat_list parse_pat
        ; parse_pat_ident
        ; parse_pat_wild
        ; parse_pat_const
        ]
    in
    let pat = parse_pat_tuple pat <|> pat in
    skip_ws *> pat <* skip_ws)
;;
