(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Angstrom
open Ast
open Common
open Constants
open Types

let ppat_wild = char '_' *> return Pattern_wild
let ppat_id_or_op = pid_or_op >>| fun i -> Pattern_ident_or_op i
let ppat_const = psconst >>| fun c -> Pattern_const c
let ppat_paren ppat = string "(" *> skip_ws *> ppat <* skip_ws <* string ")"

(* Parses tuple without parentheses *)
let ppat_tuple ppat =
  let* t1 = skip_ws *> ppat <* skip_ws <* char ',' in
  let* t2 = skip_ws *> ppat <* skip_ws in
  let* trest = many (skip_token "," *> ppat) in
  return (Pattern_tuple (t1, t2, trest))
;;

let ppat_list ppat =
  let* list = char '[' *> sep_by (char ';') (skip_ws *> ppat <* skip_ws) <* char ']' in
  return (Pattern_list list)
;;

let ppat_or ppat =
  let ppipe = skip_ws *> string "|" *> return (fun p1 p2 -> Pattern_or (p1, p2)) in
  chainl ppat ppipe
;;

let ppat_typed ppat =
  let* pat = ppat in
  let* core_type = skip_token ":" *> ptype in
  return (Pattern_typed (pat, core_type))
;;

let ppat =
  fix (fun ppat ->
    let ppat =
      choice [ ppat_paren ppat; ppat_list ppat; ppat_const; ppat_id_or_op; ppat_wild ]
    in
    let ppat = ppat_tuple ppat <|> ppat in
    let ppat = ppat_or ppat <|> ppat in
    let ppat = ppat_typed ppat <|> ppat in
    skip_ws *> ppat <* skip_ws_no_nl)
;;
