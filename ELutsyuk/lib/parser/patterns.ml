(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Auxiliaries
open Literals

let parse_pat_var =
  let* var = parse_id in
  return @@ PVar var
;;

let parse_pat_lit =
  let* lit = parse_lit in
  return @@ PLit lit
;;

let parse_pat_any =
  let* _ = token "_" in
  return @@ PAny
;;

let parse_pat_tuple parse_pat =
  round_parens
  @@
  let* el1 = parse_pat in
  let* el2 = token "," *> parse_pat in
  let* rest = many (token "," *> parse_pat) in
  return (PTuple (el1, el2, rest))
;;

let parse_pat =
  fix
  @@ fun parse_pat ->
  choice [ parse_pat_var; parse_pat_any; parse_pat_lit; parse_pat_tuple parse_pat ]
;;
