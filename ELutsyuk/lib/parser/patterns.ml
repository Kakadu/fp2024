(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Auxiliaries
open Constants

let parse_pat_var =
  trim
  @@
  let* var = parse_id in
  return @@ PVar var
;;

let parse_pat_cons =
  trim
  @@
  let* cons = parse_cons in
  return @@ PCons cons
;;

let parse_pat_any =
  trim
  @@
  let* _ = token "_" in
  return @@ PAny
;;

let parse_pat_tuple parse_pat =
  round_parens
  @@
  let* el1 = parse_pat in
  let* el2 = token "," *> parse_pat in
  let* rest = many (token "," *> parse_pat) in
  return @@ PTuple (el1, el2, rest)
;;

let parse_pat =
  fix
  @@ fun parse_pat ->
  choice [ parse_pat_var; parse_pat_any; parse_pat_cons; parse_pat_tuple parse_pat ]
;;
