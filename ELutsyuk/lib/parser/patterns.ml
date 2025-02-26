(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Const
open Auxilary

let prs_pat_var =
  trim
  @@
  let+ parsed = prs_id in
  PatVar parsed
;;

let prs_pat_const =
  trim
  @@
  let+ parsed = prs_const in
  PatConst parsed
;;

let prs_pat_any =
  trim
  @@
  let* _ = token "_" in
  let* rest = take_while Base.Char.is_alphanum in
  match rest with
  | "" -> return PatAny
  | _ -> fail "Not any pattern"
;;

let prs_pat_tuple prs_pat =
  round_par
  @@
  let* el1 = prs_pat in
  let* el2 = token "," *> prs_pat in
  let+ rest = many (token "," *> prs_pat) in
  PatTup (el1, el2, rest)
;;

let prs_pat =
  fix
  @@ fun prs_pat ->
  choice [ prs_pat_any; prs_pat_var; prs_pat_const; prs_pat_tuple prs_pat ]
;;
