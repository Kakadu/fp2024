(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Aux

let ppat_var =
  trim
  @@
  let* var = pid in
  return @@ PatVar var
;;

let ppat_const =
  trim
  @@
  let* cons = pconst in
  return @@ PatConst cons
;;

let ppat_any =
  trim
  @@
  let* _ = token "_" in
  return PatAny
;;

let ppat_tuple ppat =
  round_par
  @@
  let* el1 = ppat in
  let* el2 = token "," *> ppat in
  let* rest = many (token "," *> ppat) in
  return @@ PatTup (el1, el2, rest)
;;

let ppat = fix @@ fun ppat -> choice [ ppat_var; ppat_any; ppat_const; ppat_tuple ppat ]
