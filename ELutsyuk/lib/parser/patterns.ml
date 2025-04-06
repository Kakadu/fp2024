(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast
open Types
open Constants
open PrsAuxilary

let prs_pat_var =
  trim
  @@
  let+ parsed = prs_id in
  PatVar parsed
;;

let prs_pat_constant =
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

let prs_pat_tuple pat =
  trim
  @@
  let* el1 = pat in
  let* el2 = token "," *> pat in
  let+ rest = many (token "," *> pat) in
  PatTup (el1, el2, rest)
;;

let prs_pat_cons pat =
  trim
  @@
  let* el1 = pat in
  let* rest = many (token "::" *> pat) in
  let rec helper = function
    | [] -> el1
    | [ el2 ] -> el2
    | el2 :: rest -> PatListCons (el2, helper rest)
  in
  return (helper (el1 :: rest))
;;

let prs_pat_list pat =
  square_par
  @@
  let+ parsed = sep_by (token ";") pat in
  PatList parsed
;;

let prs_pat_type pat =
  round_par
  @@
  let* pat = pat in
  let* _ = token ":" in
  let+ typ = prs_typ in
  PatType (pat, typ)
;;

let prs_pat =
  fix
  @@ fun pat ->
  let atomary =
    choice
      [ prs_pat_any
      ; prs_pat_var
      ; prs_pat_constant
      ; round_par pat
      ; prs_pat_type pat
      ; prs_pat_list pat
      ]
  in
  let cons = prs_pat_cons atomary in
  let tuple = prs_pat_tuple atomary <|> cons in
  tuple
;;
