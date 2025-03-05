(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast
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

let prs_pat_tuple prs_pat =
  skip_ws
  *>
  let* el1 = prs_pat in
  let* el2 = token "," *> prs_pat in
  let+ rest = many (token "," *> prs_pat) in
  PatTup (el1, el2, rest)
;;

let prs_pat_cons prs_pat =
  trim
  @@
  let* el1 = prs_pat in
  let* rest = many (token "::" *> prs_pat) in
  let rec helper = function
    | [] -> el1
    | [ el2 ] -> el2
    | el2 :: rest -> PatListCons (el2, helper rest)
  in
  return (helper (el1 :: rest))
;;

let prs_pat_list prs_pat =
  square_par
  @@
  let+ parsed = sep_by (token ";") prs_pat in
  PatList parsed
;;

let prs_pat =
  fix
  @@ fun prs_pat ->
  let atomary =
    choice
      [ prs_pat_any
      ; prs_pat_var
      ; prs_pat_constant
      ; round_par prs_pat
      ; prs_pat_list prs_pat
      ]
  in
  let constructor = prs_pat_cons atomary in
  let tuple = prs_pat_tuple atomary <|> constructor in
  tuple
;;
