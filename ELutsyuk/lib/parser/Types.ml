(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.TypesTree
open PrsAuxilary

let prs_typ_constant =
  choice
    [ (token "int" >>| fun _ -> TypConst TInt)
    ; (token "string" >>| fun _ -> TypConst TStr)
    ; (token "bool" >>| fun _ -> TypConst TBool)
    ; (token "unit" >>| fun _ -> TypConst TUnit)
    ]
;;

let rec prs_typ_arrow prs_typ =
  let* left_ty = prs_typ in
  let+ right_ty = token "->" *> (prs_typ_arrow prs_typ <|> prs_typ) in
  TypArrow (left_ty, right_ty)
;;

let prs_typ_tup prs_typ =
  let* ty1 = prs_typ in
  let+ tys = many1 (token "*" *> prs_typ) in
  TypTuple (ty1 :: tys)
;;

let rec prs_typ_list prs_typ =
  let* ty = prs_typ in
  let* _ = token "list" in
  prs_typ_list (return (TypList ty)) <|> return (TypList ty)
;;

let rec prs_typ_option prs_typ =
  let* ty = prs_typ in
  let* _ = token "option" in
  prs_typ_option (return (TypOption ty)) <|> return (TypOption ty)
;;

let prs_typ =
  fix (fun typ ->
    let atom = prs_typ_constant <|> round_par typ in
    let list_or_option = prs_typ_list atom <|> prs_typ_option atom <|> atom in
    let tuple = prs_typ_tup list_or_option <|> list_or_option in
    let arrow = prs_typ_arrow tuple <|> tuple in
    arrow)
;;
