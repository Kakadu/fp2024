(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Angstrom
open Ast
open Common
open Keywords

let ptype_builtin =
  let* id = pid in
  match id with
  | t when is_builtin_type t -> return (Type_ident t)
  | _ -> fail "Failed to parse built-in type"
;;

let ptype_tuple ptype =
  let* t1 = skip_ws *> ptype <* skip_ws <* char '*' in
  let* t2 = skip_ws *> ptype <* skip_ws in
  let* trest = many (skip_token "*" *> ptype) in
  return (Type_tuple (t1, t2, trest))
;;

let ptype_paren ptype = string "(" *> skip_ws *> ptype <* skip_ws <* string ")"

let ptype_func ptype =
  let parr = skip_ws *> string "->" *> return (fun t1 t2 -> Type_func (t1, t2)) in
  chainr ptype parr
;;

let ptype =
  fix (fun ptype ->
    let ptype = ptype_paren ptype <|> ptype_builtin in
    let ptype = ptype_tuple ptype <|> ptype in
    let ptype = ptype_func ptype <|> ptype in
    skip_ws *> ptype <* skip_ws_no_nl)
;;
