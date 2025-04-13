(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common

let ptype_var =
  let* fst =
    char '\'' *> satisfy (fun c -> Base.Char.is_alpha c && Char.is_lowercase c)
    >>| fun c -> String.of_char c
  in
  let* num = pint >>| fun i -> Int.to_string i in
  return (Type_var ("'" ^ fst ^ num))
;;

let ptype_base =
  choice
    [ string "int" *> return Type_int
    ; string "float" *> return Type_float
    ; string "bool" *> return Type_bool
    ; string "char" *> return Type_char
    ; string "string" *> return Type_string
    ; string "unit" *> return Type_unit
    ; ptype_var
    ]
;;

let ptype_tuple ptype =
  let* t1 = skip_ws *> ptype <* skip_ws <* char '*' in
  let* t2 = skip_ws *> ptype <* skip_ws in
  let* trest = many (skip_token "*" *> ptype) in
  return (Type_tuple (t1, t2, trest))
;;

let ptype_constr ptype =
  let f acc = function
    | "list" -> Type_list acc
    | _ -> Type_option acc
  in
  let* fst = ptype in
  let chainl_constr =
    let rec helper acc =
      (let* ty = skip_ws *> string "list" <|> string "option" in
       helper (f acc ty))
      <|> return acc
    in
    helper fst
  in
  chainl_constr
;;

let ptype_paren ptype = string "(" *> skip_ws *> ptype <* skip_ws <* string ")"

let ptype_func ptype =
  let parr = skip_ws *> string "->" *> return (fun t1 t2 -> Type_func (t1, t2)) in
  chainr ptype parr
;;

let ptype =
  fix (fun ptype_full ->
    let ptype = ptype_base <|> ptype_paren ptype_full in
    let ptype = ptype_constr ptype <|> ptype in
    let ptype = ptype_tuple ptype <|> ptype in
    skip_ws *> ptype_func ptype <|> ptype <* skip_ws_no_nl)
;;
