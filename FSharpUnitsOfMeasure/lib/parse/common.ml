(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for parts of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Ast
open Angstrom

let parse_int = take_while1 Char.is_digit >>| Int.of_string
let parse_char = char '\'' *> any_char <* char '\''
let parse_string = char '"' *> take_till (Char.equal '"') <* char '"'
let parse_bool = string "true" <|> string "false" >>| Bool.of_string

let parse_float =
  let* int_part = take_while1 Char.is_digit in
  let* dot = string "." in
  let* fract_part = take_while Char.is_digit in
  return (Float.of_string (int_part ^ dot ^ fract_part))
;;

let parse_const =
  choice
    [ (parse_int >>| fun i -> Const_int i)
    ; (parse_char >>| fun c -> Const_char c)
    ; (parse_string >>| fun s -> Const_string s)
    ; (parse_bool >>| fun b -> Const_bool b)
    ; (parse_float >>| fun f -> Const_float f)
    ]
;;
