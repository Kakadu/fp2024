(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Ast
open Angstrom

let skip_ws = skip_while Char.is_whitespace

let keyword = function
  | "and"
  | "elif"
  | "else"
  | "false"
  | "fun"
  | "function"
  | "if"
  | "in"
  | "let"
  | "match"
  | "or"
  | "rec"
  | "then"
  | "true"
  | "type"
  | "with" -> true
  | _ -> false
;;

let ident_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' -> true
  | _ -> false
;;

let ident_start_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let parse_ident =
  let* first = satisfy ident_start_char >>| String.of_char in
  let* tail = take_while ident_char in
  return (first ^ tail)
;;

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
