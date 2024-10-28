(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast

(* F# compiler forbids tabs by default *)
let is_whitespace = function
  | ' ' | '\n' | '\r' -> true
  | _ -> false
;;

let skip_ws = skip_while is_whitespace
let skip_ws1 = satisfy is_whitespace *> skip_ws
let skip_token str = skip_ws *> string str <* skip_ws >>= fun _ -> return ()

let is_keyword = function
  | "and"
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

let is_ident_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' -> true
  | _ -> false
;;

let is_ident_start_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let parse_ident =
  let* first = satisfy is_ident_start_char >>| String.of_char in
  let* rest =
    match first with
    | "_" -> take_while1 is_ident_char
    | _ -> take_while is_ident_char
  in
  let ident = first ^ rest in
  if is_keyword ident
  then fail "Keywords cannot be used as identificators"
  else return ident
;;

let parse_const_int =
  let* int = take_while1 Char.is_digit >>| Int.of_string in
  return (Const_int int)
;;

let parse_const_char =
  let* char = char '\'' *> any_char <* char '\'' in
  return (Const_char char)
;;

let parse_const_string =
  let* str = char '"' *> take_till (Char.equal '"') <* char '"' in
  return (Const_string str)
;;

let parse_const_bool =
  let* bool = string "true" <|> string "false" >>| Bool.of_string in
  return (Const_bool bool)
;;

(* Float parsing functions should probably be inside one big function *)
(* Parse the float in form digit+ . digit* [f|F] *)
let parse_const_float_sim =
  let* int_part = take_while1 Char.is_digit in
  let* dot = string "." in
  let* fract_part = take_while Char.is_digit in
  let* f = string "f" <|> string "F" <|> string "" in
  let float = Float.of_string (int_part ^ dot ^ fract_part ^ f) in
  return (Const_float float)
;;

(* Parse the float in form digit+ (. digit* )? (e|E) (+|-)? digit+ [f|F]
   Not yet implemented *)

let parse_const_float = parse_const_float_sim

let parse_const =
  choice
    [ parse_const_int
    ; parse_const_char
    ; parse_const_string
    ; parse_const_bool
    ; parse_const_float
    ]
;;
