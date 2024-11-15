(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom

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

let parse_char = char '\'' *> any_char <* char '\''
let parse_string = char '"' *> take_till (Char.equal '"') <* char '"'
let parse_bool = string "true" <|> string "false" >>| Bool.of_string

let parse_int =
  let* int = take_while1 Char.is_digit >>| Int.of_string in
  let* next_char = peek_char in
  match next_char with
  | Some x when Char.equal x '.' -> fail "Cannot parse int, met float"
  | _ -> return int
;;

(* Floats can be in following forms:
   [0-9]+ . [0-9]* [f|F]
   [0-9]+ (. [0-9]* )? (e|E) (+|-)? [0-9]+ [f|F] *)
let parse_float =
  let* int_part = take_while1 Char.is_digit in
  let* dot = option "" (string ".") in
  let* fract_part =
    match dot with
    | "." -> take_while Char.is_digit
    | _ -> return ""
  in
  let* e = option "" (string "e" <|> string "E") in
  let* exp_sign =
    match e with
    | "e" | "E" -> option "" (string "+" <|> string "-")
    | _ -> return ""
  in
  let* exp =
    match e with
    | "e" | "E" -> take_while1 Char.is_digit
    | _ -> return ""
  in
  let* _ = option "" (string "f" <|> string "F") in
  let float = Float.of_string (int_part ^ dot ^ fract_part ^ e ^ exp_sign ^ exp) in
  return float
;;

let chainl parse_alpha parse_sep =
  let rec wrap alpha1 =
    let* app_sep = parse_sep in
    let* alpha2 = skip_ws *> parse_alpha in
    let binop = app_sep alpha1 alpha2 in
    wrap binop <|> return binop
  in
  skip_ws *> parse_alpha >>= fun init -> wrap init
;;

let rec chainr parse_alpha parse_sep =
  parse_alpha
  >>= fun a ->
  parse_sep
  >>= (fun f -> chainr (skip_ws *> parse_alpha) parse_sep >>| f a)
  <|> return a
;;
