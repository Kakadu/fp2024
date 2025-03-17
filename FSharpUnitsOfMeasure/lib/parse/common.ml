(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Keywords

(* F# compiler forbids tabs by default *)
let is_whitespace = function
  | ' ' | '\n' | '\r' -> true
  | _ -> false
;;

let is_ws_no_nl = function
  | ' ' | '\r' -> true
  | _ -> false
;;

let skip_ws = skip_while is_whitespace
let skip_ws_no_nl = skip_while is_ws_no_nl
let skip_ws_nl = skip_ws_no_nl *> char '\n' *> skip_ws
let skip_ws1 = satisfy is_whitespace *> skip_ws
let skip_token str = skip_ws *> string str <* skip_ws >>= fun _ -> return ()

let is_op_char = function
  | '+' | '-' | '*' | '/' | '<' | '>' | '=' | '|' | '.' -> true
  | _ -> false
;;

let is_builtin_op = function
  | "+"
  | "-"
  | "*"
  | "/"
  | "<="
  | "<"
  | ">="
  | ">"
  | "||"
  | "&&"
  | "+."
  | "-."
  | "*."
  | "/." -> true
  | _ -> false
;;

let is_id_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '\'' -> true
  | _ -> false
;;

let is_id_stchar = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let pid =
  let* first = satisfy is_id_stchar >>| String.of_char in
  let* rest =
    match first with
    | "_" -> take_while1 is_id_char
    | _ -> take_while is_id_char
  in
  let ident = first ^ rest in
  if is_keyword ident
  then fail "Keywords cannot be used as identificators"
  else return ident
;;

let pbuiltin_op =
  let* op = take_while is_op_char in
  match op with
  | op when is_builtin_op op -> return op
  | _ -> fail "Failed to parse builtin op"
;;

let pid_or_op =
  let parse_op = skip_token "(" *> pbuiltin_op <* skip_token ")" in
  let* ident_or_op = pid <|> parse_op in
  return ident_or_op
;;

let pchar = char '\'' *> any_char <* char '\''

(* Can't parse strings with '\' char *)
let pstring = char '"' *> take_till (Char.equal '"') <* char '"'
let pbool = string "true" <|> string "false" >>| Bool.of_string

(* Parses unsigned ints *)
let pint =
  let* int = take_while1 Char.is_digit >>| Int.of_string in
  let* next_char = peek_char in
  match next_char with
  | Some x when Char.equal x '.' -> fail "Cannot parse int, met float"
  | Some x when is_id_char x -> fail "Cannot parse int, met ident"
  | _ -> return int
;;

(* Parses signed ints *)
let psint =
  let* sign = option "" (skip_ws *> string "-" <* skip_ws) in
  let* int = pint in
  match sign with
  | "-" -> return (-int)
  | _ -> return int
;;

(* Parses unsigned floats. Used for expressions.
   Floats can be in following forms:
   [0-9]+ . [0-9]* [f|F]
   [0-9]+ (. [0-9]* )? (e|E) (+|-)? [0-9]+ [f|F] *)
let pfloat =
  let* int_part = take_while1 Char.is_digit in
  let* dot = option "" (string ".") in
  let* fract_part =
    match dot with
    | "." -> take_while Char.is_digit
    | _ -> return ""
  in
  let* e =
    if String.equal dot ""
    then string "e" <|> string "E"
    else option "" (string "e" <|> string "E")
  in
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

(* Parses signed floats. Used for patterns.
   Floats can be in following forms:
   (+|-)? [0-9]+ . [0-9]* [f|F]
   (+|-)? [0-9]+ (. [0-9]* )? (e|E) (+|-)? [0-9]+ [f|F] *)
let psfloat =
  let* sign = option "" (skip_ws *> string "-" <* skip_ws) in
  let* float = pfloat in
  match sign with
  | "-" -> return (Float.neg float)
  | _ -> return float
;;

let chainl pa psep =
  let rec wrap a1 =
    let* sep = psep in
    let* a2 = skip_ws *> pa in
    let binop = sep a1 a2 in
    wrap binop <|> return binop
  in
  skip_ws *> pa >>= fun init -> wrap init
;;

let rec chainr pa psep =
  pa >>= fun a -> psep >>= (fun f -> chainr (skip_ws *> pa) psep >>| f a) <|> return a
;;
