(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom

let chainr1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op (e >>= go) <|> return acc in
  e >>= go
;;

let chainl1 expr oper =
  let rec go acc = lift2 (fun f x -> f acc x) oper expr >>= go <|> return acc in
  expr >>= go
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_separator = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_keyword = function
  | "Some"
  | "None"
  | "rec"
  | "let"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "while"
  | "match"
  | "in"
  | "_" -> true
  | _ -> false
;;

let skip_separators = skip_while is_separator
let trim t = skip_separators *> t <* skip_separators
let token t = skip_separators *> string t <* skip_separators
let round_parens p = token "(" *> p <* token ")"
let square_brackets p = token "[" *> p <* token "]"

(** Parse first letter then try parse the rest of id *)
let parse_id =
  let* parse_first = satisfy is_letter <|> satisfy (Char.equal '_') >>| Char.escaped in
  let* parse_rest =
    take_while1 (fun ch -> is_letter ch || is_digit ch || Char.equal '_' ch) <|> return ""
  in
  let id = parse_first ^ parse_rest in
  if is_keyword id
  then fail "Error! parse_id: id must not match the keyword."
  else return id
;;
