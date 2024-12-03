(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom

let rec chainr1 pexpr op =
  let* left_operand = pexpr in
  let* f = op in
  chainr1 pexpr op >>| f left_operand <|> return left_operand
;;

let chainl1 pexpr op =
  let rec go acc =
    let* f = op in
    let* right_operand = pexpr in
    go (f acc right_operand) <|> return acc
  in
  let* init = pexpr in
  go init
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
  | "in" -> true
  | _ -> false
;;

let skip_separators = skip_while is_separator
let trim t = skip_separators *> t <* skip_separators
let token t = skip_separators *> string t <* skip_separators
let round_parens p = token "(" *> p <* token ")"
let square_brackets p = token "[]" *> p <* token "]"

(** Parse first letter then try parse the rest of id *)
let parse_id =
  let* parse_first = satisfy is_letter <|> satisfy (Char.equal '_') >>| Char.escaped in
  let* parse_rest =
    take_while1 (fun ch -> is_letter ch || is_digit ch || Char.equal '_' ch)
  in
  let id = parse_first ^ parse_rest in
  if is_keyword id
  then fail "Error! parse_id: id must not match the keyword."
  else return id
;;
