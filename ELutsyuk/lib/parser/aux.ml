(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_sep = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "and"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "match"
  | "with"
  | "in"
  | "true"
  | "false"
  | "Some"
  | "None"
  | "type"
  | "val"
  | "while"
  | "for"
  | "_" -> true
  | _ -> false
;;

let skip_sep = skip_while is_sep
let trim t = skip_sep *> t <* skip_sep
let token t = skip_sep *> string t <* skip_sep
let round_par p = token "(" *> p <* token ")"
let square_par p = token "[" *> p <* token "]"

(** Parse first letter then try parse the rest of id *)
let pid =
  let* p_first = satisfy is_letter <|> satisfy (Char.equal '_') >>| Char.escaped in
  let* p_rest =
    take_while1 (fun ch -> is_letter ch || is_digit ch || Char.equal '_' ch) <|> return ""
  in
  let id = p_first ^ p_rest in
  if is_keyword id
  then fail "Error! parse_id: id must not match the keyword."
  else return id
;;

let pint = trim @@ take_while1 is_digit >>| fun x -> Int (int_of_string x)

let pstr =
  let p_empty_string = string "{||}" >>| fun _ -> "" in
  let p_content = string "{|" *> take_till (Char.equal '|') <* string "|}" in
  let* str = p_empty_string <|> p_content in
  return @@ Str str
;;

let pbool =
  let* bool =
    choice [ trim @@ (string "true" *> return true); token "false" *> return false ]
  in
  skip_sep *> (return @@ Bool bool)
;;

let punit =
  let* _ = token "()" in
  return Unit
;;

let pconst = choice [ pint; pstr; pbool; punit ]
