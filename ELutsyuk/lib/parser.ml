(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open Ast

(*
   from homk: >>| : 'a t -> ('a -> 'b) -> 'b
   let* is a bind
   >>= "bind" -- выполняет первый парсер, значение подаёт функции, которая делает второй парсер
   >>| "map" -- выполняет парсер, а к результату применяет функцию и получается значение
   <$> -- синтаксический сахар для "apply" (меняет местами f и p)
   <*> "apply" -- выполняет парсер для функции, затем парсер для значения и затем применяет (если сделать "apply" над результатом "bind")
*)

let parse_vica =
  let* v = char 'V' in
  let* i = char 'i' in
  let* c = char 'c' in
  let* a = char 'a' in
  return [ v; i; c; a ]
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
let parse_token t = skip_separators *> t
let parse_string_token st = skip_separators *> string st

(** Parse first letter then try parse rest of id *)
let parse_id =
  parse_token
  @@
  let* parse_first = satisfy is_letter <|> satisfy (Char.equal '_') >>| Char.escaped in
  let* parse_rest =
    take_while1 (fun ch -> is_letter ch || is_digit ch || Char.equal '_' ch)
  in
  let id = parse_first ^ parse_rest in
  if is_keyword id then fail "Identifier must not match the keyword." else return id
;;

(*
   take_while захватывает любое количество символов
   take_while1 захватывает хотя бы 1 символ
*)
let parse_int =
  parse_token
  @@
  let* sign = char '+' *> return 1 <|> char '-' *> return (-1) <|> return 1 in
  let* digit = take_while1 is_digit >>| int_of_string in
  return (Int (sign * digit))
;;

let parse_str =
  parse_token
  @@
  let parse_empty_string = string "{||}" >>| fun _ -> "" in
  let parse_content = string "{|" *> take_till (Char.equal '|') <* string "|}" in
  let* str = parse_empty_string <|> parse_content in
  return (Str str)
;;
