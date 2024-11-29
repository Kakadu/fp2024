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
let clean_up t = skip_separators *> t <* skip_separators
let token t = skip_separators *> string t <* skip_separators
let round_parens p = token "(" *> p <* token ")"
let square_brackets p = token "[]" *> p <* token "]"

(* ============================ name =========================== *)

(** Parse first letter then try parse the rest of name *)
let parse_name =
  let* parse_first = satisfy is_letter <|> satisfy (Char.equal '_') >>| Char.escaped in
  let* parse_rest =
    take_while1 (fun ch -> is_letter ch || is_digit ch || Char.equal '_' ch)
  in
  let name = parse_first ^ parse_rest in
  if is_keyword name then fail "Name must not match the keyword." else return name
;;

(* ========================== literals ========================= *)

let parse_int =
  clean_up
  @@
  let* sign = char '+' *> return 1 <|> char '-' *> return (-1) <|> return 1 in
  let* digit = take_while1 is_digit >>| int_of_string in
  return (Int (sign * digit))
;;

let parse_str =
  let parse_empty_string = string "{||}" >>| fun _ -> "" in
  let parse_content = string "{|" *> take_till (Char.equal '|') <* string "|}" in
  let* str = parse_empty_string <|> parse_content in
  return (Str str)
;;

let parse_bool =
  let* parsed_bool =
    choice [ token "true" *> return true; token "false" *> return false ]
  in
  return (Bool parsed_bool)
;;

let parse_unit =
  let* _ = token "()" in
  return Unit
;;

let parse_literal = parse_int <|> parse_str <|> parse_bool <|> parse_unit

(* ======================== expressions ======================== *)
let parse_expr_var =
  let* var = parse_name in
  return (ExpVar var)
;;

let parse_expr_literal =
  let* literal = parse_literal in
  return (ExpConst literal)
;;

let parse_expr_list parse_expression =
  let* list = square_brackets @@ sep_by (token ";") parse_expression in
  return (ExpList list)
;;

(* wip *)
let parse_expr_tuple parse_expression =
  let* first = parse_expression in
  let* rest = many1 (token "," *> parse_expression) in
  return (ExpTuple (first :: rest)) |> parens
;;
