(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Angstrom

let fail = fail ""
let fail_if cond = if cond then fail else return ()
let skip_whitespace = skip_many1 (satisfy Char.is_whitespace)
let skip_line_whitespace = skip_many1 (char ' ' <|> char '\t')
let parse_line_comment = string "//" *> many_till any_char (char '\n') *> return ()
let parse_block_comment = string "/*" *> many_till any_char (string "*/") *> return ()
let parse_comment = parse_line_comment <|> parse_block_comment
let ws = skip_many (parse_comment <|> skip_whitespace)
let ws_line = skip_many (parse_block_comment <|> skip_line_whitespace)
let token s = ws_line *> string s <* ws
let parens p = char '(' *> ws *> p <* ws_line <* char ')'
let square_brackets p = char '[' *> ws *> p <* ws_line <* char ']'
let curly_braces p = char '{' *> ws *> p <* ws_line <* char '}'
let sep_by_comma p = sep_by (token ",") p
let sep_by_comma1 p = sep_by1 (token ",") p
let parse_stmt_sep = ws_line *> (char '\n' <|> char ';') *> ws

let parse_int =
  take_while1 Char.is_digit
  >>= fun str ->
  match Stdlib.int_of_string_opt str with
  | Some num -> return num
  | None -> fail
;;

let is_keyword = function
  | "break"
  | "func"
  | "defer"
  | "go"
  | "chan"
  | "if"
  | "else"
  | "continue"
  | "for"
  | "return"
  | "var" -> true
  | _ -> false
;;

let parse_ident =
  let is_first_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let is_valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let* first_char = satisfy is_first_char >>| Char.to_string in
  let* rest = take_while is_valid_char in
  let ident = first_char ^ rest in
  fail_if (is_keyword ident) *> return ident
;;

(** [parse_simple_type] parses [int], [bool] and [string] types *)
let parse_simple_type =
  choice
    [ string "int" *> return Type_int
    ; string "string" *> return Type_string
    ; string "bool" *> return Type_bool
    ]
;;

let parse_func_type ptype =
  let* _ = string "func" *> ws in
  let* args = parens (sep_by_comma ptype) in
  let* returns =
    ws_line
    *> choice
         [ (ptype >>| fun type' -> [ type' ]); parens (sep_by_comma ptype); return [] ]
  in
  return (Type_func (args, returns))
;;

let parse_array_type ptype =
  let* size = square_brackets parse_int in
  let* type' = ws_line *> ptype in
  return (Type_array (size, type'))
;;

let parse_chan_type ptype =
  let* chan_type = string "chan" *> ws *> ptype in
  return (Type_chan chan_type)
;;

let parse_type =
  fix (fun ptype ->
    parens ptype
    <|> choice
          [ parse_simple_type
          ; parse_func_type ptype
          ; parse_array_type ptype
          ; parse_chan_type ptype
          ])
;;
