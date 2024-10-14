(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error res -> print_endline res
;;

let is_keyword = function
  (* https://go.dev/ref/spec#Keywords *)
  | "break"
  | "case"
  | "chan"
  | "const"
  | "defer"
  | "else"
  | "for"
  | "func"
  | "go"
  | "if"
  | "range"
  | "return"
  | "type"
  | "var" -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_space_or_tab = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let many_sep ~sep ~parser =
  sep_by sep parser
  >>| fun list ->
  let rec helper = function
    | hd :: tl -> hd :: helper tl
    | [] -> []
  in
  helper list
;;

let skip_whitespace = skip_while Char.is_whitespace
let skip_line_whitespace = skip_while is_space_or_tab

let parse_line_comment =
  string "//" *> many_till any_char (char '\n') *> char '\n' *> return ()
;;

let parse_block_comment =
  string "/*" *> many_till any_char (string "*/") *> string "*/" *> return ()
;;

let parse_comment = parse_line_comment <|> parse_block_comment
let ws = many (skip_whitespace *> parse_comment) *> skip_whitespace
let ws_line = many (skip_line_whitespace *> parse_block_comment) *> skip_line_whitespace
let token s = ws_line *> string s <* ws
let parens p = token "(" *> p <* token ")"

(* at least one newline *)
let parse_newline = skip_while is_space_or_tab *> char '\n' *> ws
let parse_stmt_sep = parse_newline <|> ws *> char ';' *> ws
let parse_const_int = take_while1 is_digit >>| fun num -> Const_int (Int.of_string num)

let parse_const_bool =
  string "true" <|> string "false" >>| fun bl -> Const_bool (Bool.of_string bl)
;;

let parse_const_string =
  let parse_string = take_till (Char.equal '"') >>| fun str -> Const_string str in
  char '"' *> parse_string <* char '"'
;;

let parse_const = choice [ parse_const_int; parse_const_string; parse_const_bool ]

let parse_ident =
  let is_first_char_valid = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let* first_char = peek_char in
  match first_char with
  | Some chr when is_first_char_valid chr ->
    let* ident = take_while is_char in
    if is_keyword ident then fail "This is a keyword" else return ident
  | _ -> fail "Invalid identifier name"
;;

(* TODO: add arrays and functions *)
let parse_type =
  choice
    [ string "int" *> return Type_int
    ; string "string" *> return Type_string
    ; string "bool" *> return Type_bool
    ]
    ~failure_msg:"Invalid type"
;;
