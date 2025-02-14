(** Copyright 2024, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** In this file, we define a number of auxiliary Boolean functions,
    and then, using them, define mini-parsers 
    for parsing the identifiers and strings *)

(** TO DO: добавить парсер для char *)

open Angstrom
open Ast

let is_char = function
  (* to recognize char, char -> bool *)
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false
;;

let is_digit = function
  (* to recognize digit, char -> bool *)
  | '0' .. '9' -> true
  | _ -> false
;;

let is_sep = function
  (* to recognize seperatator, char -> bool *)
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_keyword = function
  (* to recognize keyword, char list -> bool *)
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
  | "function"
  | "->"
  | "_" -> true
  | _ -> false
;;

let skip_sep = skip_while is_sep
let trim t = skip_sep *> t <* skip_sep
let token t = skip_sep *> string t <* skip_sep
let round_par p = token "(" *> p <* token ")"
let square_par p = token "[" *> p <* token "]"
let optional_round_par p = round_par p <|> trim p

(* Parse first letter then try parse the rest of id *)
let parse_id =
  let* p_first = satisfy is_char <|> satisfy (Char.equal '_') >>| Char.escaped in
  let* p_rest =
    take_while1 (fun ch -> is_char ch || is_digit ch || Char.equal '_' ch) <|> return ""
  in
  let id = p_first ^ p_rest in
  if is_keyword id
  then fail "Error! parse_id: id must not match the keyword."
  else return @@ id
;;

let parse_int = trim @@ take_while1 is_digit >>| fun x -> Const_int (int_of_string x)

let parse_char =
  let* char = token "'" *> any_char <* token "'" in
  return @@ Const_char char
;;

let parse_str =
  let p_empty_string = trim @@ string "\"\"" <|> string "{||}" >>| fun _ -> "" in
  let p_content =
    choice
      [ trim @@ (string "{|" *> take_till (Char.equal '|')) <* string "|}"
      ; trim @@ (string "\"" *> take_till (Char.equal '\"')) <* token "\""
      ]
  in
  let* str = p_empty_string <|> p_content in
  return @@ Const_string str
;;

let parse_bool =
  let* bool = choice [ token "true" *> return true; token "false" *> return false ] in
  return @@ Const_bool bool
;;

let parse_unit =
  let* _ = token "()" in
  return @@ Const_unit
;;

let parse_const = choice [ parse_int; parse_char; parse_str; parse_bool; parse_unit ]

(* -------parse patterns------ *)

let parse_any_pattern =
  let* _ = trim @@ token "_" in
  return @@ Pattern_any
;;

let parse_var_pattern =
  let* var = trim @@ parse_id in
  return @@ Pattern_var var
;;

let parse_const_pattern =
  let* const = trim @@ parse_const in
  return @@ Pattern_const const
;;

let parse_tuple_pattern parse_pattern =
  round_par
  @@
  let* first_pattern = parse_pattern in
  let* other_patterns = many (token "," *> parse_pattern) in
  return @@ Pattern_tuple (first_pattern :: other_patterns)
;;

let parse_pattern =
  fix
  @@ fun parse_pattern ->
  choice
    [ parse_any_pattern
    ; parse_const_pattern
    ; parse_var_pattern
    ; parse_tuple_pattern parse_pattern
    ]
;;

(* parse expression cases :

  | Expr_var of ident 
  | Expr_const of constant 
  | Expr_binary_op of binary_op * expression * expression
  | Expr_if_then_else of expression * expression * expression
  | Expr_match_with of pattern * ( (pattern * expression)  list )
  | Expr_construct_in of construction_in
  | Expr_tuple of expression list
  | Expr_anonym_fun of pattern * expression
  | Expr_application of ident * ( expression list ) *)

let parse_expr_var =
  let* var = parse_id in
  return @@ Expr_var var
;;

let parse_expr_const =
  let* const = parse_const in
  return @@ Expr_const const
;;

let parse_expr_tuple parse_expr =
  round_par
  @@
  let* first = parse_expr in
  let* other = many (token "," *> parse_expr) in
  return @@ Expr_tuple (first :: other)
;;

let parse_expr_list parse_expr =
  let empty_list_parse = token "[]" *> (return @@ Expr_list []) in
  let non_empty_list_parse =
    square_par
    @@
    let* first = parse_expr in
    let* other = many (token ";" *> parse_expr) in
    return @@ Expr_list (first :: other)
  in
  empty_list_parse <|> non_empty_list_parse
;;

(* parsing of constants, vars, lists and tuples of constants and vars *)
let parse_expr_base_elements =
  fix
  @@ fun parse_expr ->
  choice
    [ parse_expr_const
    ; parse_expr_var
    ; parse_expr_list parse_expr
    ; parse_expr_tuple parse_expr
    ]
;;

(* operator prioritisation based on grammar *)
let parse_bin_op_T1 = token "||" *> return Or
let parse_bin_op_T2 = token "&&" *> return And

let parse_bin_op_T3 =
  choice
    [ token "<=" *> return LessEqual
    ; token ">=" *> return GreaterEqual
    ; token ">" *> return Greater
    ; token "<" *> return Less
    ; (token "!=" <|> token "<>") *> return NotEqual
    ]
;;

let parse_bin_op_T4 = choice [ token "+" *> return Plus; token "-" *> return Sub ]
let parse_bin_op_T5 = choice [ token "*" *> return Mul; token "/" *> return Div ]

(* ---------start of binary operators parsing--------- *)

let parse_T5 =
  let* t5 = parse_expr_base_elements <|> round_par @@ parse_bin_op_expression in
  return @@ t5

and parse_T4 =
  fix
  @@ fun parse_T4 ->
  let variant_1 =
    let* t5 = parse_T5 in
    let* mul_div_op = parse_bin_op_T5 in
    let* t4 = parse_T4 in
    return @@ Expr_binary_op (mul_div_op, t5, t4)
  in
  let variant_2 =
    let* t5 = parse_T5 in
    return @@ t5
  in
  let* result = variant_1 <|> variant_2 in
  return @@ result

and parse_T3 =
  fix
  @@ fun parse_T3 ->
  let variant_1 =
    let* t4 = parse_T4 in
    let* plus_sub_op = parse_bin_op_T4 in
    let* t3 = parse_T3 in
    return @@ Expr_binary_op (plus_sub_op, t4, t3)
  in
  let variant_2 =
    let* t4 = parse_T4 in
    return @@ t4
  in
  let* result = variant_1 <|> variant_2 in
  return @@ result

and parse_T2 =
  fix
  @@ fun parse_T2 ->
  let variant_1 =
    let* t3 = parse_T3 in
    let* compare_op = parse_bin_op_T3 in
    let* t2 = parse_T2 in
    return @@ Expr_binary_op (compare_op, t3, t2)
  in
  let variant_2 =
    let* t3 = parse_T3 in
    return @@ t3
  in
  let* result = variant_1 <|> variant_2 in
  return @@ result

and parse_T1 =
  fix
  @@ fun parse_T1 ->
  let variant_1 =
    let* t2 = parse_T2 in
    let* and_op = parse_bin_op_T2 in
    let* t1 = parse_T1 in
    return @@ Expr_binary_op (and_op, t2, t1)
  in
  let variant_2 =
    let* t2 = parse_T2 in
    return @@ t2
  in
  let* result = variant_1 <|> variant_2 in
  return @@ result

and parse_bin_op_expression =
  fix
  @@ fun parse_bin_op_expression ->
  let variant_1 =
    let* t1 = parse_T1 in
    let* or_op = parse_bin_op_T1 in
    let* bin_op_expression = parse_bin_op_expression in
    return @@ Expr_binary_op (or_op, t1, bin_op_expression)
  in
  let variant_2 =
    let* t1 = parse_T1 in
    return @@ t1
  in
  let* result = variant_1 <|> variant_2 in
  return @@ result
;;

(* ---------end of binary operators parsing--------- *)
