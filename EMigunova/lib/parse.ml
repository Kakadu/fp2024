(** Copyright 2024, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** In this file, we define a number of auxiliary Boolean functions,
    and then, using them, define mini-parsers 
    for parsing the identifiers and strings *)

(** TO DO: добавить парсер для char *)

open Angstrom
open Ast
open Printf

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
  | Expr_tuple of expression list
  | Expr_binary_op of binary_op * expression * expression
  | Expr_if_then_else of expression * expression * expression
  | Expr_match_with of pattern * ( (pattern * expression)  list )
  | Expr_construct_in of let_binding * expression
  | Expr_application of ident * ( expression list )
  | Expr_anonym_fun of pattern * expression         *)

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
    [ token "=" *> return Equal
    ; token "<=" *> return LessEqual
    ; token ">=" *> return GreaterEqual
    ; token ">" *> return Greater
    ; token "<" *> return Less
    ; (token "!=" <|> token "<>") *> return NotEqual
    ]
;;

let parse_bin_op_T4 = choice [ token "+" *> return Plus; token "-" *> return Sub ]
let parse_bin_op_T5 = choice [ token "*" *> return Mul; token "/" *> return Div ]

(* ---------binary operators parser--------- *)

let parse_bin_op_expression parse_expression =
  let parse_expression_base =
    parse_expr_base_elements
    <?> "case 5"
    <|> round_par @@ parse_expression
    >>= fun result -> return result
  in
  let parse_expression_mul_div =
    fix
    @@ fun parse_expression_mul_div ->
    parse_expression_base
    <?> "case 4"
    >>= fun left ->
    parse_bin_op_T5
    >>= (fun op ->
    parse_expression_mul_div >>= fun right -> return (Expr_binary_op (op, left, right)))
    <|> return left
  in
  let parse_expression_add_sub =
    fix
    @@ fun parse_expression_add_sub ->
    parse_expression_mul_div
    <?> "case 3"
    >>= fun left ->
    parse_bin_op_T4
    >>= (fun op ->
    parse_expression_add_sub >>= fun right -> return (Expr_binary_op (op, left, right)))
    <|> return left
  in
  let parse_expression_compare =
    fix
    @@ fun parse_expression_compare ->
    parse_expression_add_sub
    <?> "case 2"
    >>= fun left ->
    parse_bin_op_T3
    >>= (fun op ->
    parse_expression_compare >>= fun right -> return (Expr_binary_op (op, left, right)))
    <|> return left
  in
  let parse_expression_and =
    fix
    @@ fun parse_expression_and ->
    parse_expression_compare
    <?> "case 1"
    >>= fun left ->
    parse_bin_op_T2 *> parse_expression_and
    >>= (fun right -> return (Expr_binary_op (And, left, right)))
    <|> return left
  in
  parse_expression_and
  <?> "case 0"
  >>= fun left ->
  parse_bin_op_T1 *> parse_expression
  >>= (fun right -> return (Expr_binary_op (Or, left, right)))
  <|> return left
;;

(* ----if then else parser------ *)

let parse_if_when_else parse_expression =
  let* if_condition = token "if" *> parse_expression in
  let* then_expression = token "then" *> parse_expression in
  let* else_expression =
    token "else" *> parse_expression <|> return @@ Expr_const Const_unit
  in
  return @@ Expr_if_then_else (if_condition, then_expression, else_expression)
;;

(* in case, where else branch doesn't exist [if (...) then (...)] the else_expression
   of Expr_if_then_else constructor has value = Const_unit *)

(* --- match with parser --- *)

let parse_match_with parse_expression =
  let* compared_expression = token "match" *> parse_expression <* token "with" in
  let* pattern_first = (token "|" <|> return "") *> parse_pattern in
  let* return_expression_first = token "->" *> parse_expression in
  let parser_rest =
    token "|" *> parse_pattern
    >>= fun first_element ->
    token "->" *> parse_expression >>| fun second_element -> first_element, second_element
  in
  let* rest = many parser_rest in
  return
  @@ Expr_match_with
       (compared_expression, (pattern_first, return_expression_first) :: rest)
;;

(* ---let-binding parser--- *)

let parse_rec_flag =
  let recursive = token "let" *> token "rec" >>= fun _ -> return Recursive in
  let non_recursive = token "let" >>= fun _ -> return Non_recursive in
  recursive <|> non_recursive
;;

let parse_let_declaration =
  let parser_fun_case =
    let* rec_flag = parse_rec_flag in
    let* fun_ident = parse_id in
    let* arguments_list = many parse_pattern <* token "=" in
    return @@ Let_fun (rec_flag, fun_ident, arguments_list)
  in
  let parser_pattern_case =
    parse_rec_flag
    >>= fun rec_flag ->
    parse_pattern <* token "=" >>= fun pattern -> return @@ Let_pattern (rec_flag, pattern)
  in
  parser_fun_case <|> parser_pattern_case
;;

let parse_let_biding parse_expression =
  let* let_declaration = parse_let_declaration in
  let* let_definition = parse_expression in
  let* let_double_semicolon = token ";;" <|> return "" in
  return @@ Let_binding (let_declaration, let_definition)
;;

(* ---in-construction parser--- *)

let parse_in_construction parse_expression =
  let* parse_let_biding = parse_let_biding parse_expression in
  let* parse_expression = token "in" *> parse_expression in
  return @@ Expr_construct_in (parse_let_biding, parse_expression)
;;

(* ---application parser---*)

let parse_application parse_expression =
  let* fun_ident = parse_id in
  let* fun_arguments =
    many (round_par @@ parse_expression <|> parse_expr_base_elements)
  in
  return @@ Expr_application (fun_ident, fun_arguments)
;;

(* ---anonymous function with keyword "fun"--- *)

let parse_anonymouse_fun parse_expression =
  let* parse_pattern = token "fun" *> parse_pattern in
  let* parse_expression = token "->" *> parse_expression in
  return @@ Expr_anonym_fun (parse_pattern, parse_expression)
;;

(* ---anonymous function with keyword "function"--- *)

let parse_function_fun parse_expression =
  let* parse_keywords = token "function" *> token "->" in
  let* pattern_first = (token "|" <|> return "") *> parse_pattern in
  let* return_expression_first = token "->" *> parse_expression in
  let parser_one_matching =
    token "|" *> parse_pattern
    >>= fun first_element ->
    token "->" *> parse_expression >>| fun second_element -> first_element, second_element
  in
  let* rest = many parser_one_matching in
  return @@ Expr_function_fun ((pattern_first, return_expression_first) :: rest)
;;

(* ---expression parser--- *)
(*---write rec function with fix---*)

(* ---parser of MiniML--- *)
(*---parse list of bidings---*)
