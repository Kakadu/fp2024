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
let round_par_many t = fix @@ fun p -> round_par @@ p <|> t
let round_par_many1 t = round_par_many @@ round_par @@ t

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

let parse_empty_list_pattern =
  let* _ = token "[" *> token "]" in
  return @@ Pattren_empty_list
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
  fix
  @@ fun parse_tuple_pattern ->
  let* first = parse_pattern <|> round_par_many1 @@ parse_tuple_pattern in
  let* rest =
    many1 @@ (token "," *> (parse_pattern <|> round_par_many1 @@ parse_tuple_pattern))
  in
  return @@ Pattern_tuple (first :: rest)
;;

let parse_list_sugar_case_pattern parse_pattern =
  let* first = parse_pattern in
  let* rest = many @@ (token ";" *> parse_pattern) in
  square_par @@ return @@ Pattern_list (first :: rest)
;;

let parse_list_construct_case_pattern parse_pattern =
  fix
  @@ fun parse_list_construct_case_pattern ->
  let* first = parse_pattern <|> round_par_many1 @@ parse_list_construct_case_pattern in
  let* rest =
    many1
    @@ (token "::"
        *> (parse_pattern <|> round_par_many1 @@ parse_list_construct_case_pattern))
  in
  return @@ Pattern_list (first :: rest)
;;

let parse_list_pattern parse_pattern =
  parse_list_sugar_case_pattern parse_pattern
  <|> parse_list_construct_case_pattern parse_pattern
;;

let parse_option_pattern parse_pattern_help parse_pattern =
  let some_pattern_argument_parser =
    choice
      [ parse_pattern_help
      ; (*by this choice we considered the case : Some hd::tl where after parsing we must to get Pattern_option Some hd, no Pattern_option Some (hd::tl) *)
        round_par @@ parse_pattern
      ]
  in
  let parser_some =
    let* pattern_some = token "Some" *> some_pattern_argument_parser in
    return @@ Pattern_option (Some pattern_some)
  in
  let parser_none =
    let* _ = token "None" in
    return @@ Pattern_option None
  in
  parser_some <|> parser_none
;;

let parse_pattern_help parse_pattern =
  fix @@
  fun parse_pattern_help ->
  choice
    [ parse_any_pattern
    ; parse_empty_list_pattern
    ; parse_const_pattern
    ; parse_var_pattern
    ; parse_list_sugar_case_pattern parse_pattern (*recursion problem*)
    ; parse_option_pattern parse_pattern_help parse_pattern 
    ]
;;

let parse_pattern =
  fix
  @@ fun parse_pattern ->
  round_par_many
  @@ (parse_list_construct_case_pattern (parse_pattern_help parse_pattern)
      <|> parse_tuple_pattern (parse_pattern_help parse_pattern)
      <|> parse_pattern_help parse_pattern)
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

(* ---anonymous function with keyword "fun"--- *)

let parse_anonymouse_fun parse_expression =
  let* parse_pattern = token "fun" *> parse_pattern in
  let* parse_expression = token "->" *> parse_expression in
  return @@ Expr_anonym_fun (parse_pattern, parse_expression)
;;

(* ---anonymous function with keyword "function"--- *)

let parse_function_fun parse_expression =
  let* parse_keywords = token "function" in
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

(* ---application parser---*)

let parser_fun_apply parse_expression =
  let* apply_function =
    round_par @@ parse_anonymouse_fun parse_expression
    <|> round_par @@ parse_function_fun parse_expression
  in
  let* fun_arguments =
    many1 (round_par @@ parse_expression <|> parse_expr_base_elements)
  in
  return @@ Expr_application (Apply_function (apply_function, fun_arguments))
;;

let parser_ident_apply parse_expression =
  let* apply_ident = parse_id in
  let* fun_arguments =
    many1 (round_par @@ parse_expression <|> parse_expr_base_elements)
  in
  return @@ Expr_application (Apply_ident (apply_ident, fun_arguments))
;;

let parse_application parse_expression =
  parser_fun_apply @@ parse_expression <|> parser_ident_apply @@ parse_expression
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
  let parse_expr_base =
    parser_ident_apply parse_expression
    <|> parse_expr_base_elements
    <|> parser_fun_apply @@ parse_expression
    <|> round_par @@ parse_expression
    >>= fun result -> return result <?> "base"
  in
  let parse_expr_mul_div =
    let* first_operand = parse_expr_base in
    let rec parse_mul_div_chain =
      fun left_expression ->
      (let* operator = parse_bin_op_T5 in
       let* right_expression = parse_expr_base in
       parse_mul_div_chain @@ Expr_binary_op (operator, left_expression, right_expression))
      <|> return @@ left_expression
      <?> "mul div"
    in
    parse_mul_div_chain first_operand
  in
  let parse_expr_add_sub =
    let* first_operand = parse_expr_mul_div in
    let rec parse_add_sub_chain =
      fun left_expression ->
      (let* operator = parse_bin_op_T4 in
       let* right_expression = parse_expr_mul_div in
       parse_add_sub_chain @@ Expr_binary_op (operator, left_expression, right_expression))
      <|> return @@ left_expression
      <?> "add sub"
    in
    parse_add_sub_chain first_operand
  in
  let parse_expr_compare =
    let* first_operand = parse_expr_add_sub in
    let rec parse_compare_chain =
      fun left_expression ->
      (let* operator = parse_bin_op_T3 in
       let* right_expression = parse_expr_add_sub in
       parse_compare_chain @@ Expr_binary_op (operator, left_expression, right_expression))
      <|> return @@ left_expression
    in
    parse_compare_chain first_operand
  in
  let parse_expr_and =
    let* first_operand = parse_expr_compare in
    let rec parse_and_chain =
      fun left_expression ->
      (let* operator = parse_bin_op_T2 in
       let* right_expression = parse_expr_compare in
       parse_and_chain @@ Expr_binary_op (operator, left_expression, right_expression))
      <|> return @@ left_expression
      <?> "and"
    in
    parse_and_chain first_operand
  in
  let parse_expr_or =
    let* first_operand = parse_expr_and in
    let rec parse_or_chain =
      fun left_expression ->
      (let* operator = parse_bin_op_T1 in
       let* right_expression = parse_expr_and in
       parse_or_chain @@ Expr_binary_op (operator, left_expression, right_expression))
      <|> return @@ left_expression
      <?> "or"
    in
    parse_or_chain first_operand
  in
  parse_expr_or
;;

(* ---expression parser--- *)
(*---write rec function with fix---*)

let parse_expression =
  fix
  @@ fun parse_expression ->
  choice
    [ parse_bin_op_expression parse_expression
    ; parse_if_when_else parse_expression
    ; parse_match_with parse_expression
    ; parse_application parse_expression
    ; parse_function_fun parse_expression
    ; parse_anonymouse_fun parse_expression
    ; parse_in_construction parse_expression
    ]
;;

(* ---parser of MiniML--- *)
(*---parse list of bidings---*)

let parse_programme_sructure = many (parse_let_biding parse_expression)
