(** Copyright 2024, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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
  | "function"
  | "_" -> true
  | _ -> false
;;

let skip_sep = skip_while is_sep
let trim t = skip_sep *> t <* skip_sep
let token t = skip_sep *> string t <* skip_sep
let round_par p = token "(" *> p <* token ")"
let square_par p = token "[" *> p <* token "]"
let round_par_many t = fix @@ fun p -> trim @@ t <|> round_par @@ p
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
  else if id = "_"
  then fail "wildcard \"_\" not expected"
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
  let* _ = trim @@ satisfy (fun ch -> Char.equal '_' ch) in
  return @@ Pattern_any
;;

let parse_var_pattern =
  let* var = trim @@ parse_id in
  return @@ Pattern_var var
;;

let parse_const_pattern =
  let parse_neg_int =
    trim @@ (token "-" *> take_while1 is_digit >>| fun x -> Const_int (-int_of_string x))
  in
  let* const = trim @@ (parse_const <|> parse_neg_int) in
  return @@ Pattern_const const
;;

let parse_base_pattern =
  choice [ parse_var_pattern; parse_any_pattern; parse_const_pattern ]
;;

let parse_list_sugar_case_pattern parse_pattern =
  let empty_list_parser =
    let* _ = token "[" *> token "]" in
    return @@ Pattern_list_sugar_case []
  in
  let list_parser =
    let* _ = token "[" in
    let* first = parse_pattern in
    let* rest = many1 (token ";" *> parse_pattern) in
    let* _ = token "]" in
    return @@ Pattern_list_sugar_case (first :: rest)
  in
  empty_list_parser <|> list_parser
;;

let parse_list_construct_case_pattern parse_pattern =
  let* first = parse_pattern in
  let* rest = many1 @@ (token "::" *> parse_pattern) in
  return @@ Pattern_list_constructor_case (first :: rest)
;;

let parse_tuple_pattern parse_pattern =
  let* first = parse_pattern in
  let* rest = many1 @@ (token "," *> parse_pattern) in
  return @@ Pattern_tuple (first :: rest)
;;

let parse_option_pattern parser_for_argument_of_some =
  let parser_some =
    let* pattern_some = token "Some" *> parser_for_argument_of_some in
    return @@ Pattern_option (Some pattern_some)
  in
  let parser_none =
    let* _ = token "None" in
    return @@ Pattern_option None
  in
  parser_some <|> parser_none
;;

let parse_pattern =
  fix
  @@ fun parse_pattern ->
  round_par_many
  @@
  let parse_option_argument =
    fix
    @@ fun parse_option_argument ->
    round_par_many
    @@ choice
         [ parse_base_pattern
         ; parse_option_pattern parse_option_argument
         ; parse_list_sugar_case_pattern parse_pattern
         ]
    <|> round_par_many1 @@ parse_pattern
  in
  let parse_list_construct_element =
    round_par_many
    @@ choice
         [ parse_base_pattern
         ; parse_option_pattern parse_option_argument
         ; parse_list_sugar_case_pattern parse_pattern
         ]
    <|> round_par_many1 @@ parse_pattern
  in
  let parse_tuple_element =
    round_par_many
    @@ choice
         [ parse_list_construct_case_pattern parse_list_construct_element
         ; parse_base_pattern
         ; parse_option_pattern parse_option_argument
         ; parse_list_sugar_case_pattern parse_pattern
         ]
    <|> round_par_many1 @@ parse_pattern
  in
  round_par_many
  @@ choice
       [ parse_tuple_pattern parse_tuple_element
       ; parse_list_construct_case_pattern parse_list_construct_element
       ; parse_base_pattern
       ; parse_option_pattern parse_option_argument
       ; parse_list_sugar_case_pattern parse_pattern
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

let parse_expr_list_sugar parse_expr =
  let empty_list_parse = token "[" *> token "]" *> (return @@ Expr_list_sugar []) in
  let non_empty_list_parse =
    square_par
    @@
    let* first = parse_expr in
    let* other = many (token ";" *> parse_expr) in
    return @@ Expr_list_sugar (first :: other)
  in
  empty_list_parse <|> non_empty_list_parse
;;

let parse_expr_option parse_expression =
  fix
  @@ fun parse_expr_option ->
  let parse_some =
    let* _ = token "Some" in
    let* argument =
      round_par_many
      @@ choice
           [ parse_expr_const; parse_expr_var; parse_expr_list_sugar parse_expression ]
      <|> round_par_many1 @@ choice [ parse_expr_option; parse_expression ]
    in
    return @@ Expr_option (Some argument)
  in
  let parse_none =
    let* _ = token "None" in
    return @@ Expr_option None
  in
  parse_some <|> parse_none
;;

(* parsing of constants, vars, lists and tuples of constants and vars *)
let parse_expr_base_elements parse_expression =
  round_par_many
  @@ choice
       [ parse_expr_const
       ; parse_expr_var
       ; parse_expr_option parse_expression
       ; parse_expr_list_sugar parse_expression
       ]
;;

(*let's add ability to specify type annotations*)

let parse_type =
  fix
  @@ fun parse_type ->
  let base_type_parser =
    round_par_many
    @@ choice
         [ token "int" *> return Type_int
         ; token "char" *> return Type_char
         ; token "bool" *> return Type_bool
         ; token "string" *> return Type_string
         ; token "unit" *> return Type_unit
         ]
  in
  let tuple_type_parser parser_tuple_element_type =
    round_par_many
    @@
    let* first = parser_tuple_element_type in
    let* rest = many1 @@ (token "*" *> parser_tuple_element_type) in
    return @@ Type_tuple (first :: rest)
  in
  let parser_tuple_element_type =
    round_par_many @@ base_type_parser <|> round_par_many1 @@ parse_type
  in
  let list_type_parser =
    fix
    @@ fun list_type_parser ->
    let* element_type =
      choice
      @@ [ tuple_type_parser parser_tuple_element_type
         ; base_type_parser
         ; round_par_many1 list_type_parser
         ]
    in
    let rec list_type_parser element_type =
      (let* _ = token "list" in
       list_type_parser @@ Type_list element_type)
      <|> return @@ element_type
    in
    list_type_parser element_type
  in
  choice
    [ list_type_parser; tuple_type_parser parser_tuple_element_type; base_type_parser ]
;;

(*let parse_expression_without_tuple_list =
  (*now we can parse expressions with type annotations, but still can't parse tuple and list_constructor_case constructions*)
  round_par_many @@ parse_expression_without_type_annotation
  <|> round_par_many1
      @@
      let* expression = parse_expression_without_type_annotation in
      let* ttype = token ":" *> parse_type in
      return @@ Typed_expression (ttype, expression)
;;*)

let type_annotation expression_parser =
  (round_par_many1
   @@
   let* expression = expression_parser in
   let* ttype = token ":" *> parse_type in
   return @@ Typed_expression (ttype, expression))
  <|> round_par_many @@ expression_parser
;;

let type_annotation1 expression_parser =
  round_par_many1
  @@
  let* expression = expression_parser in
  let* ttype = token ":" *> parse_type in
  return @@ Typed_expression (ttype, expression)
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
    let* fun_ident = parse_id in
    let* arguments_list = many parse_pattern <* token "=" in
    return @@ Let_fun (fun_ident, arguments_list)
  in
  let parser_pattern_case =
    parse_pattern <* token "=" >>= fun pattern -> return @@ Let_pattern pattern
  in
  parser_fun_case <|> parser_pattern_case
;;

let parse_let_biding parse_expression =
  let* rec_flag = parse_rec_flag in
  let* let_declaration = parse_let_declaration in
  let* let_definition = parse_expression in
  return @@ Let_binding (rec_flag, let_declaration, let_definition)
;;

(* ---parser of mutually recursive let-bindings--- *)

let parse_let_rec_and_binding parse_expression =
  let parse_first =
    let* _ = token "let" *> token "rec" in
    let* let_declaration = parse_let_declaration in
    let* let_definition = parse_expression in
    return @@ Let_binding (Recursive, let_declaration, let_definition)
  in
  let parse_one_of_rest =
    let* _ = token "and" in
    let* let_declaration = parse_let_declaration in
    let* let_definition = parse_expression in
    return @@ Let_binding (Recursive, let_declaration, let_definition)
  in
  let* first = parse_first in
  let* rest = many1 @@ parse_one_of_rest in
  return @@ Let_rec_and_binding (first :: rest)
;;

(* ---in-construction parser--- *)

let parse_in_construction parse_expression =
  let* parse_let_biding = parse_let_biding parse_expression in
  let* parse_expression = token "in" *> parse_expression in
  return @@ Expr_construct_in (parse_let_biding, parse_expression)
;;

(* ---anonymous function with keyword "fun"--- *)

let parse_anonymouse_fun parse_expression =
  let parse_argument =
    round_par_many
    @@ choice
    @@ [ parse_base_pattern
       ; parse_list_sugar_case_pattern parse_pattern
       ; token "None" *> (return @@ Pattern_option None)
       ]
    <|> round_par_many1 @@ parse_pattern
  in
  let* list_of_arguments = token "fun" *> (many1 @@ parse_argument) in
  let* parse_expression = token "->" *> (round_par_many @@ parse_expression) in
  return @@ Expr_anonym_fun (list_of_arguments, parse_expression)
;;

(* ---anonymous function with keyword "function"--- *)

let parse_function_fun parse_expression =
  let* _ = token "function" in
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

let parse_application parse_expression =
  round_par_many
  @@
  let parse_application_element =
    round_par_many
    @@ choice
    @@ [ parse_expr_const; parse_expr_var; parse_expr_list_sugar parse_expression ]
    <|> round_par_many1 @@ parse_expression
    <|> type_annotation1 @@ parse_expression
  in
  let* first = parse_application_element in
  let* rest = many1 @@ parse_application_element in
  return @@ Expr_application (first, rest)
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
    parse_application parse_expression
    <|> parse_expr_base_elements parse_expression
    <|> round_par_many1
        @@ parse_expression (*съедаются скобки, из-за чего мы не можем потом распарсить *)
    <|> type_annotation1 @@ parse_expression
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
    let rec parse_add_sub_chain =
      fun left_expression ->
      (let* operator = parse_bin_op_T4 in
       let* right_expression = parse_expr_mul_div in
       parse_add_sub_chain @@ Expr_binary_op (operator, left_expression, right_expression))
      <|> return @@ left_expression
    in
    (let* first_operand = parse_expr_mul_div in
     parse_add_sub_chain first_operand)
    <|>
    (*implemention of negative int*)
    let* operator = parse_bin_op_T4 in
    let* right_expression = parse_expr_mul_div in
    parse_add_sub_chain
    @@ Expr_binary_op (operator, Expr_const (Const_int 0), right_expression)
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

let parse_expression_without_tuple_list parse_expression =
  (*also this implemention doesn't consider tuple and list_constructor_case constructions, then we will add it*)
  type_annotation
  @@ choice
       [ parse_bin_op_expression parse_expression
       ; parse_if_when_else parse_expression
       ; parse_match_with parse_expression
       ; parse_function_fun parse_expression
       ; parse_anonymouse_fun parse_expression
       ; parse_in_construction parse_expression
       ]
;;

(*let's add ability to parse tuple and list_constructor_case constructions*)

let parse_expr_list_construct parse_expression =
  let parse_element =
    round_par_many @@ parse_expression_without_tuple_list parse_expression
    <|> round_par_many1 @@ parse_expression
  in
  let* first = parse_element in
  let* rest = many1 @@ (token "::" *> parse_element) in
  return @@ Expr_list_construct (first :: rest)
;;

let parse_expr_tuple parse_expression =
  let parse_element =
    round_par_many
    @@ choice
         [ parse_expr_list_construct parse_expression
         ; parse_expression_without_tuple_list parse_expression
         ]
    <|> round_par_many1 @@ parse_expression
  in
  let* first = parse_element in
  let* rest = many1 @@ (token "," *> parse_element) in
  return @@ Expr_tuple (first :: rest)
;;

let parse_expression =
  fix
  @@ fun parse_expression ->
  choice
    [ parse_expr_tuple parse_expression
    ; parse_expr_list_construct parse_expression
    ; parse_expression_without_tuple_list parse_expression
    ]
;;

(* ---parser of MiniML--- *)
(*---parse list of bidings---*)

let parse_structure_item =
  let parse_eval =
    let* expr = parse_expression in
    return @@ Struct_eval expr
  in
  let parse_value =
    let* value =
      parse_let_rec_and_binding parse_expression <|> parse_let_biding parse_expression
    in
    return @@ Struct_value value
  in
  parse_eval <|> parse_value
;;

let parse_structure =
  let* item_list = many1 parse_structure_item in
  return @@ item_list
;;

let parse = parse_string ~consume:All parse_structure
