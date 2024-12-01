(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open MiniML.Ast

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
  if is_keyword name
  then fail "Error! parse_name: Name must not match the keyword."
  else return name
;;

(* ========================== literals ========================= *)

let parse_int =
  clean_up
  @@
  let* sign = char '+' *> return 1 <|> char '-' *> return (-1) <|> return 1 in
  let* digit = take_while1 is_digit >>| int_of_string in
  return @@ Int (sign * digit)
;;

let parse_str =
  let parse_empty_string = string "{||}" >>| fun _ -> "" in
  let parse_content = string "{|" *> take_till (Char.equal '|') <* string "|}" in
  let* str = parse_empty_string <|> parse_content in
  return @@ Str str
;;

let parse_bool =
  let* parsed_bool =
    choice [ token "true" *> return true; token "false" *> return false ]
  in
  return @@ Bool parsed_bool
;;

let parse_unit =
  let* _ = token "()" in
  return Unit
;;

let parse_literal = parse_int <|> parse_str <|> parse_bool <|> parse_unit

(* ========================== patterns ========================= *)

let parse_pat_var =
  let* name = parse_name in
  return @@ PatVar name
;;

let parse_pat_any =
  let* _ = token "_" in
  return @@ PatAny
;;

let parse_pat_lit =
  let* literal = parse_literal in
  return @@ PatLiteral literal
;;

let parse_pat_tup parse_pattern =
  round_parens @@ sep_by1 (token ",") parse_pattern
  >>= function
  | first :: second :: rest -> return @@ PatTuple (first, second, rest)
  | _ -> fail "Error! parse_pat_tuple: Tuple pattern must have at least two elements."
;;

let parse_pattern =
  let parse_body parse_pattern =
    choice [ parse_pat_var; parse_pat_any; parse_pat_lit; parse_pat_tup parse_pattern ]
  in
  fix parse_body
;;

(* ====================== binary operations ==================== *)

let parse_bin_op op_token op_constructor =
  clean_up
  @@ (token op_token *> (return @@ fun exp1 exp2 -> ExpBinOp (op_constructor, exp1, exp2)))
;;

let parse_mul = parse_bin_op "*" Mul
let parse_add = parse_bin_op "+" Add
let parse_sub = parse_bin_op "-" Sub
let parse_div = parse_bin_op "/" Div
let parse_equal = parse_bin_op "=" Eq
let parse_non_equal = parse_bin_op "<>" NonEq
let parse_less = parse_bin_op "<" Lt
let parse_less_equal = parse_bin_op "<=" LtEq
let parse_greater = parse_bin_op "=" Gt
let parse_greater_equal = parse_bin_op "=" GtEq
let parse_and = parse_bin_op "&&" And
let parse_or = parse_bin_op "||" Or

(* ======================== expressions ======================== *)
let parse_expr_var =
  let* var = parse_name in
  return @@ ExpVar var
;;

let parse_expr_literal =
  let* literal = parse_literal in
  return @@ ExpConst literal
;;

let parse_expr_list parse_expr =
  square_brackets
  @@
  let* first_element = parse_expr in
  let* rest_list = many (token ";" *> parse_expr) in
  return @@ ExpList (first_element :: rest_list)
;;

let parse_expr_tuple parse_expr =
  round_parens
  @@
  let* exp1 = parse_expr in
  let* _ = token "," in
  let* exp2 = parse_expr in
  let* rest = many (token "," *> parse_expr) in
  return (ExpTuple (exp1, exp2, rest))
;;

let parse_expr_lambda parse_expr =
  let rec parse_body parse_expr =
    let* lambda_name = parse_name in
    let* _ = token "->" in
    let* exp = parse_expr in
    return @@ ExpLambda (lambda_name, exp)
  in
  token "fun" *> parse_body parse_expr
;;

let parse_expr_let parse_expr =
  let rec parse_let_bindings parse_expr =
    let* name = parse_name in
    let* _ = token "=" in
    let* expr = parse_expr in
    return @@ (name, expr)
  in
  token "let"
  *>
  let* is_rec = token "rec" *> return Rec <|> return NonRec in
  let* expr = many @@ parse_let_bindings parse_expr in
  let* in_expr = token "in" *> parse_expr <|> return ExpUnit in
  return @@ ExpLet (is_rec, expr, in_expr)
;;

let parse_exp_app pexpr =
  let operator = return @@ fun exp1 exp2 -> ExpApp (exp1, exp2) in
  chainl1 pexpr operator
;;

let parse_exp_branch parse_expr =
  let* if_cond = token "if" *> parse_expr in
  let* then_cond = token "then" *> parse_expr in
  let* else_cond = token "else" *> parse_expr >>| (fun exp1 -> Some exp1) <|> None in
  return @@ ExpBranch (if_cond, then_cond, else_cond)
;;

let parse_expr =
  fix
  @@ fun expr ->
  let base_expr =
    choice
      [ parse_expr_literal
      ; parse_expr_var
      ; round_parens expr
      ; parse_expr_list expr
      ; parse_expr_lambda expr
      ]
  in
  let expr_applications = parse_exp_app base_expr <|> base_expr in
  let expr_mul_div = chainl1 expr_applications (parse_mul <|> parse_div) in
  let expr_add_sub = chainl1 expr_mul_div (parse_add <|> parse_sub) in
  let expr_compare =
    chainl1
      expr_add_sub
      (parse_equal
       <|> parse_less
       <|> parse_less_equal
       <|> parse_greater
       <|> parse_greater_equal
       <|> parse_non_equal)
  in
  let expr_logical = chainr1 expr_compare (parse_or <|> parse_and) in
  let expr_tuples = parse_expr_tuple expr_logical <|> expr_logical in
  let expr_branches = parse_exp_branch expr_tuples <|> expr_tuples in
  let expr_let = parse_expr_let expr_branches <|> expr_branches in
  expr_let
;;
