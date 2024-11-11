(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let is_keyword = function
  | "let" | "match" | "in" | "if" | "then" | "else" | "fun" | "rec" | "true" | "false" ->
    true
  | _ -> false
;;

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let white_space = take_while Char.is_whitespace
let token s = white_space *> string s
let token1 s = white_space *> s
let parse_parens p = token "(" *> p <* token ")"

let parse_const_int =
  let sign = choice [ token "+"; token "-"; token "" ] in
  let num = take_while1 Char.is_digit in
  lift2 (fun s n -> ConstInt (Int.of_string (s ^ n))) sign num
;;

let parse_const_bool =
  choice
    [ token "true" *> return (ConstBool true); token "false" *> return (ConstBool false) ]
;;

let parse_const_string =
  token "\"" *> take_till (Char.equal '\"') <* token "\"" >>| fun s -> ConstString s
;;

let parse_const = choice [ parse_const_int; parse_const_bool; parse_const_string ]
let parse_unar_oper = choice [ token "-" *> return Negative; token "not" *> return Not ]

let parse_ident =
  let parse_first_char =
    satisfy (fun ch -> is_lowercase ch || is_uppercase ch || Char.equal ch '_')
    >>| Char.escaped
  in
  let parse_other_chars =
    take_while (fun ch ->
      is_lowercase ch || is_uppercase ch || is_digit ch || Char.equal ch '_')
  in
  token1 @@ lift2 ( ^ ) parse_first_char parse_other_chars
  >>= fun s -> if is_keyword s then fail "It is not identifier" else return s
;;

let parse_pattern_var = parse_ident >>| fun id -> PatVariable id
let parse_pattern_const = parse_const >>| fun c -> PatConst c

let parse_pattern_tuple parse_pattern =
  parse_parens (sep_by1 (token ",") parse_pattern)
  >>= function
  | [ single ] -> return single
  | first :: second :: rest -> return (PatTuple (first, second, rest))
  | [] -> fail "Empty tuple pattern not allowed"
;;

let parse_pattern =
  fix (fun pat ->
    let pat =
      choice [ parse_pattern_var; parse_pattern_const; parse_pattern_tuple pat ]
    in
    pat)
;;

let parse_left_associative expr oper =
  let rec go acc = lift2 (fun f x -> f acc x) oper expr >>= go <|> return acc in
  expr >>= go
;;

let parse_expr_bin_oper parse_bin_op tkn =
  white_space *> token tkn *> return (fun e1 e2 -> ExpBinOper (parse_bin_op, e1, e2))
;;

let multiply = parse_expr_bin_oper Multiply "*"
let division = parse_expr_bin_oper Division "/"
let plus = parse_expr_bin_oper Plus "+"
let minus = parse_expr_bin_oper Minus "-"

let compare =
  choice
    [ parse_expr_bin_oper Equal "="
    ; parse_expr_bin_oper NotEqual "<>"
    ; parse_expr_bin_oper LowestEqual "<="
    ; parse_expr_bin_oper LowerThan "<"
    ; parse_expr_bin_oper GretestEqual ">="
    ; parse_expr_bin_oper GreaterThan ">"
    ]
;;

let parse_expr_ident = parse_ident >>| fun x -> ExpIdent x
let parse_expr_const = parse_const >>| fun c -> ExpConst c

let parse_expr_branch parse_expr =
  lift3
    (fun cond t f -> ExpBranch (cond, t, f))
    (token "if" *> parse_expr)
    (token "then" *> parse_expr)
    (option None (token "else" *> parse_expr >>| Option.some))
;;

let parse_expr_unar_oper parse_expr =
  parse_unar_oper >>= fun op -> parse_expr >>= fun expr -> return (ExpUnarOper (op, expr))
;;

let parse_expr_list expr =
  let parse_elements = sep_by (token ";") expr in
  token "[" *> parse_elements <* token "]" >>| fun elements -> ExpList elements
;;

let parse_expr_function parse_expr =
  parse_left_associative parse_expr (return (fun x y -> ExpFunction (x, y)))
;;

let parse_expr_lambda parse_expr =
  token "fun" *> sep_by1 white_space parse_pattern
  <* token "->"
  >>= fun params -> parse_expr >>| fun body -> ExpLambda (params, body)
;;

let parse_expr_let parse_expr =
  let rec parse_body parse_expr =
    parse_pattern
    >>= fun pat ->
    parse_body parse_expr <|> (token "=" *> parse_expr >>| fun e -> ExpLambda ([ pat ], e))
  in
  token "let"
  *> lift4
       (fun is_rec pat e1 e2 -> ExpLet (is_rec, pat, e1, e2))
       (token "rec" *> return true <|> return false)
       (parse_parens parse_pattern <|> parse_pattern)
       (token "=" *> parse_expr <|> parse_body parse_expr)
       (token "in" *> parse_expr >>| Option.some <|> return None)
;;

let parse_expr_tuple expr =
  parse_parens (sep_by1 (token ",") expr)
  >>= function
  | [ single ] -> return single
  | first :: second :: rest -> return (ExpTuple (first, second, rest))
  | [] -> fail "Empty tuple"
;;

let parse_expr =
  fix (fun expr ->
    let expr = choice [ parse_expr_ident; parse_expr_const; parse_parens expr ] in
    let expr = parse_expr_tuple expr <|> expr in
    let expr = parse_expr_function expr <|> expr in
    let expr = parse_left_associative expr (multiply <|> division) in
    let expr = parse_left_associative expr (plus <|> minus) in
    let expr = parse_left_associative expr compare in
    let expr = parse_expr_unar_oper expr <|> expr in
    let expr = parse_expr_branch expr <|> expr in
    let expr = parse_expr_list expr <|> expr in
    let expr = parse_expr_lambda expr <|> expr in
    let expr = parse_expr_let expr <|> expr in
    expr)
;;

let parse_program =
  let definitions =
    many (parse_expr_let parse_expr <* option () (token ";;" >>| ignore))
  in
  definitions <* white_space
;;

let parse input = parse_string ~consume:All parse_program input
