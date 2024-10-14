(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

let is_letter c =
  match c with
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "in"
  | "fun"
  | "match"
  | "with"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false" -> true
  | _ -> false
;;

let ws = take_while Char.is_whitespace
let token s = ws *> string s

(* Парсинг чисел *)
let pnumber =
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* digits = take_while1 is_digit in
  return (int_of_string (sign ^ digits))
;;

let pliteral =
  choice
    [ (let* n = pnumber in
       return (ExprLiteral (IntLiteral n)))
    ; token "true" *> return (ExprLiteral (BoolLiteral true))
    ; token "false" *> return (ExprLiteral (BoolLiteral false))
    ; token "()" *> return (ExprLiteral UnitLiteral)
    ; token "nil" *> return (ExprLiteral NilLiteral)
    ]
;;

let id =
  let* first_char = satisfy is_letter in
  let* rest = take_while (fun c -> is_letter c || is_digit c || Char.equal c '_') in
  return (String.make 1 first_char ^ rest)
;;

let pvariable = id >>= fun v -> return (ExprVariable v)

(* Бинарные операции *)
let pbinop =
  choice
    [ token "+" *> return Add
    ; token "-" *> return Sub
    ; token "*" *> return Mul
    ; token "/" *> return Div
    ; token "<" *> return Lt
    ; token ">" *> return Gt
    ; token "=" *> return Eq
    ]
;;

(* Унарные операции *)
let punop pexpression =
  let* op = choice [ token "+"; token "-"; token "not" ] in
  let* expr = pexpression in
  match op with
  | "+" -> return (ExprUnOperation (UnaryPlus, expr))
  | "-" -> return (ExprUnOperation (UnaryMinus, expr))
  | "not" -> return (ExprUnOperation (UnaryNeg, expr))
  | _ -> fail "unsupported unary operator"
;;

(* Условный оператор *)
let pif pexpression =
  let* _ = token "if" in
  let* cond = pexpression in
  let* _ = token "then" in
  let* then_expr = pexpression in
  let* _ = token "else" in
  let* else_expr = pexpression in
  return (ExprIf (cond, then_expr, Some else_expr))
;;

(* let rec *)
let pletrec pexpression =
  let* _ = token "let" in
  let* _ = token "rec" in
  let* id = id in
  let* _ = token "n" in
  let* _ = token "=" in
  let* expr = pexpression in
  let* _ = token ";" in
  return (ExprLet (Rec, [ PVar id, expr ], ExprVariable id))
;;

let primary_expr pexpression =
  choice
    [ pliteral
    ; (* литералы *)
      pif pexpression
    ; (* условные операторы *)
      pletrec pexpression
    ; (* let rec *)
      punop pexpression
    ; (* унарные операции *)
      pvariable (* переменные *)
    ]
;;

let pexpr_with_binop pexpression =
  let* left = primary_expr pexpression in
  (*можно всретить типо 5+5+5*)
  let* op_opt = option None (pbinop >>| fun op -> Some op) in
  match op_opt with
  | None -> return left
  | Some op ->
    let* right = primary_expr pexpression in
    return (ExprBinOperation (op, left, right))
;;

(* Финальный парсер для выражений *)
let pexpression = fix (fun p -> choice [ pexpr_with_binop p; primary_expr p ])
let parse s = parse_string ~consume:Prefix pexpression s

(* Test parsers *)
let test_parse input =
  match Angstrom.parse_string ~consume:Prefix pexpression input with
  | Ok expr ->
    Format.printf "Parsed expression: %s\n" (Ast.show_expression expr);
    true
  | Error err ->
    Format.printf "Parse error: %s\n" err;
    false
;;

(* Функция для тестирования парсинга if-операторов *)
let test_parse_pif input =
  match Angstrom.parse_string ~consume:Prefix (pif pexpression) input with
  | Ok expr ->
    Format.printf "Parsed expression: %s\n" (Ast.show_expression expr);
    true
  | Error err ->
    Format.printf "Parse error: %s\n" err;
    false
;;

let test_parse_unop input =
  match Angstrom.parse_string ~consume:Prefix (punop pexpression) input with
  | Ok expr ->
    Format.printf "Parsed expression: %s\n" (Ast.show_expression expr);
    true
  | Error err ->
    Format.printf "Parse error: %s\n" err;
    false
;;

let test_parse_pnumber input =
  match Angstrom.parse_string ~consume:Prefix pliteral input with
  | Ok expr ->
    Format.printf "Parsed expression: %s\n" (Ast.show_expression expr);
    true
  | Error err ->
    Format.printf "Parse error: %s\n" err;
    false
;;

let test_parse_binop_expr input =
  match Angstrom.parse_string ~consume:Prefix (pexpr_with_binop pexpression) input with
  | Ok expr ->
    Format.printf "Parsed expression: %s\n" (Ast.show_expression expr);
    true
  | Error err ->
    Format.printf "Parse error: %s\n" err;
    false
;;
