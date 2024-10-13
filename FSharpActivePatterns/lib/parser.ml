(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let skip_ws =
  skip_while (function
    | ' ' -> true
    | '\n' -> true
    | '\t' -> true
    | _ -> false)
;;

let skip_ws1 = char ' ' *> skip_ws
let parse_parens p = skip_ws *> char '(' *> skip_ws *> p <* skip_ws <* char ')' <* skip_ws
let add = skip_ws *> char '+' *> skip_ws *> return Binary_add
let sub = skip_ws *> char '-' *> skip_ws *> return Binary_subtract
let mul = skip_ws *> char '*' *> skip_ws *> return Binary_multiply
let div = skip_ws *> char '/' *> skip_ws *> return Binary_divide
let equal = skip_ws *> char '=' *> skip_ws *> return Binary_equal
let unequal = skip_ws *> string "<>" *> skip_ws *> return Binary_unequal
let less = skip_ws *> char '<' *> skip_ws *> return Binary_less
let less_or_equal = skip_ws *> string "<=" *> skip_ws *> return Binary_less_or_equal
let greater = skip_ws *> char '>' *> skip_ws *> return Binary_greater
let greater_or_equal = skip_ws *> string ">=" *> skip_ws *> return Binary_greater_or_equal
let log_or = skip_ws *> string "||" *> skip_ws *> return Logical_or
let log_and = skip_ws *> string "&&" *> skip_ws *> return Logical_and
let log_not = skip_ws *> string "not" *> skip_ws *> return Unary_negative

let parse_integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| fun s -> Const (Int_lt (int_of_string s))
;;

let parse_ident = 
  take_while1 (function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false)
  >>| fun s -> Ident(s)

let parse_word = parse_ident >>| (fun ident -> Variable(ident))

(** find full chain of left-associated expressions on the same level of associativity, such as a-b+cc or a*b/c *)
let parse_binary_chainl1 e op =
  let rec go acc = lift2 (fun f x -> Bin_expr (f, acc, x)) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(** parse exactly one infix binary operation and returns Bin_expr (bin_op, e1, e2) *)
let parse_binary1 e op = lift3 (fun e1 bin_op e2 -> Bin_expr (bin_op, e1, e2)) e op e

(** parse chain of unary left-associated expressions, such as - + - - 3 and returns Unary_expr (f, expr) *)
let rec parse_unary_chain e op =
  op
  >>= (fun un_op ->
        parse_unary_chain e op >>= fun expr -> return (Unary_expr (un_op, expr)))
  <|> e
;;

(** bool [b] accepts boolean_literal [b] and returns Const Bool_lt from it*)
let parse_bool =
  skip_ws *> string "true"
  <|> string "false"
  >>| fun s -> Const (Bool_lt (bool_of_string s))
;;

(** parse string literal [s] without escaping symbols and returns Const (String_lt [s]) *)
let string_expr =
  skip_ws *> char '\"' *> take_while (fun c -> c <> '\"') >>| fun s -> Const (String_lt s)
;;

(** parse integer expression, such as [(3 + 5) * (12 - 5)] and returns Binary_expr (f, e1, e2) *)
let int_expr : expr t =
  fix (fun expr ->
    let factor = skip_ws *> (parse_parens expr <|> parse_integer <|> parse_word) <* skip_ws in
    let term = parse_binary_chainl1 factor (mul <|> div) in
    parse_binary_chainl1 term (add <|> sub))
;;

(** parse comparison expression with integers, bool literals and strings and return Bin_expr(comp_op, e1, e2) *)
let comparison_expr : expr t =
  parse_binary1
    int_expr
    (less_or_equal <|> greater_or_equal <|> unequal <|> less <|> greater <|> equal)
  <|> parse_binary1 (int_expr <|> parse_bool <|> string_expr) (equal <|> unequal)
;;

(** parse bool_expr, such as [3 > 2 || true <> false && 12 > 7] and returns boolean expr*)
let bool_expr : expr t =
  fix (fun expr ->
    let level1 = skip_ws *> (parse_parens expr <|> parse_bool <|> comparison_expr) <* skip_ws in
    let level2 = parse_unary_chain level1 log_not in
    let level3 = parse_binary_chainl1 level2 (equal <|> unequal) in
    let level4 = parse_binary_chainl1 level3 log_and in
    parse_binary_chainl1 level4 log_or)
;;

let parse_expr parse_statement = 
  choice
  [
    parse_parens parse_statement;
    bool_expr;
    string_expr;
    int_expr;
  ]
;;

let parse_if parse_statement =
  fix 
  (fun parse_if -> 
    lift3
      (fun cond th el -> If_then_else (cond, th, el))
      (skip_ws *> string "if" *> skip_ws1 *> bool_expr)
      (skip_ws *> string "then" *> (parse_if <|> parse_statement))
      (skip_ws *> string "else" *> ((parse_if <|> parse_statement) >>= fun e -> return (Some e)) <|> return None)
  )
;; 

let parse_let parse_statement = 
  skip_ws
  *> string "let"
  *> skip_ws1
  *> lift4 
       (fun rec_flag name args body in_expr -> LetIn(rec_flag, name, args, body, in_expr))
       (string "rec" *> return Rec <|> return Nonrec)
       (skip_ws *> parse_ident >>= fun ident -> return (Some(ident)) <|> return None)
       (skip_ws *> many (parse_expr parse_statement) >>= fun args -> if List.length args > 0 then return (Some(args)) else return None)
       (skip_ws *> string "=" *> skip_ws *> parse_statement) <*>
       (skip_ws *> string "in" *> (parse_statement >>= fun e -> return (Some e)) <|> return None)
;;

let parse_statement = 
  fix
  (fun parse_statement -> 
  let statement = parse_let parse_statement in
  let statement = parse_if statement <|> statement in 
  let statement = parse_expr statement <|> statement in
  statement)
;;

let parse (str : string) : expr =
  match parse_string ~consume:All parse_statement str with
  | Ok v -> v
  | Error msg -> failwith msg
;;
