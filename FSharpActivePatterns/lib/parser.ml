(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base
(*open PrintAst*)

(*
let debug p =
  p >>= fun result ->
  print_expr 0 result;
  return result
;;*)

(* TECHNICAL FUNCTIONS *)
let skip_ws =
  skip_while (function
    | ' ' -> true
    | '\n' -> true
    | '\t' -> true
    | _ -> false)
;;

let skip_ws1 = char ' ' *> skip_ws

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec unary_chain op e =
  op >>= (fun unexpr -> unary_chain op e >>= fun expr -> return (unexpr expr)) <|> e
;;

(* SIMPLE PARSERS *)
let p_int =
  skip_ws *> take_while1 Char.is_digit >>| fun s -> Const (Int_lt (Int.of_string s))
;;

let p_bool =
  skip_ws *> string "true"
  <|> string "false"
  >>| fun s -> Const (Bool_lt (Bool.of_string s))
;;

let is_keyword = function
  | "if"
  | "then"
  | "else"
  | "let"
  | "in"
  | "not"
  | "true"
  | "false"
  | "fun"
  | "match"
  | "with" -> true
  | _ -> false
;;

let p_ident =
  let find_string = 
    skip_ws *>
    lift2 (fun s1 s2 -> s1 ^ s2)
    (take_while1 (function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false))
    (take_while (function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
    | _ -> false))
  in 
  find_string >>= fun str -> 
    if is_keyword str then fail "keywords are not allowed as variable names"
    else if String.equal str "_" then fail "wildcard is not allowed as a variable name"
    else return (Ident str)
;; 

let p_var = p_ident >>| fun ident -> Variable ident

(* EXPR PARSERS *)
let p_parens p = skip_ws *> char '(' *> skip_ws *> p <* skip_ws <* char ')' <* skip_ws
let make_binexpr op expr1 expr2 = Bin_expr (op, expr1, expr2) [@@inline always]
let make_unexpr op expr = Unary_expr (op, expr) [@@inline always]

let p_binexpr binop_str binop =
  skip_ws *> string binop_str *> skip_ws *> return (make_binexpr binop)
;;

let p_unexpr unop_str unop =
  skip_ws *> string unop_str *> skip_ws *> return (make_unexpr unop)
;;

let p_not = p_unexpr "not" Unary_not
let unminus = p_unexpr "-" Unary_minus
let add = p_binexpr "+" Binary_add
let sub = p_binexpr "-" Binary_subtract
let mul = p_binexpr "*" Binary_multiply
let div = p_binexpr "/" Binary_divide
let equal = p_binexpr "=" Binary_equal
let unequal = p_binexpr "<>" Binary_unequal
let less = p_binexpr "<" Binary_less
let less_or_equal = p_binexpr "<=" Binary_less_or_equal
let greater = p_binexpr ">" Binary_greater
let greater_or_equal = p_binexpr ">=" Binary_greater_or_equal
let log_or = p_binexpr "||" Logical_or
let log_and = p_binexpr "&&" Logical_and

let p_if p_expr =
  lift3
    (fun cond th el -> If_then_else (cond, th, el))
    (skip_ws *> string "if" *> skip_ws1 *> (p_expr))
    (skip_ws *> string "then" *> skip_ws1 *> (p_expr))
    (skip_ws
       *> string "else"
       *> skip_ws1
       *> (p_expr >>= fun e -> return (Some e))
       <|> return None)
;;

let p_letin p_expr =
    skip_ws
    *> string "let"
    *> skip_ws1
    *> lift4
         (fun rec_flag name args body in_expr ->
           LetIn (rec_flag, name, args, body, in_expr))
         (string "rec" *> return Rec <|> return Nonrec)
         (p_ident >>= fun ident -> return (Some ident) <|> return None)
         (many (skip_ws *> p_var)
          >>= fun args ->
          if not (List.length args = 0) then return (Some args) else return None)
         (skip_ws *> string "=" *> skip_ws *> p_expr)
    <*> skip_ws *> string "in" *> skip_ws *> p_expr
;;

let p_let p_expr =
  skip_ws
  *> string "let"
  *> skip_ws1
  *> lift4
       (fun rec_flag name args body -> Let (rec_flag, name, args, body))
       (string "rec" *> return Rec <|> return Nonrec)
       (p_ident >>= fun ident -> return ident)
       (skip_ws *> many (skip_ws *> p_var)
        >>= fun args ->
        if not (List.length args = 0) then return (Some args) else return None)
       (skip_ws *> string "=" *> skip_ws *> p_expr)
;;


let app_first expr = 
  skip_ws *> 
  fix (fun a_exp -> 
    let expr = choice [p_var; p_if expr; p_parens a_exp] in
    expr
    )
;;

let p_apply expr =
  let* name = app_first expr in
  let rec parse_args acc =
    (skip_ws *> expr >>= fun arg ->
     parse_args (Function_call (acc, arg))) <|> return acc
  in
  parse_args name
;; 
(*
let p_apply expr =
  (*Printf.printf "\n\n\n\n here"; *)
  chainl1 expr (return (fun f arg -> Function_call (f, arg)))
;; *)

let p_expr =
  skip_ws
  *> fix (fun p_expr ->
    let atom = choice [ p_var; p_int; p_bool; p_parens p_expr ] in
    let if_expr = p_if (p_expr <|> atom) <|> atom in
    let letin_expr = p_letin (p_expr <|> if_expr) <|> if_expr in
    let app = p_apply letin_expr <|> letin_expr in
    let factor = chainl1 app (mul <|> div) in
    let term = chainl1 factor (add <|> sub) in
    let comp_eq = chainl1 term (equal <|> unequal) in
    let comp_less = chainl1 comp_eq (less_or_equal <|> less) in
    let comp_gr = chainl1 comp_less (greater_or_equal <|> greater) in
    let comp_and = chainl1 comp_gr log_and in
    let comp_or = chainl1 comp_and log_or in
    let unary = choice [ unary_chain p_not comp_or; unary_chain unminus comp_or ] in
    unary)
;;

let p_statement = p_let p_expr

let p_construction =
  p_expr >>= (fun e -> return (Expr e)) <|> (p_statement >>= fun s -> return (Statement s))
;;

(* MAIN PARSE FUNCTION *)
let parse (str : string) : construction option =
  match parse_string ~consume:All (skip_ws *> p_construction <* skip_ws) str with
  | Ok ast -> Some ast
  | Error _ -> None
;;
