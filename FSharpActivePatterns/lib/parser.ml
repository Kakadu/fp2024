(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

(* TECHNICAL FUNCTIONS *)

let is_ws = function
  | ' ' -> true
  | '\n' -> true
  | '\t' -> true
  | _ -> false
;;

let skip_ws = skip_while is_ws

let peek_sep1 =
  peek_char
  >>= fun c ->
  match c with
  | None -> return None
  | Some c ->
    if is_ws c || Char.equal c '(' || Char.equal c ')'
    then return (Some c)
    else fail "need a delimiter"
;;

let skip_ws_sep1 = peek_sep1 *> skip_ws

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec chainr1 e op =
  e >>= fun rest -> op >>= (fun f -> chainr1 e op >>| f rest) <|> return rest
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
  | "with"
  | "and"
  | "_" -> true
  | _ -> false
;;

let p_type =
  skip_ws
  *> char ':'
  *> skip_ws
  *> (choice [ string "int"; string "bool" ] >>| fun var_type -> Some var_type)
  <|> return None
;;

let p_ident =
  let find_string =
    skip_ws
    *> lift2
         ( ^ )
         (take_while1 (function
           | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
           | _ -> false))
         (take_while (function
           | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
           | _ -> false))
  in
  find_string
  >>= fun str ->
  if is_keyword str
  then fail "keywords are not allowed as variable names"
  else p_type >>| fun type_opt -> Ident (str, type_opt)
;;

let p_var = p_ident >>| fun ident -> Variable ident

(*
   let make_list expr1 expr2 = Cons_list (expr1, expr2)

   let cons p_expr =
   skip_ws *> string "::" *> skip_ws *>
   lift2 (fun expr1 expr2 -> Cons_list (expr1, expr2)) p_expr p_expr
   ;;
*)

let p_empty_list =
  skip_ws *> string "[" *> skip_ws *> string "]" *> skip_ws >>= fun _ -> return Empty_list
;;

let make_list expr1 expr2 = Cons_list (expr1, expr2)

let p_cons_list p_expr =
  chainr1 (p_expr <|> p_empty_list) (skip_ws *> string "::" *> skip_ws *> return make_list)
;;

let p_semicolon_list p_expr =
  skip_ws
  *> string "["
  *> skip_ws
  *> fix (fun p_cons_list ->
    p_expr
    <* skip_ws
    <* string "]"
    >>| (fun expr -> Cons_list (expr, Empty_list))
    <|> (p_expr
         <* skip_ws
         <* string ";"
         >>= fun expr -> p_cons_list >>= fun rest -> return (Cons_list (expr, rest)))
    <|> (string "]" *> skip_ws >>= fun _ -> return Empty_list))
;;

let p_list p_expr = p_semicolon_list p_expr <|> p_cons_list p_expr

(* EXPR PARSERS *)
let p_parens p = skip_ws *> char '(' *> skip_ws *> p <* skip_ws <* char ')'
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

let p_tuple p_expr =
  skip_ws
  *> string "("
  *> lift3
       (fun fst snd tail -> Tuple (fst, snd, tail))
       p_expr
       (skip_ws *> string "," *> skip_ws *> p_expr)
       (many (skip_ws *> string "," *> skip_ws *> p_expr))
  <* skip_ws
  <* string ")"
;;

let p_if p_expr =
  lift3
    (fun cond th el -> If_then_else (cond, th, el))
    (skip_ws *> string "if" *> skip_ws_sep1 *> p_expr)
    (skip_ws *> string "then" *> skip_ws_sep1 *> p_expr)
    (skip_ws
     *> string "else"
     *> skip_ws_sep1
     *> (p_expr <* peek_sep1 >>= fun e -> return (Some e))
     <|> return None)
;;

let p_and p_expr =
  skip_ws
  *> string "and"
  *> skip_ws_sep1
  *> lift3
       (fun name args body -> And_bind (name, args, body))
       p_ident
       (many (skip_ws *> p_var))
       (skip_ws *> string "=" *> skip_ws *> p_expr)
;;

(*
   let p_letin p_expr =
   skip_ws
   *> string "let"
   *> skip_ws_sep1
   *> lift4
   (fun rec_flag name args body and_list in_expr ->
   LetIn (rec_flag, name, args, body, and_list, in_expr))
   (string "rec" *> return Rec <|> return Nonrec)
   (p_ident >>= fun ident -> return (Some ident) <|> return None)
   (many (skip_ws *> p_var))
   (skip_ws *> string "=" *> skip_ws *> p_expr)
   <*> skip_ws *> many (skip_ws *> p_and p_expr)
   <*> skip_ws *> string "in" *> skip_ws_sep1 *> p_expr
   ;; *)

let p_letin p_expr =
  skip_ws *> string "let" *> skip_ws_sep1 *> (string "rec" *> return Rec <|> return Nonrec)
  >>= fun rec_flag ->
  p_ident
  >>= (fun name -> return (Some name))
  <|> return None
  >>= fun name_option ->
  skip_ws *> many (skip_ws *> p_var)
  >>= fun args ->
  skip_ws *> string "=" *> skip_ws *> p_expr
  >>= fun body ->
  skip_ws *> many (skip_ws *> p_and p_expr)
  >>= fun and_list ->
  skip_ws *> string "in" *> skip_ws_sep1 *> p_expr
  >>= fun in_expr -> return (LetIn (rec_flag, name_option, args, body, and_list, in_expr))
;;

(*
   let p_let p_expr =
   skip_ws
   *> string "let"
   *> skip_ws_sep1
   *> lift4
   (fun rec_flag name args body and_list -> Let (rec_flag, name, args, body, and_list))
   (string "rec" *> return Rec <|> return Nonrec)
   p_ident
   (skip_ws *> many (skip_ws *> p_var))
   (skip_ws *> string "=" *> p_expr)
   <*> skip_ws *> many (skip_ws *> p_and p_expr)
   ;; *)

let p_let p_expr =
  skip_ws *> string "let" *> skip_ws_sep1 *> (string "rec" *> return Rec <|> return Nonrec)
  >>= fun rec_flag ->
  p_ident
  >>= fun name ->
  skip_ws *> many (skip_ws *> p_var)
  >>= fun args ->
  skip_ws *> string "=" *> skip_ws *> p_expr
  >>= fun body ->
  skip_ws *> many (skip_ws *> p_and p_expr)
  >>= fun and_list -> return (Let (rec_flag, name, args, body, and_list))
;;

let app_first expr =
  skip_ws
  *> fix (fun a_exp ->
    let expr = choice [ p_var; p_if expr; p_letin expr; p_parens a_exp ] in
    expr)
;;

let p_apply expr =
  let* name = app_first expr in
  let rec parse_args acc =
    skip_ws *> choice [ p_var; p_bool; p_int; p_if expr; p_letin expr; p_parens expr ]
    >>= (fun arg -> parse_args (Function_call (acc, arg)))
    <|> return acc
  in
  parse_args name
;;

let p_option p_expr =
  skip_ws *> (string "None" *> return (Option None))
  <|> (skip_ws *> string "Some" *> p_expr >>| fun expr -> Option (Some expr))
;;

let p_expr =
  skip_ws
  *> fix (fun p_expr ->
    let atom = choice [ p_var; p_int; p_bool; p_parens p_expr ] in
    let list = p_list atom <|> atom in
    let tuple = p_tuple list <|> list in
    let if_expr = p_if (p_expr <|> tuple) <|> tuple in
    let letin_expr = p_letin (p_expr <|> if_expr) <|> if_expr in
    let apply = p_apply (p_expr <|> letin_expr) <|> letin_expr in
    let unary = choice [ unary_chain p_not apply; unary_chain unminus apply ] in
    let factor = chainl1 unary (mul <|> div) in
    let term = chainl1 factor (add <|> sub) in
    let comp_eq = chainl1 term (equal <|> unequal) in
    let comp_less = chainl1 comp_eq (less_or_equal <|> less) in
    let comp_gr = chainl1 comp_less (greater_or_equal <|> greater) in
    let comp_and = chainl1 comp_gr log_and in
    let comp_or = chainl1 comp_and log_or in
    let option = p_option comp_or <|> comp_or in
    option)
;;

let p_statement = p_let p_expr

let p_construction =
  p_expr >>= (fun e -> return (Expr e)) <|> (p_statement >>= fun s -> return (Statement s))
;;

(* MAIN PARSE FUNCTION *)
let parse (str : string) =
  match parse_string ~consume:All (skip_ws *> p_construction <* skip_ws) str with
  | Ok ast -> Some ast
  | Error _ -> None
;;