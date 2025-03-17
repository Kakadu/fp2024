(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 292 *)

open Base
open Angstrom
open Ast
open Common
open Patterns
open Constants
open Types

let pexpr_id_or_op = pid_or_op >>| fun i -> Expr_ident_or_op i
let pexpr_const = puconst >>| fun c -> Expr_const c

let pexpr_ite pexp =
  let* cond = skip_ws *> string "if" *> skip_ws *> pexp in
  let* branch1 = skip_ws *> string "then" *> skip_ws *> pexp in
  let* branch2 =
    option None (skip_ws *> string "else" *> skip_ws *> pexp >>| fun e -> Some e)
  in
  return (Expr_ifthenelse (cond, branch1, branch2))
;;

let pexpr_paren pexp = string "(" *> skip_ws *> pexp <* skip_ws <* string ")"
let parse_prefix_app = skip_ws_no_nl *> return (fun e1 e2 -> Expr_apply (e1, e2))

let parse_infix_app op =
  skip_ws
  *> string op
  *> return (fun e1 e2 -> Expr_apply (Expr_apply (Expr_ident_or_op op, e1), e2))
;;

(* Refactor this with explicit operations priorities later *)
let pexpr_app pexpr =
  let app = chainl pexpr parse_prefix_app <|> pexpr in
  let app =
    chainl
      app
      (parse_infix_app "*."
       <|> parse_infix_app "/."
       <|> parse_infix_app "*"
       <|> parse_infix_app "/")
    <|> app
  in
  let app =
    chainl
      app
      (parse_infix_app "+."
       <|> parse_infix_app "-."
       <|> parse_infix_app "+"
       <|> parse_infix_app "-")
    <|> app
  in
  let app =
    chainl
      app
      (parse_infix_app "<="
       <|> parse_infix_app "<"
       <|> parse_infix_app ">="
       <|> parse_infix_app ">")
    <|> app
  in
  let app = chainr app (parse_infix_app "::") <|> app in
  let app = chainr app (parse_infix_app "||" <|> parse_infix_app "&&") <|> app in
  app
;;

(* Makes sense only for + and - for now *)
let pexpr_app_un pexpr =
  let* op = skip_ws *> string "-" <|> string "+" <* skip_ws in
  let* e = pexpr in
  match op with
  | "+" -> return e
  | _ ->
    (match e with
     | Expr_const (Const_int i) -> return (Expr_const (Const_int (-i)))
     | Expr_const (Const_float f) -> return (Expr_const (Const_float (Float.neg f)))
     | Expr_const (Const_unit_of_measure (Unit_of_measure (Mnum_int i, m))) ->
       return @@ Expr_const (Const_unit_of_measure (Unit_of_measure (Mnum_int (-i), m)))
     | Expr_const (Const_unit_of_measure (Unit_of_measure (Mnum_float f, m))) ->
       return
       @@ Expr_const
            (Const_unit_of_measure (Unit_of_measure (Mnum_float (Float.neg f), m)))
     | _ -> return (Expr_apply (Expr_ident_or_op op, e)))
;;

let pexpr_lam pexpr =
  skip_token "fun"
  *>
  let* args = many1 ppat in
  skip_token "->"
  *>
  let* expr = pexpr in
  let rec wrap = function
    | h :: tl -> Expr_lam (h, wrap tl)
    | [] -> expr
  in
  return (wrap args)
;;

let pbind_val pexpr =
  let* name = ppat in
  skip_token "="
  *>
  let* expr = pexpr in
  return (Bind (name, expr))
;;

let pbind_fun pexpr =
  let* name = ppat_id_or_op in
  let* args = many1 (skip_ws *> ppat) in
  skip_token "="
  *>
  let* expr = pexpr in
  let rec wrap = function
    | h :: tl -> Expr_lam (h, wrap tl)
    | [] -> expr
  in
  return (Bind (name, wrap args))
;;

let pbind pexpr = pbind_val pexpr <|> pbind_fun pexpr

let pexpr_letin pexpr =
  skip_token "let"
  *>
  let* rec_flag = option Nonrecursive (skip_token "rec" *> return Recursive) in
  let* binding_fst = pbind pexpr in
  let* binding_rest = many (skip_token "and" *> pbind pexpr) in
  skip_token "in"
  *>
  let* last_expr = pexpr in
  return (Expr_let (rec_flag, binding_fst, binding_rest, last_expr))
;;

(* Parses tuple without parentheses *)
let pexpr_tuple pexpr =
  let* tuple_fst = skip_ws *> pexpr <* skip_ws <* char ',' in
  let* tuple_snd = skip_ws *> pexpr in
  let* tuple_rest = skip_ws *> many (skip_token "," *> pexpr) in
  return (Expr_tuple (tuple_fst, tuple_snd, tuple_rest))
;;

let pexpr_list pexpr =
  let* list = char '[' *> sep_by (char ';') (skip_ws *> pexpr <* skip_ws) <* char ']' in
  return (Expr_list list)
;;

let prules pexpr =
  let parse_rule =
    let* pat = ppat <* skip_token "->" in
    let* expr = pexpr in
    return (Rule (pat, expr))
  in
  (skip_token "|" <|> skip_ws) *> sep_by1 (skip_token "|") parse_rule
;;

let pexpr_match pexpr =
  let* expr = skip_token "match" *> pexpr <* skip_token "with" in
  let* rules = prules pexpr in
  match rules with
  | h :: tl -> return (Expr_match (expr, h, tl))
  | _ -> fail "Failed to parse match expression"
;;

let pexpr_function pexpr =
  let* rules = skip_token "function" *> prules pexpr in
  match rules with
  | h :: tl -> return (Expr_function (h, tl))
  | _ -> fail "Failed to parse function expression"
;;

let pexpr_opt pexpr =
  let* opt =
    skip_token "Some" *> pexpr
    >>| (fun e -> Some e)
    <|> (skip_token "None" >>| fun _ -> None)
  in
  return (Expr_option opt)
;;

let pexpr_typed pexpr =
  let* expr = pexpr <* skip_token ":" in
  let* core_type = ptype in
  return (Expr_typed (expr, core_type))
;;

let pexpr =
  fix (fun pexpr ->
    let pexpr =
      choice
        [ pexpr_paren (pexpr_typed pexpr)
        ; pexpr_paren pexpr
        ; pexpr_letin pexpr
        ; pexpr_opt pexpr
        ; pexpr_list pexpr
        ; pexpr_ite pexpr
        ; pexpr_match pexpr
        ; pexpr_function pexpr
        ; pexpr_lam pexpr
        ; pexpr_id_or_op
        ; pexpr_const
        ]
    in
    let pexpr = pexpr_paren pexpr <|> pexpr in
    let pexpr = pexpr_tuple pexpr <|> pexpr_app_un pexpr <|> pexpr_app pexpr <|> pexpr in
    let pexpr = pexpr_app_un pexpr <|> pexpr in
    skip_ws *> pexpr <* skip_ws_no_nl)
;;
