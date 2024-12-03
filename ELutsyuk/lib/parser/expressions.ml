(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Auxiliaries
open Literals
open Patterns

(*  *)
let parse_expr_var =
  let* var = parse_id in
  return @@ Var var
;;

(*  *)
let parse_expr_lit =
  let* literal = parse_lit in
  return @@ Lit literal
;;

(*  *)
let parse_bin_op op_token op_constructor =
  trim
  @@ (token op_token *> (return @@ fun exp1 exp2 -> BinaryOp (op_constructor, exp1, exp2)))
;;

let parse_rel =
  choice
    [ parse_bin_op "=" Eq
    ; parse_bin_op "<>" NonEq
    ; parse_bin_op "<" Lt
    ; parse_bin_op "<=" LtEq
    ; parse_bin_op "=" Gt
    ; parse_bin_op "=" GtEq
    ]
;;

let parse_logical = choice [ parse_bin_op "&&" And; parse_bin_op "||" Or ]
let parse_mul = parse_bin_op "*" Mult
let parse_add = parse_bin_op "+" Add
let parse_sub = parse_bin_op "-" Sub
let parse_div = parse_bin_op "/" Div

(*  *)
let parse_expr_list parse_expr =
  (square_brackets
   @@
   let* el1 = parse_expr in
   let* rest = many (token ";" *> parse_expr) in
   return @@ List (el1 :: rest))
  <|> token "[]" *> (return @@ List [])
;;

(*  *)
let parse_expr_tuple parse_expr =
  round_parens
  @@
  let* el1 = parse_expr in
  let* el2 = token "," *> parse_expr in
  let* rest = many (token "," *> parse_expr) in
  return (Tuple (el1, el2, rest))
;;

(*  *)
let parse_expr_fun parse_pat parse_expr =
  let parse_body =
    let* pat = parse_pat in
    let* params = many parse_pat in
    token "->"
    *>
    let* body_expr = parse_expr in
    let expr =
      match params with
      | [] -> body_expr
      | _ -> List.fold_right (fun par acc -> Fun (par, acc)) params body_expr
    in
    return @@ Fun (pat, expr)
  in
  token "fun" *> parse_body
;;

(*  *)
let parse_expr_let parse_pat parse_expr =
  let parse_let_binding =
    let* is_rec = token "rec" *> return Rec <|> return NonRec in
    let* let_pat = parse_pat in
    let* params = many parse_pat in
    let* body_expr = token "=" *> parse_expr in
    let expr =
      match params with
      | [] -> body_expr
      | _ -> List.fold_right (fun par acc -> Fun (par, acc)) params body_expr
    in
    return (is_rec, let_pat, expr)
  in
  token "let"
  *>
  let* is_rec, pat, expr = parse_let_binding in
  let* in_expr = token "in" *> parse_expr <|> return @@ Lit Unit in
  return @@ Let ({ is_rec; pat; expr }, in_expr)
;;

(*  *)
let parse_expr_app parse_expr =
  let app = return @@ fun exp1 exp2 -> App (exp1, exp2) in
  chainl1 parse_expr app
;;

(*  *)
let parse_expr_branch parse_expr =
  let* if_cond = token "if" *> parse_expr in
  let* then_cond = token "then" *> parse_expr in
  let* else_cond = token "else" *> parse_expr <|> return @@ Lit Unit in
  return @@ Branch (if_cond, then_cond, else_cond)
;;

(* *)
let parse_expr_match parse_expr parse_pat =
  let parse_case =
    token "|"
    *>
    let* case_pat = parse_pat in
    let* case_expr = token "->" *> parse_expr in
    return { match_pat = case_pat; match_expr = case_expr }
  in
  let* exp = token "match" *> parse_expr in
  let* case1 = token "with" *> parse_case in
  let* rest_cases = many1 parse_case in
  return @@ Match (exp, case1, rest_cases)
;;

let parse_expr =
  fix
  @@ fun parse_expr ->
  let base_expr =
    choice
      [ round_parens parse_expr
      ; parse_expr_lit
      ; parse_expr_var
      ; parse_expr_list parse_expr
      ; parse_expr_fun parse_pat parse_expr
      ]
  in
  let expr_app = parse_expr_app base_expr <|> base_expr in
  let expr_mul_div = chainl1 expr_app (parse_mul <|> parse_div) in
  let expr_add_sub = chainl1 expr_mul_div (parse_add <|> parse_sub) in
  let expr_compare = chainl1 expr_add_sub parse_rel in
  let expr_logical = chainr1 expr_compare parse_logical in
  let expr_tuples = parse_expr_tuple expr_logical <|> expr_logical in
  let expr_branches = parse_expr_branch expr_tuples <|> expr_tuples in
  let expr_let = parse_expr_let parse_pat expr_branches <|> expr_branches in
  expr_let
;;
