(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast
open PrsAuxilary
open Constants
open Patterns
open Types

let prs_expr_var =
  trim
  @@
  let+ parsed = prs_id in
  Var parsed
;;

let prs_expr_const =
  trim
  @@
  let+ parsed = prs_const in
  Const parsed
;;

let prs_expr_list expr =
  square_par
  @@
  let+ parsed = sep_by (token ";") expr in
  List parsed
;;

let prs_expr_tuple expr =
  let* el1 = expr in
  let* el2 = token "," *> expr in
  let+ rest = many (token "," *> expr) in
  Tup (el1, el2, rest)
;;

let prs_expr_fun expr =
  let* pat = token "fun" *> prs_pat in
  let* params = many prs_pat in
  let+ body_expr = token "->" *> expr in
  Fun (pat, params, body_expr)
;;

let rec prs_expr_body expr =
  let* pat = prs_pat in
  let* pats = many prs_pat in
  choice
    [ prs_expr_body expr
    ; (let+ exp = token "=" *> expr in
       Fun (pat, pats, exp))
    ]
;;

let prs_let_binding expr =
  let* pat = prs_pat in
  let+ body_expr = choice [ token "=" *> expr; prs_expr_body expr ] in
  Binding (pat, body_expr)
;;

let prs_expr_let expr =
  trim
  @@ (token "let"
      *>
      let* is_rec = token "rec" *> return Rec <|> return NonRec in
      let* binding = prs_let_binding expr in
      let* bindings_list = many (token "and" *> prs_let_binding expr) in
      let+ in_expr = token "in" *> expr in
      Let (is_rec, binding, bindings_list, in_expr))
;;

let prs_expr_branch prs_expr =
  let* if_cond = token "if" *> prs_expr in
  let* then_cond = token "then" *> prs_expr in
  let+ else_cond = token "else" *> prs_expr <|> return @@ Const Unit in
  Branch (if_cond, then_cond, else_cond)
;;

let chainl1 expr oper =
  let rec go acc = lift2 (fun f x -> f acc x) oper expr >>= go <|> return acc in
  expr >>= go
;;

let prs_bin_op binop sign =
  trim @@ (token sign *> return (fun exp1 exp2 -> BinOp (binop, exp1, exp2)))
;;

let prs_logical = choice [ prs_bin_op And "&&"; prs_bin_op Or "||" ]
let prs_mul = prs_bin_op Mul "*"
let prs_add = prs_bin_op Add "+"
let prs_sub = prs_bin_op Sub "-"
let prs_div = prs_bin_op Div "/"

let prs_rel =
  choice
    [ prs_bin_op Eq "="
    ; prs_bin_op Ne "<>"
    ; prs_bin_op Le "<="
    ; prs_bin_op Ge ">="
    ; prs_bin_op Lt "<"
    ; prs_bin_op Gt ">"
    ]
;;

let prs_unary op sign = trim @@ (token sign *> return (fun exp -> Unary (op, exp)))
let prs_minus = prs_unary Minus "-"
let prs_plus = prs_unary Plus "+"
let prs_not = prs_unary Not "not" <* skip_ws

let prs_option expr =
  let p_some_expr =
    token "Some"
    *>
    let* p_expr = round_par expr <|> expr in
    return (Some p_expr)
  in
  let p_none = token "None" *> return None in
  let+ parsed = p_some_expr <|> p_none in
  Option parsed
;;

let prs_expr_app expr =
  let app = return @@ fun exp1 exp2 -> App (exp1, exp2) in
  chainl1 expr app
;;

let prs_expr_type expr =
  let expr_with_type =
    let* exp = expr in
    let* _ = token ":" in
    let+ typ = prs_typ in
    Type (exp, typ)
  in
  expr_with_type <|> round_par expr_with_type
;;

let unary_chain exp op =
  fix (fun self -> op >>= (fun unop -> self >>= fun exp -> return (unop exp)) <|> exp)
;;

let prs_expr =
  fix (fun expr ->
    let atom_expr =
      choice
        [ prs_expr_const
        ; prs_expr_var
        ; round_par expr
        ; prs_expr_list expr
        ; prs_expr_fun expr
        ; prs_option expr
        ; round_par (prs_expr_type expr)
        ]
    in
    let let_expr = prs_expr_let expr in
    let ite_expr = prs_expr_branch (expr <|> atom_expr) <|> atom_expr in
    let app_expr = prs_expr_app (ite_expr <|> atom_expr) <|> ite_expr in
    let un_expr =
      choice
        [ unary_chain app_expr prs_not
        ; unary_chain app_expr prs_minus
        ; unary_chain app_expr prs_plus
        ]
    in
    let factor_expr = chainl1 un_expr (prs_mul <|> prs_div) in
    let sum_expr = chainl1 factor_expr (prs_add <|> prs_sub) in
    let rel_expr = chainl1 sum_expr prs_rel in
    let log_expr = chainl1 rel_expr prs_logical in
    let tup_expr = prs_expr_tuple log_expr <|> log_expr in
    choice [ let_expr; tup_expr ])
;;
