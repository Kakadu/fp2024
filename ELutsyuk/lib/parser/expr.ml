(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Aux
open Pat

let pexpr_var =
  trim
  @@
  let* var = pid in
  return @@ Var var
;;

let pexpr_const =
  trim
  @@
  let* cons = pconst in
  return @@ Const cons
;;

let pexpr_list parse_expr =
  token "[]" *> (return @@ List [])
  <|> square_par
      @@
      let* el1 = parse_expr in
      let* rest = many (token ";" *> parse_expr) in
      return @@ List (el1 :: rest)
;;

let pexpr_tuple parse_expr =
  round_par
  @@
  let* el1 = parse_expr in
  let* el2 = token "," *> parse_expr in
  let* rest = many (token "," *> parse_expr) in
  return (Tup (el1, el2, rest))
;;

let pexpr_fun parse_pat parse_expr =
  token "fun"
  *>
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
;;

let plet_binding parse_expr =
  let* is_rec = token "rec" *> return Rec <|> return NonRec in
  let* let_pat = ppat in
  let* params = many ppat in
  let* body_expr = token "=" *> parse_expr in
  let expr =
    match params with
    | [] -> body_expr
    | _ -> List.fold_right (fun par acc -> Fun (par, acc)) params body_expr
  in
  return (is_rec, let_pat, expr)
;;

let pexpr_let parse_expr =
  token "let"
  *>
  let* is_rec, pat, expr = plet_binding parse_expr in
  let* in_expr = token "in" *> parse_expr <|> return @@ Const Unit in
  return @@ Let ({ is_rec; pat; expr }, in_expr)
;;

let pexpr_branch pexpr =
  let* if_cond = token "if" *> pexpr in
  let* then_cond = token "then" *> pexpr in
  let* else_cond = token "else" *> pexpr <|> return @@ Const Unit in
  return @@ Branch (if_cond, then_cond, else_cond)
;;

let pexpr_match ppat pexpr =
  let p_case =
    token "|"
    *>
    let* case_pat = ppat in
    let* case_expr = token "->" *> pexpr in
    return { match_pat = case_pat; match_expr = case_expr }
  in
  let* exp = token "match" *> pexpr in
  let* case1 = token "with" *> p_case in
  let* rest_cases = many1 p_case in
  return @@ Match (exp, case1, rest_cases)
;;

let chainl1 expr oper =
  let rec go acc = lift2 (fun f x -> f acc x) oper expr >>= go <|> return acc in
  expr >>= go
;;

let pbin_op op_token constructor =
  token op_token *> (return @@ fun exp1 exp2 -> BinOp (constructor, exp1, exp2))
;;

let prel =
  choice
    [ pbin_op "=" Eq
    ; pbin_op "<>" Ne
    ; pbin_op "<=" Le
    ; pbin_op ">=" Ge
    ; pbin_op "<" Lt
    ; pbin_op ">" Gt
    ]
;;

let plogical = choice [ pbin_op "&&" And; pbin_op "||" Or ]
let pmul = pbin_op "*" Mul
let padd = pbin_op "+" Add
let psub = pbin_op "-" Sub
let pdiv = pbin_op "/" Div

let pexpr_app pexpr =
  let app = return @@ fun exp1 exp2 -> App (exp1, exp2) in
  chainl1 pexpr app
;;

let pexpr =
  fix
  @@ fun expr ->
  let base_expr = choice [ round_par expr; pexpr_const; pexpr_var; pexpr_list expr ] in
  let app_expr = pexpr_app base_expr <|> base_expr in
  let mul_expr = chainl1 app_expr (pmul <|> pdiv) in
  let add_expr = chainl1 mul_expr (padd <|> psub) in
  let comprasion_expr = chainl1 add_expr (prel <|> plogical) in
  let branch_expr = pexpr_branch comprasion_expr <|> comprasion_expr in
  let match_expr = pexpr_match ppat branch_expr <|> branch_expr in
  let tup_expr = pexpr_tuple match_expr <|> match_expr in
  let list_expr = pexpr_list tup_expr <|> tup_expr in
  let fun_expr = pexpr_fun ppat list_expr <|> list_expr in
  let let_expr = pexpr_let fun_expr <|> fun_expr in
  let_expr
;;
