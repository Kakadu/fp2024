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

let pbin_op l_exp binop =
  chainl1 l_exp (binop >>| fun op exp1 exp2 -> BinOp (op, exp1, exp2))
;;

let prel =
  choice
    [ token "=" *> return Eq
    ; token "<>" *> return Ne
    ; token "<=" *> return Le
    ; token ">=" *> return Ge
    ; token "<" *> return Lt
    ; token ">" *> return Gt
    ]
;;

let plogical = choice [ token "&&" *> return And; token "||" *> return Or ]
let pmul = token "*" *> return Mul
let padd = token "+" *> return Add
let psub = token "-" *> return Sub
let pdiv = token "/" *> return Div

let pexpr_app pexpr =
  let app = return @@ fun exp1 exp2 -> App (exp1, exp2) in
  chainl1 pexpr app
;;

let pexpr_unary pexpr =
  let p_minus = token "-" *> return (fun exp -> BinOp (Sub, Const (Int 0), exp)) in
  let p_plus = token "+" *> return (fun exp -> exp) in
  p_minus <|> p_plus <*> pexpr <|> pexpr
;;

let pexpr =
  fix
  @@ fun expr ->
  let atomary = choice [ round_par expr; pexpr_const; pexpr_var; pexpr_list expr ] in
  let apply = pexpr_app atomary <|> atomary in
  let unary = pexpr_unary apply <|> apply in
  let mul = pbin_op unary (pmul <|> pdiv) in
  let add = pbin_op mul (padd <|> psub) in
  let compr = pbin_op add (prel <|> plogical) in
  let branch = pexpr_branch compr <|> compr in
  let match_exp = pexpr_match ppat branch <|> branch in
  let tup = pexpr_tuple match_exp <|> match_exp in
  let list = pexpr_list tup <|> tup in
  let fun_exp = pexpr_fun ppat list <|> list in
  let let_exp = pexpr_let fun_exp <|> fun_exp in
  let_exp
;;
