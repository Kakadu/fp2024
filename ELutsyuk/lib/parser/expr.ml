(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Auxilary
open Const
open Patterns

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

let prs_expr_list prs_expr =
  token "[]" *> (return @@ List [])
  <|> square_par
      @@
      let* el1 = prs_expr in
      let+ rest = many (token ";" *> prs_expr) in
      List (el1 :: rest)
;;

let prs_expr_tuple prs_expr =
  round_par
  @@
  let* el1 = prs_expr in
  let* el2 = token "," *> prs_expr in
  let+ rest = many (token "," *> prs_expr) in
  Tup (el1, el2, rest)
;;

let prs_expr_fun prs_pat prs_expr =
  let* _ = token "fun" in
  let* pat = prs_pat in
  let* params = many prs_pat in
  let* _ = token "->" in
  let+ body_expr = prs_expr in
  let expr =
    match params with
    | [] -> body_expr
    | _ -> List.fold_right (fun par acc -> Fun (par, acc)) params body_expr
  in
  Fun (pat, expr)
;;

let prs_let_binding prs_expr =
  let* pat = prs_pat in
  let* params = many prs_pat in
  let* body_expr = token "=" *> prs_expr in
  let expr =
    match params with
    | [] -> body_expr
    | _ -> List.fold_right (fun par acc -> Fun (par, acc)) params body_expr
  in
  return { pat : pat; expr : expr }
;;

let prs_expr_let prs_expr =
  let* _ = token "let" in
  let* is_rec = token "rec" *> return Rec <|> return NonRec in
  let* binding = prs_let_binding prs_expr in
  let* bindings_list = many (token "and" *> prs_let_binding prs_expr) in
  let+ in_expr = token "in" *> prs_expr <|> return @@ Const Unit in
  Let (is_rec, binding, bindings_list, in_expr)
;;

let prs_expr_branch prs_expr =
  let* if_cond = token "if" *> prs_expr in
  let* then_cond = token "then" *> prs_expr in
  let+ else_cond = token "else" *> prs_expr <|> return @@ Const Unit in
  Branch (if_cond, then_cond, else_cond)
;;

(* let prs_expr_match prs_pat prs_expr =
  let p_case =
    token "|"
    *>
    let* case_pat = prs_pat in
    let* case_expr = token "->" *> prs_expr in
    return { match_pat = case_pat; match_expr = case_expr }
  in
  let* exp = token "match" *> prs_expr in
  let* case1 = token "with" *> p_case in
  let* rest_cases = many1 p_case in
  return @@ Match (exp, case1, rest_cases)
;; *)

let chainl1 expr oper =
  let rec go acc = lift2 (fun f x -> f acc x) oper expr >>= go <|> return acc in
  expr >>= go
;;

let prs_bin_op l_exp binop =
  chainl1 l_exp (binop >>| fun op exp1 exp2 -> BinOp (op, exp1, exp2))
;;

let prs_rel =
  choice
    [ token "=" *> return Eq
    ; token "<>" *> return Ne
    ; token "<=" *> return Le
    ; token ">=" *> return Ge
    ; token "<" *> return Lt
    ; token ">" *> return Gt
    ]
;;

let prs_logical = choice [ token "&&" *> return And; token "||" *> return Or ]
let prs_mul = token "*" *> return Mul
let prs_add = token "+" *> return Add
let prs_sub = token "-" *> return Sub
let prs_div = token "/" *> return Div

let prs_expr_app expr =
  let app = return @@ fun exp1 exp2 -> App (exp1, exp2) in
  chainl1 expr app
;;

let prs_expr_unary prs_expr =
  let not_followed_by_space p =
    let* c = p in
    let* next = peek_char in
    match next with
    | Some (' ' | '\t' | '\n') -> fail "Unexpected space after unary operator"
    | _ -> return c
  in
  let p_minus =
    not_followed_by_space (char '-')
    *> return (fun exp -> BinOp (Sub, Const (Int 0), exp))
  in
  let p_plus = not_followed_by_space (char '+') *> return (fun exp -> exp) in
  choice [ p_minus <*> prs_expr; p_plus <*> prs_expr; prs_expr ]
;;

let prs_expr =
  fix
  @@ fun expr ->
  let atomary = choice [ round_par expr; prs_expr_const; prs_expr_var ] in
  let unary = prs_expr_unary atomary <|> atomary in
  let apply = prs_expr_app unary <|> unary in
  let mul = prs_bin_op apply (prs_mul <|> prs_div) in
  let add = prs_bin_op mul (prs_add <|> prs_sub) in
  let compr = prs_bin_op add (prs_rel <|> prs_logical) in
  let branch = prs_expr_branch compr <|> compr in
  (* let match_exp = prs_expr_match prs_pat branch <|> branch in *)
  let tup = prs_expr_tuple branch <|> branch in
  let list = prs_expr_list tup <|> tup in
  let fun_exp = prs_expr_fun prs_pat list <|> list in
  let let_exp = prs_expr_let fun_exp <|> fun_exp in
  let_exp
;;
