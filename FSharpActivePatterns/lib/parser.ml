(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base
open KeywordChecker

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
let expr_const_factory parser = parser >>| fun lit -> Const lit
let pat_const_factory parser = parser >>| fun lit -> PConst lit
let p_int = skip_ws *> take_while1 Char.is_digit >>| fun s -> Int_lt (Int.of_string s)
let p_int_expr = expr_const_factory p_int
let p_int_pat = pat_const_factory p_int

let p_bool =
  skip_ws *> string "true" <|> string "false" >>| fun s -> Bool_lt (Bool.of_string s)
;;

let p_bool_expr = expr_const_factory p_bool
let p_bool_pat = pat_const_factory p_bool

let p_string =
  char '"' *> take_while (fun c -> not (Char.equal c '"'))
  <* char '"'
  >>| fun x -> String_lt x
;;

let p_string_expr = expr_const_factory p_string
let p_string_pat = pat_const_factory p_string

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

let p_var_expr = p_ident >>| fun ident -> Variable ident
let p_var_pat = p_ident >>| fun ident -> PVar ident

let p_empty_list empty_list =
  skip_ws *> string "[" *> skip_ws *> string "]" *> skip_ws *> return empty_list
;;

let p_empty_list_expr = p_empty_list Empty_list
let p_empty_list_pat = p_empty_list PEmptyList
let make_cons_expr e1 e2 = Cons_list (e1, e2)
let make_cons_pat p1 p2 = PCons (p1, p2)

let p_cons_list p p_empty_list make_list =
  skip_ws
  *> chainr1 (p <|> p_empty_list) (skip_ws *> string "::" *> skip_ws *> return make_list)
;;

let p_cons_list_expr p_expr = p_cons_list p_expr p_empty_list_expr make_cons_expr
let p_cons_list_pat p_pat = p_cons_list p_pat p_empty_list_pat make_cons_pat

let p_semicolon_list p empty_list cons_list =
  skip_ws
  *> string "["
  *> skip_ws
  *> fix (fun p_semi_list ->
    choice
      [ (p
         <* skip_ws
         <* string ";"
         <* skip_ws
         >>= fun hd -> p_semi_list >>= fun tl -> return (cons_list hd tl))
      ; (p <* skip_ws <* string "]" <* skip_ws >>| fun hd -> cons_list hd empty_list)
      ; string "]" *> skip_ws *> return empty_list
      ])
;;

let p_semicolon_list_expr p_expr = p_semicolon_list p_expr Empty_list make_cons_expr
let p_semicolon_list_pat p_pat = p_semicolon_list p_pat PEmptyList make_cons_pat
let p_unit = skip_ws *> string "(" *> skip_ws *> string ")" *> skip_ws *> return Unit_lt
let p_unit_expr = expr_const_factory p_unit
let p_unit_pat = pat_const_factory p_unit

(* EXPR PARSERS *)
let p_parens p = skip_ws *> char '(' *> skip_ws *> p <* skip_ws <* char ')'
let make_binexpr op expr1 expr2 = Bin_expr (op, expr1, expr2) [@@inline always]
let make_unexpr op expr = Unary_expr (op, expr) [@@inline always]
let make_tuple_expr e1 e2 rest = Tuple (e1, e2, rest) [@@inline always]
let make_tuple_pat p1 p2 rest = PTuple (p1, p2, rest)

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

let p_tuple make p =
  skip_ws
  *> string "("
  *> lift3
       (fun fst snd tail -> make fst snd tail)
       p
       (skip_ws *> string "," *> skip_ws *> p)
       (many (skip_ws *> string "," *> skip_ws *> p))
  <* skip_ws
  <* string ")"
;;

let p_tuple_expr p_expr = p_tuple make_tuple_expr p_expr
let p_tuple_pat p_pat = p_tuple make_tuple_pat p_pat

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

let p_let_bind p_expr =
  skip_ws
  *> string "and"
  *> skip_ws_sep1
  *> lift3
       (fun name args body -> Let_bind (name, args, body))
       p_ident
       (many (skip_ws *> p_ident))
       (skip_ws *> string "=" *> skip_ws *> p_expr)
;;

(*
   let p_letin2 p_expr =
   skip_ws *> string "let" *> skip_ws_sep1 *> (string "rec" *> return Rec <|> return Nonrec)
   >>= fun rec_flag ->
   p_ident
   >>= return
   >>= fun name ->
   skip_ws *> many (skip_ws *> p_ident)
   >>= fun args ->
   skip_ws *> string "=" *> skip_ws *> p_expr
   >>= fun body ->
   skip_ws *> many (skip_ws *> p_let_bind p_expr)
   >>= fun let_bind_list ->
   skip_ws *> string "in" *> skip_ws_sep1 *> p_expr
   >>= fun in_expr ->
   return (LetIn (rec_flag, Let_bind (name, args, body), let_bind_list, in_expr))
   ;; *)

let p_letin p_expr =
  skip_ws
  *> string "let"
  *> skip_ws_sep1
  *>
  let* rec_flag = string "rec" *> skip_ws_sep1 *> return Rec <|> return Nonrec in
  let* name = p_ident in
  let* args = skip_ws *> many (skip_ws *> p_ident) in
  let* body = skip_ws *> string "=" *> skip_ws *> p_expr in
  let* let_bind_list = skip_ws *> many (skip_ws *> p_let_bind p_expr) in
  let* in_expr = skip_ws *> string "in" *> skip_ws_sep1 *> p_expr in
  return (LetIn (rec_flag, Let_bind (name, args, body), let_bind_list, in_expr))
;;

(*
   let p_let p_expr =
   skip_ws *> string "let" *> skip_ws_sep1 *> (string "rec" *> return Rec <|> return Nonrec)
   >>= fun rec_flag ->
   p_ident
   >>= fun name ->
   skip_ws *> many (skip_ws *> p_ident)
   >>= fun args ->
   skip_ws *> string "=" *> skip_ws *> p_expr
   >>= fun body ->
   skip_ws *> many (skip_ws *> p_let_bind p_expr)
   >>= fun let_bind_list ->
   return (Let (rec_flag, Let_bind (name, args, body), let_bind_list))
   ;; *)

let p_let p_expr =
  skip_ws
  *> string "let"
  *> skip_ws_sep1
  *>
  let* rec_flag = string "rec" *> skip_ws_sep1 *> return Rec <|> return Nonrec in
  let* name = p_ident in
  let* args = skip_ws *> many (skip_ws *> p_ident) in
  let* body = skip_ws *> string "=" *> skip_ws *> p_expr in
  let* let_bind_list = skip_ws *> many (skip_ws *> p_let_bind p_expr) in
  return (Let (rec_flag, Let_bind (name, args, body), let_bind_list))
;;

let app_first expr =
  skip_ws
  *> fix (fun a_exp ->
    let expr = choice [ p_var_expr; p_if expr; p_letin expr; p_parens a_exp ] in
    expr)
;;

let p_apply expr =
  let* name = app_first expr in
  let rec parse_args acc =
    skip_ws
    *> choice
         [ p_var_expr
         ; p_bool_expr
         ; p_unit_expr
         ; p_int_expr
         ; p_string_expr
         ; p_if expr
         ; p_letin expr
         ; p_parens expr
         ]
    >>= (fun arg -> parse_args (Function_call (acc, arg)))
    <|> return acc
  in
  parse_args name
;;

let p_option p_expr =
  skip_ws *> (string "None" *> skip_ws_sep1 *> return (Option None))
  <|> (skip_ws *> string "Some" *> skip_ws_sep1 *> p_expr
       >>| fun expr -> Option (Some expr))
;;

let make_cons_pat pat1 pat2 = PCons (pat1, pat2)
(*
   let p_cons_pat p_pat =
   skip_ws
   *> chainr1
   (p_pat <|> p_empty_list)
   (skip_ws *> string "::" *> skip_ws *> return make_cons_pat) *)

let p_pat_const = choice [ p_int_pat; p_bool_pat; p_unit_pat; p_string_pat ]

let p_pat =
  fix (fun self ->
    skip_ws
    *> choice
         [ p_tuple_pat self
         ; p_empty_list_pat
         ; p_semicolon_list_pat self
         ; p_cons_list_pat p_var_pat
         ; p_var_pat
         ; p_pat_const
         ; string "_" *> return Wild
         ])
;;

let p_lambda p_expr =
  skip_ws
  *> string "fun"
  *> skip_ws_sep1
  *>
  let* pat = skip_ws *> p_pat <* skip_ws in
  let* pat_list = many (skip_ws *> p_pat) <* skip_ws in
  let* _ = skip_ws *> string "->" in
  let* body = p_expr in
  return (Lambda (pat, pat_list, body))
;;

let p_match p_expr =
  lift4
    (fun value pat1 expr1 list -> Match (value, pat1, expr1, list))
    (skip_ws *> string "match" *> p_expr <* skip_ws <* string "with")
    (skip_ws *> string "|" *> p_pat <* skip_ws <* string "->")
    (skip_ws *> p_expr)
    (many
       (skip_ws *> string "|" *> p_pat
        <* skip_ws
        <* string "->"
        >>= fun pat -> p_expr >>= fun expr -> return (pat, expr)))
;;

let p_expr =
  skip_ws
  *> fix (fun p_expr ->
    let atom =
      choice
        [ p_var_expr
        ; p_int_expr
        ; p_string_expr
        ; p_unit_expr
        ; p_bool_expr
        ; p_parens p_expr
        ; p_semicolon_list_expr p_expr
        ]
    in
    let tuple = p_tuple make_tuple_expr (p_expr <|> atom) <|> atom in
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
    let cons_list = p_cons_list_expr comp_or <|> comp_or in
    let ematch = p_match cons_list <|> cons_list in
    let efun = p_lambda ematch <|> ematch in
    let option = p_option efun <|> efun in
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
