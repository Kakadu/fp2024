(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let is_keyword = function
  | "let"
  | "match"
  | "in"
  | "if"
  | "then"
  | "else"
  | "fun"
  | "rec"
  | "true"
  | "false"
  | "Some"
  | "and" -> true
  | _ -> false
;;

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let white_space = take_while Char.is_whitespace
let token s = white_space *> string s
let token1 s = white_space *> s
let parse_parens p = token "(" *> p <* token ")"

let parse_const_int =
  let sign = choice [ token "" ] in
  let num = take_while1 Char.is_digit in
  lift2 (fun s n -> ConstInt (Int.of_string (s ^ n))) sign num
;;

let parse_const_bool =
  choice
    [ token "true" *> return (ConstBool true); token "false" *> return (ConstBool false) ]
;;

let parse_const_string =
  token "\"" *> take_till (Char.equal '\"') <* token "\"" >>| fun s -> ConstString s
;;

let parse_const = choice [ parse_const_int; parse_const_bool; parse_const_string ]
let parse_unar_oper = choice [ token "-" *> return Negative; token "not" *> return Not ]

let parse_ident =
  let parse_first_char =
    satisfy (fun ch -> is_lowercase ch || is_uppercase ch || Char.equal ch '_')
    >>| Char.escaped
  in
  let parse_other_chars =
    take_while (fun ch ->
      is_lowercase ch || is_uppercase ch || is_digit ch || Char.equal ch '_')
  in
  token1 @@ lift2 ( ^ ) parse_first_char parse_other_chars
  >>= fun s -> if is_keyword s then fail "It is not identifier" else return s
;;

let parse_base_type =
  choice
    [ token "int" *> return (TyPrim "int")
    ; token "bool" *> return (TyPrim "bool")
    ; token "string" *> return (TyPrim "string")
    ; token "unit" *> return (TyPrim "unit")
    ]
;;

let rec parse_type_list t =
  let* base = t in
  white_space
  *> token "list"
  *> (parse_type_list (return (TyList base)) <|> return (TyList base))
;;

let parse_type =
  let base_type = parse_base_type in
  let list_type = parse_type_list base_type <|> base_type in
  list_type
;;

let parse_pattern_with_type parse_pattern =
  let* pat = white_space *> token "(" *> parse_pattern in
  let* constr =
    white_space *> token ":" *> white_space *> parse_type <* white_space <* token ")"
  in
  return (PatType (pat, constr))
;;

let parse_pattern_var = parse_ident >>| fun id -> PatVariable id
let parse_pattern_const = parse_const >>| fun c -> PatConst c
let parse_pattern_any = token "_" *> return PatAny

let parse_pattern_tuple parse_pattern =
  parse_parens (sep_by1 (token ",") parse_pattern)
  >>= function
  | [ single ] -> return single
  | first :: second :: rest -> return (PatTuple (first, second, rest))
  | [] -> fail "Empty tuple pattern not allowed"
;;

let parse_pattern_empty = token "()" *> return PatUnit

let parse_pattern =
  fix (fun pat ->
    let pat =
      choice
        [ parse_pattern_var
        ; parse_pattern_any
        ; parse_pattern_const
        ; parse_pattern_tuple pat
        ; parse_pattern_with_type pat
        ; parse_pattern_empty
        ]
    in
    pat)
;;

let parse_left_associative expr oper =
  let rec go acc = lift2 (fun f x -> f acc x) oper expr >>= go <|> return acc in
  expr >>= go
;;

let parse_expr_bin_oper parse_bin_op tkn =
  token tkn *> return (fun e1 e2 -> ExpBinOper (parse_bin_op, e1, e2))
;;

let multiply = parse_expr_bin_oper Multiply "*"
let division = parse_expr_bin_oper Division "/"
let plus = parse_expr_bin_oper Plus "+"
let minus = parse_expr_bin_oper Minus "-"

let compare =
  choice
    [ parse_expr_bin_oper Equal "="
    ; parse_expr_bin_oper NotEqual "<>"
    ; parse_expr_bin_oper LowestEqual "<="
    ; parse_expr_bin_oper LowerThan "<"
    ; parse_expr_bin_oper GretestEqual ">="
    ; parse_expr_bin_oper GreaterThan ">"
    ]
;;

let and_op = parse_expr_bin_oper And "&&"
let or_op = parse_expr_bin_oper Or "||"
let parse_expr_ident = parse_ident >>| fun x -> ExpIdent x
let parse_expr_const = parse_const >>| fun c -> ExpConst c

let parse_expr_branch parse_expr =
  lift3
    (fun cond t f -> ExpBranch (cond, t, f))
    (token "if" *> parse_expr)
    (token "then" *> parse_expr)
    (option None (token "else" *> parse_expr >>| Option.some))
;;

let parse_expr_option expr =
  choice
    [ token "None" *> return (ExpOption None)
    ; (token "Some"
       *> choice
            [ parse_parens expr (* Парсинг выражения в скобках *)
            ; expr (* Парсинг выражения без скобок *)
            ]
       >>| fun e -> ExpOption (Some e))
    ]
;;

let parse_expr_unar_oper parse_expr =
  parse_unar_oper >>= fun op -> parse_expr >>= fun expr -> return (ExpUnarOper (op, expr))
;;

let parse_expr_list expr =
  let parse_elements = sep_by (token ";") expr in
  token "[" *> parse_elements <* token "]" >>| fun elements -> ExpList elements
;;

let parse_expr_lambda parse_expr =
  token "fun" *> sep_by1 white_space parse_pattern
  <* token "->"
  >>= fun params -> parse_expr >>| fun body -> ExpLambda (params, body)
;;

let parse_expr_with_type parse_expr =
  let* expr = white_space *> token "(" *> parse_expr in
  let* constr =
    white_space *> token ":" *> white_space *> parse_type <* white_space <* token ")"
  in
  return (ExpTypeAnnotation (expr, constr))
;;

let parse_lambda_params () = sep_by1 white_space parse_pattern

let parse_expr_let parse_expr =
  let parse_body parse_expr =
    parse_lambda_params ()
    >>= fun params -> token "=" *> parse_expr >>| fun body -> ExpLambda (params, body)
  in
  let parse_rec_flag =
    token "rec"
    *> (peek_char
        >>= function
        | Some c when Char.is_whitespace c -> return true
        | _ -> return false)
    <|> return false
  in
  token "let"
  *> lift4
       (fun is_rec pat e1 e2 -> ExpLet (is_rec, pat, e1, e2))
       parse_rec_flag
       (parse_parens parse_pattern <|> parse_pattern)
       (token "=" *> parse_expr <|> parse_body parse_expr)
       (token "in" *> parse_expr >>| Option.some <|> return None)
;;

let parse_expr_tuple expr =
  parse_parens (sep_by1 (token ",") expr)
  >>= function
  | [ single ] -> return single
  | first :: second :: rest -> return (ExpTuple (first, second, rest))
  | [] -> fail "Empty tuple"
;;

let parse_expr_function parse_expr =
  let parse_application left right = lift2 (fun f x -> ExpFunction (f, x)) left right in
  let rec go acc =
    parse_application (return acc) (parse_expr_tuple parse_expr <|> parse_expr)
    >>= go
    <|> return acc
  in
  parse_expr >>= go
;;

let parse_let_and_binding parse_expr =
  let parse_single_binding =
    let _ =
      lift2
        (fun pat expr -> pat, expr)
        (parse_parens parse_pattern <|> parse_pattern)
        (token "=" *> parse_expr)
    in
    parse_pattern
    >>= fun pat ->
    token "=" *> parse_expr
    >>| (fun expr -> pat, expr)
    <|> (parse_lambda_params ()
         >>= fun params ->
         token "=" *> parse_expr >>| fun body -> pat, ExpLambda (params, body))
  in
  parse_single_binding
  >>= fun first ->
  many1 (token "and" *> parse_single_binding) >>| fun rest -> first :: rest
;;

let parse_expr_let_and parse_expr =
  let parse_rec_flag =
    token "rec"
    *> (peek_char
        >>= function
        | Some c when Char.is_whitespace c -> return true
        | _ -> return false)
    <|> return false
  in
  token "let"
  *> lift3
       (fun is_rec bindings body -> ExpLetAnd (is_rec, bindings, body))
       parse_rec_flag
       (parse_let_and_binding parse_expr)
       (token "in" *> parse_expr >>| Option.some <|> return None)
;;

let parse_expr =
  fix (fun expr ->
    let term =
      choice
        [ parse_expr_ident
        ; parse_expr_const
        ; parse_expr_list expr
        ; parse_parens expr
        ; parse_expr_with_type expr
        ]
    in
    let func = parse_expr_function term in
    let cons = parse_expr_option func <|> func in
    let ife = parse_expr_branch expr <|> cons in
    let unops = parse_expr_unar_oper ife <|> ife in
    let ops1 = parse_left_associative unops (multiply <|> division) in
    let ops2 = parse_left_associative ops1 (plus <|> minus) in
    let cmp = parse_left_associative ops2 compare in
    let boolean = parse_left_associative cmp (and_op <|> or_op) in
    let tuple = parse_expr_tuple boolean <|> boolean in
    let lambda = parse_expr_lambda expr <|> tuple in
    choice [ parse_expr_let expr; parse_expr_lambda expr; lambda ])
;;

let parse_program =
  let definitions_or_exprs =
    many
      (choice [ parse_expr_let_and parse_expr; parse_expr_let parse_expr; parse_expr ]
       <* option () (token ";;" >>| ignore))
  in
  definitions_or_exprs <* white_space
;;

let parse input = parse_string ~consume:All parse_program input
