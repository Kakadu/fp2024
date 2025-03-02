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
  let parse_unparenthesized =
    lift3
      (fun p1 p2 rest -> PatTuple (p1, p2, rest))
      parse_pattern
      (token "," *> parse_pattern)
      (many (token "," *> parse_pattern))
    <* white_space
  in
  parse_parens parse_unparenthesized <|> parse_unparenthesized
;;

let parse_pattern_list parse_pattern =
  let semicols = token ";" in
  token "[" *> (sep_by semicols parse_pattern >>| fun patterns -> PatList patterns)
  <* token "]"
;;

let parse_pattern_empty = token "()" *> return PatUnit

let parse_pattern_option parse_pattern =
  lift
    (fun e -> PatOption e)
    (token "Some" *> parse_pattern
     >>| (fun e -> Some e)
     <|> (token "None" >>| fun _ -> None))
;;

let parse_pattern =
  fix (fun pat ->
    let atom =
      choice
        [ parse_pattern_var
        ; parse_pattern_any
        ; parse_pattern_const
        ; parse_pattern_empty
        ; parse_pattern_with_type pat
        ; parse_parens pat
        ; parse_pattern_option pat
        ]
    in
    let tuple = parse_pattern_tuple atom <|> atom in
    let lst = parse_pattern_list tuple <|> tuple in
    lst)
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

let parse_expr_with_type parse_expr =
  let parse_annotated_type = token ":" *> parse_type in
  lift2 (fun expr t -> ExpTypeAnnotation (expr, t)) parse_expr parse_annotated_type
;;

let parse_expr_branch parse_expr =
  lift3
    (fun cond t f -> ExpBranch (cond, t, f))
    (token "if" *> parse_expr)
    (token "then" *> parse_expr)
    (option None (token "else" *> parse_expr >>| Option.some))
;;

let parse_expr_option parse_expr =
  choice
    [ token "None" *> return (ExpOption None)
    ; (token "Some" *> choice [ parse_parens parse_expr; parse_expr ]
       >>| fun e -> ExpOption (Some e))
    ]
;;

let parse_expr_unar_oper parse_expr =
  parse_unar_oper >>= fun op -> parse_expr >>= fun expr -> return (ExpUnarOper (op, expr))
;;

let parse_expr_list parse_expr =
  let parse_elements = sep_by (token ";") parse_expr in
  token "[" *> parse_elements <* token "]" >>| fun elements -> ExpList elements
;;

let parse_expr_function e =
  parse_left_associative e (return (fun e1 e2 -> ExpFunction (e1, e2)))
;;

let parse_expr_lambda parse_expr =
  token "fun" *> sep_by1 white_space parse_pattern
  <* token "->"
  >>= fun params -> parse_expr >>| fun body -> ExpLambda (params, body)
;;

let parse_expr_tuple parse_expr =
  let commas = token "," in
  let tuple =
    lift3
      (fun e1 e2 rest -> ExpTuple (e1, e2, rest))
      (parse_expr <* commas)
      parse_expr
      (many (commas *> parse_expr))
    <* white_space
  in
  parse_parens tuple <|> tuple
;;

let parse_body parse_expr =
  many1 parse_pattern
  >>= fun patterns -> token "=" *> parse_expr >>| fun body -> ExpLambda (patterns, body)
;;

let parse_expr_let parse_expr =
  token "let"
  *> lift4
       (fun rec_flag value_bindings and_bindings body ->
         ExpLet (rec_flag, value_bindings, and_bindings, body))
       (token "rec" *> (take_while1 Char.is_whitespace *> return true) <|> return false)
       (lift2
          (fun pat expr -> pat, expr)
          parse_pattern
          (token "=" *> parse_expr <|> parse_body parse_expr))
       (many
          (token "and"
           *> lift2
                (fun pat expr -> pat, expr)
                parse_pattern
                (token "=" *> parse_expr <|> parse_body parse_expr)))
       (token "in" *> parse_expr)
;;

let parse_expr =
  fix (fun expr ->
    let term =
      choice
        [ parse_expr_ident
        ; parse_expr_const
        ; parse_expr_list expr
        ; parse_parens expr
        ; parse_parens (parse_expr_with_type expr)
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

let parse_structure =
  let parse_eval = parse_expr >>| fun e -> SEval e in
  let parse_value =
    token "let"
    *> lift3
         (fun r id id_list -> SValue (r, id, id_list))
         (token "rec" *> (take_while1 Char.is_whitespace *> return true) <|> return false)
         (lift2
            (fun pat expr -> pat, expr)
            parse_pattern
            (token "=" *> parse_expr <|> parse_body parse_expr))
         (many
            (token "and"
             *> lift2
                  (fun pat expr -> pat, expr)
                  parse_pattern
                  (token "=" *> parse_expr <|> parse_body parse_expr)))
  in
  choice [ parse_eval; parse_value ]
;;

let parse_program =
  let definitions_or_exprs = many parse_structure <* option () (token ";;" >>| ignore) in
  definitions_or_exprs <* white_space
;;

let parse input = parse_string ~consume:All parse_program input
