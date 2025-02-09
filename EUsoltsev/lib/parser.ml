(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

let is_keyword = function
  | "let"
  | "in"
  | "fun"
  | "rec"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "Some"
  | "None"
  | "and"
  | "match"
  | "with"
  | "function"
  | "type" -> true
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
let pws1 = take_while1 Char.is_whitespace
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

let parse_const_unit = token "()" *> return ConstUnit

let parse_const =
  choice [ parse_const_int; parse_const_bool; parse_const_string; parse_const_unit ]
;;

let parse_unar_oper =
  choice
    [ token "-" *> return Negative
    ; token "not" *> return Not
    ; token "+" *> return Positive
    ]
;;

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

let parse_tuple_type ptype_opt =
  let star = token "*" in
  lift3
    (fun t1 t2 rest -> TyTuple (t1 :: t2 :: rest))
    ptype_opt
    (star *> ptype_opt)
    (many (star *> ptype_opt))
;;

let rec parse_fun_type ptype_opt =
  ptype_opt
  >>= fun left ->
  token "->" *> parse_fun_type ptype_opt
  >>= (fun right -> return (TyArrow (left, right)))
  <|> return left
;;

let parse_option_type ptype_opt =
  ptype_opt >>= fun t -> token "option" *> return (TyOption t)
;;

let parse_type_helper =
  fix (fun typ ->
    let atom = parse_base_type <|> parse_parens typ in
    let list = parse_type_list atom <|> atom in
    let option = parse_option_type list <|> list in
    let tuple = parse_tuple_type option <|> option in
    let func = parse_fun_type tuple <|> tuple in
    func)
;;

let parse_type =
  let t = parse_type_helper in
  token ":" *> t
;;

let parse_type_option =
  let some_type = parse_type_helper in
  token ":" *> (some_type >>| fun t -> Some t) <|> return None
;;

let parse_pattern_var = parse_ident >>| fun id -> PatVariable id
let parse_pattern_const = parse_const >>| fun c -> PatConst c
let parse_pattern_any = token "_" *> return PatAny

let parse_pattern_tuple pat =
  let commas = token "," in
  let tuple =
    lift3
      (fun p1 p2 rest -> PatTuple (p1, p2, rest))
      pat
      (commas *> pat)
      (many (commas *> pat))
    <* white_space
  in
  parse_parens tuple <|> tuple
;;

let parse_pattern_list pat =
  let semicols = token ";" in
  token "[" *> (sep_by semicols pat >>| fun patterns -> PatList patterns) <* token "]"
;;

let rec parse_pattern_cons pat =
  let cons =
    pat
    >>= fun head ->
    token "::" *> parse_pattern_cons pat
    >>= (fun tail -> return (PatCons (head, tail)))
    <|> return head
  in
  parse_parens cons <|> cons
;;

let parse_pattern_option pat =
  lift
    (fun e -> PatOption e)
    (token "Some" *> pat >>| (fun e -> Some e) <|> (token "None" >>| fun _ -> None))
;;

let parse_pattern =
  fix (fun pat ->
    let atom =
      parse_pattern_const <|> parse_pattern_var <|> parse_pattern_any <|> parse_parens pat
    in
    let opt = parse_pattern_option atom <|> atom in
    let tuple = parse_pattern_tuple opt <|> opt in
    let lst = parse_pattern_list tuple <|> tuple in
    let cons = parse_pattern_cons lst <|> lst in
    cons)
;;

let parse_left_associative parse_expr oper =
  let rec go acc = lift2 (fun f x -> f acc x) oper parse_expr >>= go <|> return acc in
  parse_expr >>= go
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
let cons = parse_expr_bin_oper Cons "::"
let parse_expr_ident = parse_ident >>| fun x -> ExpIdent x
let parse_expr_const = parse_const >>| fun x -> ExpConst x

let parse_expr_constrant parse_expr =
  lift2 (fun expr t -> ExpConstrant (expr, t)) parse_expr parse_type
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

let parse_expr_apply e =
  parse_left_associative e (return (fun e1 e2 -> ExpApply (e1, e2)))
;;

let parse_inf_oper expr =
  let parse_inf_op =
    token1
      (take_while1 (function
        | '|'
        | '~'
        | '?'
        | '<'
        | '>'
        | '!'
        | '&'
        | '*'
        | '/'
        | '='
        | '+'
        | '-'
        | '@'
        | '^' -> true
        | _ -> false))
  in
  parse_parens parse_inf_op
  >>= fun inf_op ->
  lift2
    (fun left right -> ExpApply (ExpApply (ExpIdent inf_op, left), right))
    (pws1 *> expr)
    (white_space *> expr)
;;

let parse_first_ty_pattern =
  let ty_pat = lift2 (fun pat ty -> pat, ty) parse_pattern parse_type_option in
  ty_pat <|> parse_parens ty_pat <|> (parse_pattern >>| fun p -> p, None)
;;

let parse_ty_pattern =
  let ty_pat = lift2 (fun pat ty -> pat, ty) parse_pattern parse_type_option in
  parse_parens ty_pat <|> (parse_pattern >>| fun p -> p, None)
;;

let parse_expr_lambda parse_expr =
  token "fun" *> pws1 *> many1 parse_ty_pattern
  >>= fun args -> token "->" *> parse_expr >>| fun body -> ExpLambda (args, body)
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

let parse_expr_match parse_expr =
  let parse_case =
    parse_pattern
    <* token "->"
    >>= fun pattern -> white_space *> parse_expr >>| fun expr -> ExpCase (pattern, expr)
  in
  let parse_match_expression =
    token "match" *> parse_expr
    <* token "with"
    >>= fun expr ->
    (token "|" <|> white_space) *> parse_case
    >>= fun first_case ->
    many (token "|" *> parse_case)
    >>| fun other_cases -> ExpMatch (expr, first_case, other_cases)
  in
  let parse_function_expression =
    token "function" *> (token "|" <|> white_space) *> parse_case
    >>= fun first_case ->
    many (token "|" *> parse_case)
    >>| fun other_cases -> ExpFunction (first_case, other_cases)
  in
  parse_match_expression <|> parse_function_expression
;;

let rec chainr e op =
  let* left = e in
  (let* f = op in
   let* right = chainr e op in
   return (f left right))
  <|> return left
;;

let parse_body parse_expr =
  many1 parse_ty_pattern
  >>= fun patterns -> token "=" *> parse_expr >>| fun body -> ExpLambda (patterns, body)
;;

let parse_value_binding parse_expr =
  lift2
    (fun ty_pattern expr -> ExpValueBind (ty_pattern, expr))
    parse_first_ty_pattern
    (token "=" *> parse_expr <|> parse_body parse_expr)
;;

let parse_expr_let parse_expr =
  token "let"
  *> lift4
       (fun rec_flag value_bindings and_bindings body ->
         ExpLet (rec_flag, value_bindings, and_bindings, body))
       (token "rec" *> (pws1 *> return true) <|> return false)
       (parse_value_binding parse_expr)
       (many (token "and" *> parse_value_binding parse_expr))
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
        ; parse_expr_lambda expr
        ; parse_expr_option expr
        ; parse_expr_match expr
        ; parse_parens (parse_expr_constrant expr)
        ]
    in
    let let_expr = parse_expr_let expr in
    let ife = parse_expr_branch (expr <|> term) <|> term in
    let inf_op = parse_inf_oper (ife <|> term) <|> ife in
    let func = parse_expr_apply (inf_op <|> term) <|> inf_op in
    let unops = parse_expr_unar_oper func <|> func in
    let ops1 = parse_left_associative unops (multiply <|> division) in
    let ops2 = parse_left_associative ops1 (plus <|> minus) in
    let cmp = parse_left_associative ops2 compare in
    let boolean = parse_left_associative cmp (and_op <|> or_op) in
    let tuple = parse_expr_tuple boolean <|> boolean in
    let cons = chainr tuple cons in
    choice [ let_expr; cons ])
;;

let parse_structure =
  let parse_eval = parse_expr >>| fun e -> SEval e in
  let parse_value =
    token "let"
    *> lift3
         (fun r id id_list -> SValue (r, id, id_list))
         (token "rec" *> (pws1 *> return true) <|> return false)
         (parse_value_binding parse_expr)
         (many (token "and" *> parse_value_binding parse_expr))
  in
  choice [ parse_eval; parse_value ]
;;

let parse_program =
  let definitions_or_exprs = many parse_structure <* option () (token ";;" >>| ignore) in
  definitions_or_exprs <* white_space
;;

let parse str = parse_string ~consume:All parse_program str
