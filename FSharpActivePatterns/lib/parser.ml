(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base
open KeywordChecker
open TypedTree

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
    (match c with
     | '(' | ')' | ']' | ';' | ':' | ',' -> return (Some c)
     | _ -> if is_ws c then return (Some c) else fail "need a delimiter")
;;

let skip_ws_sep1 = peek_sep1 *> skip_ws

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec chainr1 e op =
  let* left = e in
  (let* f = op in
   let* right = chainr1 e op in
   return (f left right))
  <|> return left
;;

let rec unary_chain op e =
  op >>= (fun unexpr -> unary_chain op e >>= fun expr -> return (unexpr expr)) <|> e
;;

(* SIMPLE PARSERS *)
let expr_const_factory parser = parser >>| fun lit -> Const lit
let pat_const_factory parser = parser >>| fun lit -> PConst lit

let p_int =
  skip_ws
  *> let* sign = string "+" <|> string "-" <|> string "" in
     let* number = take_while1 Char.is_digit in
     return (Int_lt (Int.of_string (sign ^ number)))
;;

let p_int_expr = expr_const_factory p_int
let p_int_pat = pat_const_factory p_int

let p_bool =
  skip_ws *> string "true"
  <|> skip_ws *> string "false"
  >>| fun s -> Bool_lt (Bool.of_string s)
;;

let p_bool_expr = expr_const_factory p_bool
let p_bool_pat = pat_const_factory p_bool

let p_escaped_char =
  char '\\'
  *> (any_char
      >>= function
      | '"' -> return '"'
      | '\\' -> return '\\'
      | 'n' -> return '\n'
      | 't' -> return '\t'
      | 'r' -> return '\r'
      | other -> fail (Printf.sprintf "Unknown escape sequence: \\%c" other))
;;

let p_regular_char = satisfy (fun c -> Char.(c <> '"' && c <> '\\'))

let p_string =
  let+ s = skip_ws *> char '"' *> many (p_regular_char <|> p_escaped_char) <* char '"' in
  String_lt (String.of_char_list s)
;;

let p_string_expr = expr_const_factory p_string
let p_string_pat = pat_const_factory p_string

let p_inf_oper =
  let* oper =
    skip_ws
    *> take_while1 (function
      | '+'
      | '-'
      | '<'
      | '>'
      | '*'
      | '|'
      | '!'
      | '$'
      | '%'
      | '&'
      | '.'
      | '/'
      | ':'
      | '='
      | '?'
      | '@'
      | '^'
      | '~' -> true
      | _ -> false)
  in
  if is_keyword oper
  then fail "keywords are not allowed as variable names"
  else return (Ident oper)
;;

let p_varname =
  let* name =
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
  if is_keyword name
  then fail "keywords are not allowed as variable names"
  else return name
;;

let p_ident =
  let* varname = p_varname in
  return (Ident varname)
;;

let p_type = skip_ws *> char ':' *> skip_ws *> p_varname >>| fun s -> Primitive s
let p_var_expr = p_ident >>| fun ident -> Variable ident
let p_var_pat = p_ident >>| fun ident -> PVar ident

let p_semicolon_list p_elem =
  skip_ws
  *> string "["
  *> skip_ws
  *> let+ list =
       fix (fun p_semi_list ->
         choice
           [ (let* hd = p_elem <* skip_ws <* string ";" in
              let* tl = p_semi_list in
              return (hd :: tl))
           ; (let* hd = p_elem <* skip_ws <* string "]" in
              return [ hd ])
           ; skip_ws *> string "]" *> return []
           ])
     in
     list
;;

let p_semicolon_list_expr p_expr = p_semicolon_list p_expr >>| fun l -> List l
let p_semicolon_list_pat p_pat = p_semicolon_list p_pat >>| fun l -> PList l
let p_unit = skip_ws *> string "(" *> skip_ws *> string ")" *> return Unit_lt
let p_unit_expr = expr_const_factory p_unit
let p_unit_pat = pat_const_factory p_unit

(* EXPR PARSERS *)
let p_parens p = skip_ws *> char '(' *> skip_ws *> p <* skip_ws <* char ')'
let make_binexpr op expr1 expr2 = Bin_expr (op, expr1, expr2) [@@inline always]
let make_unexpr op expr = Unary_expr (op, expr) [@@inline always]
let make_tuple_expr e1 e2 rest = Tuple (e1, e2, rest) [@@inline always]
let make_tuple_pat p1 p2 rest = PTuple (p1, p2, rest)
let p_binexpr binop_str binop = skip_ws *> string binop_str *> return (make_binexpr binop)
let p_unexpr unop_str unop = skip_ws *> string unop_str *> return (make_unexpr unop)
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
let bitwise_or = p_binexpr "|||" Binary_or_bitwise
let bitwise_and = p_binexpr "&&&" Binary_and_bitwise
let bitwise_xor = p_binexpr "^^^" Binary_xor_bitwise
let cons = p_binexpr "::" Binary_cons

let p_cons_list_pat p_pat =
  chainr1 p_pat (skip_ws *> string "::" *> return (fun l r -> PCons (l, r)))
;;

let p_tuple make p =
  let tuple =
    let* fst = p <* skip_ws <* string "," in
    let* snd = p in
    let* rest = many (skip_ws *> string "," *> p) in
    return (make fst snd rest)
  in
  p_parens tuple <|> tuple
;;

let p_tuple_pat p_pat = p_tuple make_tuple_pat p_pat

let p_if p_expr =
  lift3
    (fun cond th el -> If_then_else (cond, th, el))
    (skip_ws *> string "if" *> peek_sep1 *> p_expr)
    (skip_ws *> string "then" *> peek_sep1 *> p_expr)
    (skip_ws
     *> string "else"
     *> peek_sep1
     *> (p_expr <* peek_sep1 >>= fun e -> return (Some e))
     <|> return None)
;;

let p_option p make_option =
  skip_ws *> string "None" *> peek_sep1 *> return (make_option None)
  <|> let+ inner = skip_ws *> string "Some" *> peek_sep1 *> p in
      make_option (Some inner)
;;

let make_option_expr expr = Option expr
let make_option_pat pat = POption pat
let p_wild_pat = skip_ws *> string "_" *> return Wild

let p_pat_const =
  choice [ p_int_pat; p_bool_pat; p_unit_pat; p_string_pat; p_var_pat; p_wild_pat ]
;;

let p_constraint_pat p_pat =
  let* pat = p_pat in
  let* typ = p_type in
  return (PConstraint (pat, typ))
;;

let p_pat =
  skip_ws
  *> fix (fun self ->
    let atom = choice [ p_pat_const; p_parens self; p_parens (p_constraint_pat self) ] in
    let semicolon_list = p_semicolon_list_pat (self <|> atom) <|> atom in
    let opt = p_option semicolon_list make_option_pat <|> semicolon_list in
    let cons = p_cons_list_pat opt in
    let tuple = p_tuple_pat cons <|> cons in
    tuple)
;;

let p_let_bind p_expr =
  let* name = p_pat <|> (p_parens p_inf_oper >>| fun oper -> PVar oper) in
  let* args = many p_pat in
  let* body = skip_ws *> string "=" *> p_expr in
  return (Let_bind (name, args, body))
;;

let p_letin p_expr =
  skip_ws
  *> string "let"
  *> skip_ws_sep1
  *>
  let* rec_flag = string "rec" *> peek_sep1 *> return Rec <|> return Nonrec in
  let* let_bind1 = p_let_bind p_expr in
  let* let_binds = many (skip_ws *> string "and" *> peek_sep1 *> p_let_bind p_expr) in
  let* in_expr = skip_ws *> string "in" *> peek_sep1 *> p_expr in
  return (LetIn (rec_flag, let_bind1, let_binds, in_expr))
;;

let p_let p_expr =
  skip_ws
  *> string "let"
  *> skip_ws_sep1
  *>
  let* rec_flag = string "rec" *> peek_sep1 *> return Rec <|> return Nonrec in
  let* let_bind1 = p_let_bind p_expr in
  let* let_binds = many (skip_ws *> string "and" *> peek_sep1 *> p_let_bind p_expr) in
  return (Let (rec_flag, let_bind1, let_binds))
;;

let p_apply p_expr =
  chainl1 (p_expr <* peek_sep1) (return (fun expr1 expr2 -> Apply (expr1, expr2)))
;;

let p_lambda p_expr =
  skip_ws
  *> string "fun"
  *> peek_sep1
  *>
  let* arg1 = p_pat in
  let* args = many p_pat <* skip_ws <* string "->" in
  let* body = p_expr in
  return (Lambda (arg1, args, body))
;;

let p_case p_expr =
  let* pat = skip_ws *> string "|" *> p_pat <* skip_ws <* string "->" in
  let* expr = p_expr in
  return (pat, expr)
;;

let p_first_case p_expr =
  let* pat = skip_ws *> (string "|" *> p_pat <|> p_pat) <* skip_ws <* string "->" in
  let* expr = p_expr in
  return (pat, expr)
;;

let p_match p_expr =
  let* value = skip_ws *> string "match" *> p_expr <* skip_ws <* string "with" in
  let* pat1, expr1 = p_first_case p_expr in
  let* cases = many (p_case p_expr) in
  return (Match (value, (pat1, expr1), cases))
;;

let p_function p_expr =
  skip_ws
  *> string "function"
  *>
  let* pat1, expr1 = p_first_case p_expr in
  let* cases = many (p_case p_expr) in
  return (Function ((pat1, expr1), cases))
;;

let p_inf_oper_expr p_expr =
  skip_ws
  *> chainl1
       p_expr
       (p_inf_oper
        >>= fun op ->
        return (fun expr1 expr2 -> Apply (Apply (Variable op, expr1), expr2)))
;;

let p_constraint_expr p_expr =
  let* expr = p_expr in
  let* typ = p_type in
  return (EConstraint (expr, typ))
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
        ; p_parens (p_constraint_expr p_expr)
        ]
    in
    let if_expr = p_if (p_expr <|> atom) <|> atom in
    let letin_expr = p_letin (p_expr <|> if_expr) <|> if_expr in
    let option = p_option letin_expr make_option_expr <|> letin_expr in
    let apply = p_apply option <|> option in
    let unary = choice [ unary_chain p_not apply; unary_chain unminus apply ] in
    let factor = chainl1 unary (mul <|> div) in
    let term = chainl1 factor (add <|> sub) in
    let cons_op = chainr1 term cons in
    let comp_eq = chainl1 cons_op (equal <|> unequal) in
    let comp_less = chainl1 comp_eq (less_or_equal <|> less) in
    let comp_gr = chainl1 comp_less (greater_or_equal <|> greater) in
    let bit_xor = chainl1 comp_gr bitwise_xor in
    let bit_and = chainl1 bit_xor bitwise_and in
    let bit_or = chainl1 bit_and bitwise_or in
    let comp_and = chainl1 bit_or log_and in
    let comp_or = chainl1 comp_and log_or in
    let inf_oper = p_inf_oper_expr comp_or <|> comp_or in
    let tuple = p_tuple make_tuple_expr inf_oper <|> inf_oper in
    let p_function = p_function (p_expr <|> tuple) <|> tuple in
    let ematch = p_match (p_expr <|> p_function) <|> p_function in
    let efun = p_lambda (p_expr <|> ematch) <|> ematch in
    efun)
;;

let p_statement = p_let p_expr

let p_construction =
  p_expr >>= (fun e -> return (Expr e)) <|> (p_statement >>= fun s -> return (Statement s))
;;

(* MAIN PARSE FUNCTION *)
let parse (str : string) =
  parse_string ~consume:All (skip_ws *> p_construction <* skip_ws) str
;;
