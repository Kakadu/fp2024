(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Angstrom

let skip_whitespaces = skip_while Char.is_whitespace

let parse_comments =
  skip_whitespaces *> string "(*" *> many_till any_char (string "*)") *> return ()
;;

let ws = many parse_comments *> skip_whitespaces
let token str = ws *> string str

let skip_round_par parse =
  token "(" *> parse <* (token ")" <|> fail "There is no closing bracket.")
;;

let skip_square_par parse =
  token "[" *> parse <* (token "]" <|> fail "There is no closing bracket.")
;;

let is_keyword = function
  | "let"
  | "rec"
  | "and"
  | "if"
  | "then"
  | "else"
  | "match"
  | "with"
  | "in"
  | "true"
  | "false"
  | "Some"
  | "None"
  | "type"
  | "val"
  | "while"
  | "for"
  | "_" -> true
  | _ -> false
;;

let is_separator = function
  | ')'
  | '('
  | '<'
  | '>'
  | '@'
  | ','
  | ';'
  | ':'
  | '\\'
  | '"'
  | '/'
  | '['
  | ']'
  | '?'
  | '='
  | '{'
  | '}'
  | ' '
  | '\r'
  | '\t'
  | '\n'
  | '*'
  | '-' -> true
  | _ -> false
;;

let keyword str =
  token str
  *>
  let* is_space =
    peek_char
    >>| function
    | Some c -> is_separator c
    | None -> true
  in
  if is_space
  then return str <* ws
  else fail (Printf.sprintf "There is no separator after %S." str)
;;

let safe_tl = function
  | [] -> []
  | _ :: tail -> tail
;;

let parse_chain_left_associative parse parse_fun =
  let rec go acc =
    (let* f = parse_fun in
     let* elem = parse in
     go (f acc elem))
    <|> return acc
  in
  let* elem = parse in
  go elem
;;

let parse_chain_right_associative parse parse_fun =
  let rec go acc =
    (let* f = parse_fun in
     let* elem = parse in
     let* next_elem = go elem in
     return (f acc next_elem))
    <|> return acc
  in
  let* elem = parse in
  go elem
;;

(* ================= recursion flag ================= *)

let parse_rec_flag = ws *> option NonRec (keyword "rec" *> return Rec)

(* ==================== constant ==================== *)

let parse_const_int =
  take_while1 Char.is_digit >>| fun int_value -> Int (Int.of_string int_value)
;;

let parse_const_char =
  string "\'" *> any_char <* string "\'" >>| fun char_value -> Char char_value
;;

let parse_const_string =
  choice
    [ string "\"" *> take_till (Char.equal '\"') <* string "\""
    ; string "{|" *> take_till (Char.equal '|') <* string "|}"
    ]
  >>| fun str_value -> String str_value
;;

let parse_const_unit = string "()" >>| fun _ -> Unit

let parse_const_bool =
  string "true" >>| (fun _ -> Bool true) <|> (string "false" >>| fun _ -> Bool false)
;;

let parse_constant =
  ws
  *> choice
       [ parse_const_int
       ; parse_const_char
       ; parse_const_string
       ; parse_const_bool
       ; parse_const_unit
       ]
;;

(* ==================== ident ==================== *)

let parse_ident =
  ws
  *>
  let* fst_char =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
    >>| String.of_char
  in
  let* rest_str =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  let id = fst_char ^ rest_str in
  if is_keyword id then fail (Printf.sprintf "Impossible name: %S." id) else return id
;;

(* --------------------- type ---------------------- *)

let parse_base_type =
  choice
    [ keyword "unit" *> return TypUnit
    ; keyword "int" *> return TypInt
    ; keyword "char" *> return TypChar
    ; keyword "string" *> return TypStr
    ; keyword "bool" *> return TypBool
    ]
;;

let parse_tuple_type parse_type =
  let* fst_type = parse_type in
  let* snd_type = token "*" *> parse_type in
  let* type_list = many (token "*" *> parse_type) in
  return (TypTuple (fst_type, snd_type, type_list))
;;

let parse_list_type parse_type =
  let f acc_ty = function
    | "list" -> TypList acc_ty
    | _ -> failwith "Expected 'list'"
  in
  let rec go acc_ty =
    let* ty = keyword "list" in
    go (f acc_ty ty) <|> return acc_ty
  in
  let* fst_ty = parse_type in
  go fst_ty
;;

let parse_core_type =
  ws
  *> fix (fun parse_full_type ->
    let parse_type = parse_base_type <|> skip_round_par parse_full_type in
    let parse_type = parse_list_type parse_type <|> parse_type in
    parse_tuple_type parse_type <|> parse_type)
;;

(* -------------------- pattern -------------------- *)

let parse_pat_with_type parse_pat =
  let* pat = ws *> token "(" *> parse_pat in
  let* constr = ws *> token ":" *> ws *> parse_core_type <* ws <* token ")" in
  return (PatWithTyp (constr, pat))
;;

let parse_pat_any = keyword "_" *> return PatAny
let parse_pat_var = parse_ident >>| fun var -> PatVar var
let parse_pat_constant = parse_constant >>| fun const -> PatConst const

let parse_tuple parse tuple =
  let* fst = parse in
  let* snd = token "," *> parse in
  let* tail = many (token "," *> parse) in
  return (tuple (fst, snd, tail))
;;

let parse_pat_tuple parse_pat =
  parse_tuple parse_pat (fun (fst_pat, snd_pat, pat_list) ->
    PatTup (fst_pat, snd_pat, pat_list))
;;

let parse_pattern_option parse_pat =
  lift
    (fun e -> PatOption e)
    (keyword "Some" *> parse_pat
     >>| (fun e -> Some e)
     <|> (keyword "None" >>| fun _ -> None))
;;

let parse_list_construct_case_pattern parse_pat =
  let* first = parse_pat in
  let* rest = many1 @@ (token "::" *> parse_pat) in
  return (PatListConstructor (first :: rest))
;;

let parse_pattern_list parse_pat =
  let empty_list_parser =
    let* _ = token "[" *> token "]" in
    return (PatList [])
  in
  let list_parser =
    let* _ = token "[" in
    let* first = parse_pat in
    let* rest = many (token ";" *> parse_pat) in
    let* _ = token "]" in
    return (PatList (first :: rest))
  in
  empty_list_parser <|> list_parser
;;

let parse_pattern =
  ws
  *> fix (fun parse_full_pat ->
    let parse_pat =
      choice
        [ parse_pat_var
        ; parse_pat_with_type parse_full_pat
        ; parse_pat_any
        ; parse_pat_constant
        ; parse_pattern_list parse_full_pat
        ; skip_round_par parse_full_pat
        ; parse_pattern_option parse_full_pat
        ]
    in
    let parse_pat = parse_pat_tuple parse_pat <|> parse_pat in
    let parse_pat = parse_pattern_list parse_pat <|> parse_pat in
    let parse_pat = parse_list_construct_case_pattern parse_pat <|> parse_pat in
    parse_pat)
;;

(* -------------------- operator -------------------- *)

let cmp =
  choice
    [ token "=" *> return Equals
    ; token "<>" *> return NotEquals
    ; token "<=" *> return LessEquals
    ; token ">=" *> return GreaterEquals
    ; token "<" *> return LessThan
    ; token ">" *> return GreaterThan
    ]
;;

let logical = choice [ token "&&" *> return And; token "||" *> return Or ]
let add_sub = choice [ token "+" *> return Add; token "-" *> return Sub ]
let mult_div = choice [ token "/" *> return Div; token "*" *> return Mult ]

let bin_op chain1 parse_exp parse_fun_op =
  chain1 parse_exp (parse_fun_op >>| fun opr exp1 exp2 -> ExpBinOper (opr, exp1, exp2))
;;

let parse_left_bin_op = bin_op parse_chain_left_associative
let parse_right_bin_op = bin_op parse_chain_right_associative
let parse_un_oper = choice [ token "-" *> return Neg; keyword "not" *> return Not ]

(* -------------------- expression -------------------- *)

let parse_exp_with_type parse_exp =
  let* expr = ws *> token "(" *> parse_exp in
  let* constr = ws *> token ":" *> ws *> parse_core_type <* ws <* token ")" in
  return (ExpWithTyp (constr, expr))
;;

let parse_exp_ident = parse_ident >>| fun id -> ExpVar id
let parse_exp_constant = parse_constant >>| fun const -> ExpConst const

let parse_exp_tuple parse_exp =
  parse_tuple parse_exp (fun (fst_exp, snd_exp, exp_list) ->
    ExpTup (fst_exp, snd_exp, exp_list))
;;

let parse_list_construct_case_exp parse_exp =
  let* first = parse_exp in
  let* rest = many1 (token "::" *> parse_exp) in
  return (ExpListConstructor (first :: rest))
;;

let parse_exp_list parse_exp =
  let empty_list_parser =
    let* _ = token "[" *> token "]" in
    return (ExpList [])
  in
  let list_parser =
    let* _ = token "[" in
    let* first = parse_exp in
    let* rest = many (token ";" *> parse_exp) in
    let* _ = token "]" in
    return (ExpList (first :: rest))
  in
  empty_list_parser <|> list_parser
;;

let parse_exp_fun parse_exp =
  keyword "fun"
  *>
  let* pat = parse_pattern in
  let* params = many parse_pattern in
  token "->"
  *>
  let* body_exp = parse_exp in
  let exp =
    match params with
    | [] -> body_exp
    | _ -> List.fold_right ~f:(fun par acc -> ExpFun (par, acc)) params ~init:body_exp
  in
  return (ExpFun (pat, exp))
;;

let parse_exp_ifthenelse parse_expr =
  lift3
    (fun cond t f -> ExpIfThenElse (cond, t, f))
    (keyword "if" *> parse_expr)
    (keyword "then" *> parse_expr)
    (option None (keyword "else" *> parse_expr >>| Option.some))
;;

let parse_match_case parse_exp =
  ws
  *> option () (token "|" *> return ())
  *>
  let* pat = parse_pattern in
  let* exp = token "->" *> parse_exp in
  return { case_pat = pat; case_expr = exp }
;;

let parse_exp_match parse_exp =
  let* exp = keyword "match" *> parse_exp <* keyword "with" in
  let* case_list = sep_by1 (token "|") (parse_match_case parse_exp) in
  return (ExpMatch (exp, List.hd_exn case_list, safe_tl case_list))
;;

let parse_exp_function parse_exp =
  keyword "function"
  *>
  let* case_list = sep_by1 (token "|") (parse_match_case parse_exp) in
  return (ExpFunction (List.hd_exn case_list, safe_tl case_list))
;;

let parse_exp_bin_op parse_exp =
  let parse_exp = parse_left_bin_op parse_exp mult_div in
  let parse_exp = parse_left_bin_op parse_exp add_sub in
  let parse_exp = parse_left_bin_op parse_exp cmp in
  parse_right_bin_op parse_exp logical
;;

let parse_exp_un_oper parse_exp =
  parse_un_oper >>= fun op -> parse_exp >>= fun expr -> return (ExpUnOper (op, expr))
;;

let parse_exp_option parse_exp =
  choice
    [ keyword "None" *> return (ExpOption None)
    ; (keyword "Some" *> choice [ skip_round_par parse_exp; parse_exp ]
       >>| fun e -> ExpOption (Some e))
    ]
;;

let parse_binding parse_exp =
  let* pattern = parse_pattern in
  let* xs = many parse_pattern in
  let+ parse_exp = token "=" *> parse_exp in
  { pat = pattern
  ; expr =
      (match xs with
       | [] -> parse_exp
       | _ -> List.fold_right ~f:(fun f p -> ExpFun (f, p)) xs ~init:parse_exp)
  }
;;

let parse_exp_let parse_exp =
  keyword "let"
  *>
  let* rec_flag = keyword "rec" *> return Rec <|> return NonRec in
  let* vb = parse_binding parse_exp in
  let* value_bindings = many (keyword "and" *> parse_binding parse_exp) in
  let+ expr = keyword "in" *> parse_exp in
  ExpLet (rec_flag, vb, value_bindings, expr)
;;

let parse_exp_apply parse_exp =
  parse_chain_left_associative parse_exp (return (fun exp1 exp2 -> ExpApp (exp1, exp2)))
;;

let parse_expression =
  ws
  *> fix (fun expr ->
    let expr_const =
      choice
        [ skip_round_par expr
        ; parse_exp_option expr
        ; parse_exp_constant
        ; parse_exp_with_type expr
        ; parse_exp_ident
        ; parse_exp_ifthenelse expr
        ]
    in
    let expr_fun = parse_exp_fun expr <|> expr_const in
    let expr_list = parse_exp_list expr <|> expr_fun in
    let expr_apply = parse_exp_apply expr_list <|> expr_list in
    let expr_bin_op = parse_exp_bin_op expr_apply <|> expr_apply in
    let expr_un_op = parse_exp_un_oper expr_bin_op <|> expr_bin_op in
    let expr_cons = parse_list_construct_case_exp expr_un_op <|> expr_un_op in
    let expr_let_in = parse_exp_let expr <|> expr_cons in
    let expr_function = parse_exp_function expr <|> expr_let_in in
    let expr_match = parse_exp_match expr <|> expr_function in
    let expr_tuple = parse_exp_tuple expr_match <|> expr_match in
    expr_tuple)
;;

(* ==================== structure ==================== *)

let parse_structure_value parse_exp =
  keyword "let"
  *>
  let* rec_flag = keyword "rec" *> return Rec <|> return NonRec in
  let* vb = parse_binding parse_exp in
  let+ value_bindings = many (keyword "and" *> parse_binding parse_exp) in
  Binding (rec_flag, vb, value_bindings)
;;

let parse_structure =
  ws
  *>
  let str_value = parse_structure_value parse_expression in
  let str_eval = str_value <|> (parse_expression >>| fun ex -> EvalExp ex) in
  let semicolons = many (token ";;") in
  sep_by semicolons str_eval <* semicolons <* ws
;;

(* ==================== execute ==================== *)

let parse = parse_string ~consume:All parse_structure
