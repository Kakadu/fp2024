(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Angstrom

(* ==================== Utils ==================== *)

let skip_whitespaces = skip_while Char.is_whitespace

let parse_comments =
  skip_whitespaces *> string "(*" *> many_till any_char (string "*)") *> return ()
;;

let ws = many parse_comments *> skip_whitespaces

let skip_parens parse_ =
  ws *> (string "(" *> ws *> parse_)
  <* (ws <* string ")" <|> fail "There is no closing bracket.")
;;

let is_separator = function
  | '(' | '[' | '\'' | '"' | ' ' | '\t' | '\n' -> true
  | _ -> false
;;

let is_keyword = function
  (* https://ocaml.org/manual/5.2/lex.html#sss:keywords *)
  | "and"
  | "else"
  | "false"
  | "fun"
  | "function"
  | "if"
  | "in"
  | "let"
  | "match"
  | "rec"
  | "then"
  | "true"
  | "with" -> true
  | _ -> false
;;

let keyword str =
  string str
  *>
  let* char_value = peek_char_fail in
  if is_separator char_value
  then return str
  else fail (Printf.sprintf "There is no separator after \"%s\"." str)
;;

(* ==================== Ident ==================== *)

let parse_ident =
  ws
  *>
  let* start_ident = satisfy Char.is_alpha >>| String.of_char in
  let* rest_ident =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  let ident = start_ident ^ rest_ident in
  if is_keyword ident
  then fail (Printf.sprintf "Impossible name: \"%s\"." ident)
  else return ident
;;

(* ==================== Rec_flag ==================== *)

let parse_rec_flag = ws *> option Nonrecursive (keyword "rec" *> return Recursive)

(* ==================== Constant ==================== *)

let parse_const_int =
  take_while1 Char.is_digit >>| fun int_value -> Const_integer (Int.of_string int_value)
;;

let parse_const_char =
  string "\'" *> any_char <* string "\'" >>| fun char_value -> Const_char char_value
;;

let parse_const_string =
  string "\"" *> take_till (Char.equal '\"')
  <* string "\""
  >>| fun str_value -> Const_string str_value
;;

let parse_constant =
  ws *> choice [ parse_const_int; parse_const_char; parse_const_string ]
;;

(* ==================== Pattern ==================== *)

let parse_pat_any = ws *> string "_" *> return Pat_any
let parse_pat_var = parse_ident >>| fun var -> Pat_var var
let parse_pat_constant = parse_constant >>| fun const -> Pat_constant const

let parse_pat_tuple parse_pat =
  let* first_pat = parse_pat in
  let* rest_pats = many1 (ws *> string "," *> parse_pat) in
  return (Pat_tuple (List.cons first_pat rest_pats))
;;

let parse_bool_pat =
  ws *> keyword "true" <|> keyword "false" >>| fun name -> Pat_construct (name, None)
;;

let parse_pat_construct parse_pat =
  let parse_elements =
    ws *> string "[" *> sep_by (ws *> string ";") parse_pat
    <* string "]"
    >>| fun list_ ->
    List.fold_right
      list_
      ~init:(Pat_construct ("[]", None))
      ~f:(fun pat acc -> Pat_construct ("::", Some (Pat_tuple [ pat; acc ])))
  in
  parse_elements <|> parse_bool_pat
;;

let parse_pattern =
  fix (fun parse_full_pat ->
    let parse_pat =
      choice
        [ skip_parens parse_full_pat
        ; parse_pat_any
        ; parse_pat_var
        ; parse_pat_constant
        ; parse_pat_construct parse_full_pat
        ]
    in
    choice [ parse_pat_tuple parse_pat; parse_pat ])
;;

(* ==================== Expression ==================== *)

(* -------------------- Operator -------------------- *)

let parse_chain_left_associative parse_exp parse_fun_op =
  let rec go acc_exp =
    (let* fun_op = parse_fun_op in
     let* exp = parse_exp in
     go (fun_op acc_exp exp))
    <|> return acc_exp
  in
  let* first_exp = parse_exp in
  go first_exp
;;

let parse_chain_right_associative parse_exp parse_fun_op =
  let rec go acc_exp =
    (let* fun_op = parse_fun_op in
     let* exp = parse_exp in
     let* next_exp = go exp in
     return (fun_op acc_exp next_exp))
    <|> return acc_exp
  in
  let* first_exp = parse_exp in
  go first_exp
;;

let bin_op chain1 parse_exp parse_fun_op =
  chain1 parse_exp (parse_fun_op >>| fun op exp1 exp2 -> Exp_apply (op, [ exp1; exp2 ]))
;;

let parse_left_bin_op = bin_op parse_chain_left_associative
let parse_right_bin_op = bin_op parse_chain_right_associative

let parse_operator ops =
  choice (List.map ~f:(fun op -> ws *> string op *> return (Exp_ident op)) ops)
;;

let mul_div = parse_operator [ "*"; "/" ]
let add_sub = parse_operator [ "+"; "-" ]
let cmp = parse_operator [ ">="; "<="; "<>"; "="; ">"; "<" ]
let and_ = parse_operator [ "&&" ]
let or_ = parse_operator [ "||" ]

(* -------------------- Value_binding -------------------- *)

let parse_fun_binding parse_exp =
  let* name = ws *> parse_pat_var in
  let* args = ws *> sep_by1 ws parse_pattern in
  let* exp = ws *> string "=" *> parse_exp in
  return { pat = name; exp = Exp_fun (args, exp) }
;;

let parse_simple_binding parse_exp =
  let* pat = parse_pattern in
  let* exp = ws *> string "=" *> ws *> parse_exp in
  return { pat; exp }
;;

let parse_value_binding_list parse_exp =
  sep_by1
    (ws *> keyword "and")
    (parse_fun_binding parse_exp <|> parse_simple_binding parse_exp)
;;

(* -------------------- Expression -------------------- *)

let parse_exp_ident = parse_ident >>| fun ident -> Exp_ident ident
let parse_exp_constant = parse_constant >>| fun const -> Exp_constant const

let parse_exp_let parse_exp =
  ws
  *> keyword "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* value_binding = ws *> parse_value_binding_list parse_exp <* ws <* keyword "in" in
  let* exp = ws *> parse_exp in
  return (Exp_let (rec_flag, value_binding, exp))
;;

let parse_exp_fun parse_exp =
  ws
  *> keyword "fun"
  *>
  let* pats = many1 parse_pattern <* ws <* string "->" in
  let* exp = ws *> parse_exp in
  return (Exp_fun (pats, exp))
;;

let parse_exp_apply_fun parse_exp =
  let* var = parse_exp in
  many parse_exp >>| fun exps -> if List.is_empty exps then var else Exp_apply (var, exps)
;;

let parse_exp_apply_op parse_exp =
  let parse_cur_exp = parse_left_bin_op parse_exp mul_div in
  let parse_cur_exp = parse_left_bin_op parse_cur_exp add_sub in
  let parse_cur_exp = parse_left_bin_op parse_cur_exp cmp in
  let parse_cur_exp = parse_right_bin_op parse_cur_exp and_ in
  parse_right_bin_op parse_cur_exp or_
;;

let parse_exp_apply parse_exp =
  let parse_cur_exp = parse_exp_apply_fun parse_exp in
  parse_exp_apply_op parse_cur_exp
;;

let parse_cases parse_exp =
  let parse_case =
    let* pat = parse_pattern <* ws <* string "->" in
    let* exp = ws *> parse_exp in
    return { left = pat; right = exp }
  in
  option () (string "|" *> return ()) *> sep_by1 (ws *> string "|" *> ws) parse_case
;;

let parse_exp_match parse_exp =
  let* exp = ws *> keyword "match" *> ws *> parse_exp <* ws <* keyword "with" in
  let* cases = ws *> parse_cases parse_exp in
  return (Exp_match (exp, cases))
;;

let parse_exp_tuple parse_exp =
  let* first_exp = parse_exp in
  let* rest_exps = many1 (ws *> string "," *> parse_exp) in
  return (Exp_tuple (List.cons first_exp rest_exps))
;;

let parse_bool_exp =
  ws *> keyword "true" <|> keyword "false" >>| fun name -> Exp_construct (name, None)
;;

let parse_exp_construct parse_exp =
  let parse_elements =
    ws *> string "[" *> sep_by (ws *> string ";") parse_exp
    <* ws
    <* string "]"
    >>| fun list_ ->
    List.fold_right
      list_
      ~init:(Exp_construct ("[]", None))
      ~f:(fun exp acc -> Exp_construct ("::", Some (Exp_tuple [ exp ; acc ])))
  in
  parse_elements <|> parse_bool_exp
;;

let parse_exp_ifthenelse parse_exp =
  let* if_ = ws *> keyword "if" *> parse_exp in
  let* then_ = ws *> keyword "then" *> parse_exp in
  let* else_ =
    option None (ws *> keyword "else" >>| Option.some)
    >>= function
    | None -> return None
    | Some _ -> parse_exp >>| Option.some
  in
  return (Exp_ifthenelse (if_, then_, else_))
;;

let parse_exp_sequence parse_exp =
  parse_chain_left_associative
    parse_exp
    (ws *> string ";" *> return (fun exp1 exp2 -> Exp_sequence (exp1, exp2)))
;;

let parse_expression =
  ws
  *> fix (fun parse_full_exp ->
    let parse_exp =
      choice
        [ parse_exp_ident
        ; parse_exp_constant
        ; skip_parens parse_full_exp
        ; parse_exp_let parse_full_exp
        ; parse_exp_fun parse_full_exp
        ; parse_exp_match parse_full_exp
        ; parse_exp_ifthenelse parse_full_exp
        ]
    in
    let parse_exp = parse_exp_construct parse_exp <|> parse_exp in
    let parse_exp = parse_exp_sequence parse_exp in
    let parse_exp = parse_exp_apply parse_exp in
    parse_exp_tuple parse_exp <|> parse_exp)
;;

(* ==================== Structure ==================== *)

let parse_struct_value =
  keyword "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* value_binding_list = parse_value_binding_list parse_expression in
  ws
  *> option
       (Struct_value (rec_flag, value_binding_list))
       (ws *> keyword "in" *> parse_expression
        >>| fun exp -> Struct_eval (Exp_let (rec_flag, value_binding_list, exp)))
;;

let parse_structure =
  let parse_structure_item =
    ws *> (parse_struct_value <|> (parse_expression >>| fun exp -> Struct_eval exp))
  in
  let semicolons = many (ws *> string ";;") in
  sep_by semicolons parse_structure_item <* semicolons <* ws
;;

(* ==================== Execute ==================== *)

let parse = parse_string ~consume:All parse_structure
