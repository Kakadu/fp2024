(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Angstrom

(* ==================== Utils ==================== *)

let skip_whitespaces = skip_while Char.is_whitespace
let skip_whitespaces1 = take_while1 Char.is_whitespace

let parse_comments =
  skip_whitespaces *> string "(*" *> many_till any_char (string "*)") *> return ()
;;

let ws = many parse_comments *> skip_whitespaces
let ws1 = skip_whitespaces1 *> many parse_comments <|> many1 parse_comments
let skip_parens parse_ = ws *> char '(' *> ws *> parse_ <* ws <* char ')'

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

(* ==================== Ident ==================== *)

let parse_ident =
  ws
  *>
  let* start_ident =
    satisfy (function
      | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false)
    >>| String.of_char
  in
  let* parse_rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  let ident = String.( ^ ) start_ident parse_rest in
  if is_keyword ident then fail ident else return ident
;;

(* ==================== Rec_flag ==================== *)

let parse_rec_flag = ws1 *> option Nonrecursive (string "rec" *> ws1 *> return Recursive)

(* ==================== Constant ==================== *)

let parse_const_int =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| fun int_value -> Const_integer (Int.of_string int_value)
;;

let parse_const_char =
  char '\'' *> any_char <* char '\'' >>| fun char_value -> Const_char char_value
;;

let parse_const_string =
  char '\"' *> take_till (Char.equal '\"')
  <* char '\"'
  >>| fun str_value -> Const_string str_value
;;

let parse_constant =
  ws *> choice [ parse_const_int; parse_const_char; parse_const_string ]
;;

(* ==================== Pattern ==================== *)

let parse_pat_any = char '_' *> return Pat_any
let parse_pat_var = parse_ident >>| fun var -> Pat_var var
let parse_pat_constant = parse_constant >>| fun const -> Pat_constant const

let parse_pat_tuple parse_pat =
  let* first_pat = parse_pat in
  let* rest_pat_list = many1 (ws *> string "," *> parse_pat) in
  return (Pat_tuple (List.cons first_pat rest_pat_list))
;;

let parse_pattern =
  fix (fun parse_full_pat ->
    let parse_pat =
      choice
        [ skip_parens parse_full_pat; parse_pat_any; parse_pat_var; parse_pat_constant ]
    in
    parse_pat_tuple parse_pat <|> parse_pat)
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

let bin_op chain1 parse_exp parse_fun_op =
  chain1 parse_exp (parse_fun_op >>| fun op exp1 exp2 -> Exp_apply (op, [ exp1; exp2 ]))
;;

let parse_left_bin_op = bin_op parse_chain_left_associative

let select_operator op_list =
  choice (List.map ~f:(fun op -> ws *> string op *> return (Exp_ident op)) op_list)
;;

let mul_div = select_operator [ "*"; "/" ]
let add_sub = select_operator [ "+"; "-" ]
let cmp = select_operator [ ">="; "<="; "<>"; "="; ">"; "<" ]

let parse_operator parse_exp =
  let parse_cur_exp = parse_left_bin_op parse_exp mul_div in
  let parse_cur_exp = parse_left_bin_op parse_cur_exp add_sub in
  parse_left_bin_op parse_cur_exp cmp
;;

(* -------------------- Value_binding -------------------- *)

let parse_fun_binding parse_exp =
  let* name = ws *> parse_pat_var in
  let* args = ws *> sep_by1 ws parse_pattern in
  let* exp = ws *> char '=' *> parse_exp in
  return { pat = name; exp = Exp_fun (args, exp) }
;;

let parse_simple_binding parse_exp =
  let* pat = parse_pattern in
  let* exp = ws *> char '=' *> ws *> parse_exp in
  return { pat; exp }
;;

let parse_value_binding_list parse_exp =
  sep_by1
    (ws *> string "and")
    (parse_fun_binding parse_exp <|> parse_simple_binding parse_exp)
;;

(* -------------------- Expression -------------------- *)

let parse_exp_ident = parse_ident >>| fun ident -> Exp_ident ident
let parse_exp_constant = parse_constant >>| fun const -> Exp_constant const

let parse_exp_let parse_exp =
  ws
  *> string "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* value_binding = ws *> parse_value_binding_list parse_exp <* ws <* string "in" in
  let* expression = ws *> parse_exp in
  return (Exp_let (rec_flag, value_binding, expression))
;;

let parse_exp_apply_fun parse_exp =
  let* var = parse_exp in
  many parse_exp
  >>| fun exp_list -> if List.length exp_list = 0 then var else Exp_apply (var, exp_list)
;;

let parse_exp_apply parse_exp =
  let parse_cur_exp = parse_exp_apply_fun parse_exp in
  parse_operator parse_cur_exp
;;

let parse_cases parse_exp =
  let parse_case =
    let* pat = parse_pattern <* ws <* string "->" in
    let* exp = ws *> parse_exp in
    return { left = pat; right = exp }
  in
  option () (char '|' *> return ()) *> sep_by1 (ws *> char '|' *> ws) parse_case
;;

let parse_exp_match parse_exp =
  let* expression = ws *> string "match" *> ws *> parse_exp <* ws <* string "with" in
  let* cases = ws *> parse_cases parse_exp in
  return (Exp_match (expression, cases))
;;

let parse_exp_tuple parse_exp =
  let* first_exp = parse_exp in
  let* rest_exp_list = many1 (ws *> string "," *> parse_exp) in
  return (Exp_tuple (List.cons first_exp rest_exp_list))
;;

let parse_exp_ifthenelse parse_exp =
  let* if_ = ws *> string "if" *> parse_exp in
  let* then_ = ws *> string "then" *> parse_exp in
  let* else_ =
    option None (ws *> string "else" >>| Option.some)
    >>= function
    | None -> return None
    | Some _ -> parse_exp >>| Option.some
  in
  return (Exp_ifthenelse (if_, then_, else_))
;;

let parse_expression =
  ws
  *> fix (fun parse_full_exp ->
    let parse_exp =
      choice
        [ skip_parens parse_full_exp
        ; parse_exp_ident
        ; parse_exp_constant
        ; parse_exp_let parse_full_exp
        ; parse_exp_match parse_full_exp
        ; parse_exp_ifthenelse parse_full_exp
        ]
    in
    let parse_exp = parse_exp_apply parse_exp in
    parse_exp_tuple parse_exp <|> parse_exp)
;;

(* ==================== Structure ==================== *)

let parse_struct_value =
  string "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* value_binding_list = parse_value_binding_list parse_expression in
  return (Struct_value (rec_flag, value_binding_list))
;;

let parse_structure =
  let parse_structure_item =
    ws *> (parse_expression >>| (fun exp -> Struct_eval exp) <|> parse_struct_value)
  in
  let semicolons = many (ws *> string ";;") in
  sep_by semicolons parse_structure_item <* semicolons <* ws
;;

(* ==================== Execute ==================== *)

let parse = parse_string ~consume:All parse_structure
