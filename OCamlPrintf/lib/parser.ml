(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Angstrom

(* ==================== Utils ==================== *)

let skip_whitespaces = skip_while Char.is_whitespace

let parse_comments =
  skip_whitespaces *> string "(*" *> many_till any_char (string "*)") <* return ()
;;

let ws = many parse_comments *> skip_whitespaces
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

let parse_ident =
  ws
  *>
  let parse_start =
    satisfy (function
      | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false)
    >>| String.of_char
  in
  let parse_rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  lift2 String.( ^ ) parse_start parse_rest
  >>= fun ident -> if is_keyword ident then fail ident else return ident
;;

(* ==================== Rec_flag ==================== *)

let parse_rec_flag = ws *> option Nonrecursive (string "rec " *> return Recursive)

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
  lift2 List.cons parse_pat (many1 (ws *> string ", " *> parse_pat))
  >>| fun pat_list -> Pat_tuple pat_list
;;

let parse_pattern =
  fix (fun parse_full_pat ->
    let parse_cur_pat =
      choice
        [ skip_parens parse_full_pat; parse_pat_any; parse_pat_var; parse_pat_constant ]
    in
    let parse_cur_pat = parse_pat_tuple parse_cur_pat <|> parse_cur_pat in
    parse_cur_pat)
;;

(* ==================== Expression ==================== *)

(* -------------------- Operator -------------------- *)

let parse_chain_left_associative (parse_exp : expression t) parse_fun_op =
  let rec go acc_exp =
    (let* fun_op = parse_fun_op in
     let* cur_exp = parse_exp in
     go (fun_op acc_exp cur_exp))
    <|> return acc_exp
  in
  let* start_exp = parse_exp in
  go start_exp
;;

let bin_op chain1 (parse_exp : expression t) parse_fun_op : expression t =
  chain1 parse_exp (parse_fun_op >>| fun op exp1 exp2 -> Exp_apply (op, [ exp1; exp2 ]))
;;

let parse_left_bin_op = bin_op parse_chain_left_associative

let select_operator operators =
  choice (List.map ~f:(fun op -> ws *> string op *> return (Exp_ident op)) operators)
;;

let mul_div = select_operator [ "*"; "/" ]
let add_sub = select_operator [ "+"; "-" ]
let cmp = select_operator [ ">="; "<="; "<>"; "="; ">"; "<" ]

let parse_operator parse_exp =
  let parse_cur_exp = parse_left_bin_op parse_exp mul_div in
  let parse_cur_exp = parse_left_bin_op parse_cur_exp add_sub in
  parse_left_bin_op parse_cur_exp cmp
;;

(* -------------------- Expression -------------------- *)

let parse_exp_ident = parse_ident >>| fun ident -> Exp_ident ident
let parse_exp_constant = parse_constant >>| fun const -> Exp_constant const

let parse_cases parse_exp =
  let parse_case =
    lift2
      (fun pat exp -> { left = pat; right = exp })
      (parse_pattern <* ws <* string "-> ")
      (ws *> parse_exp)
  in
  option () (char '|' *> return ()) *> sep_by1 (ws *> char '|' *> ws) parse_case
;;

let parse_exp_match parse_exp =
  lift2
    (fun expression cases -> Exp_match (expression, cases))
    (ws *> string "match " *> ws *> parse_exp <* ws <* string "with")
    (ws *> parse_cases parse_exp)
;;

let parse_exp_tuple parse_exp =
  lift2 List.cons parse_exp (many1 (ws *> string ", " *> parse_exp))
  >>| fun exp_list -> Exp_tuple exp_list
;;

let parse_exp_ifthenelse parse_exp =
  lift3
    (fun if_ then_ else_ -> Exp_ifthenelse (if_, then_, else_))
    (ws *> string "if " *> parse_exp)
    (ws *> string "then " *> parse_exp)
    (option None (ws *> string "else " >>| Option.some)
     >>= function
     | None -> return None
     | Some _ -> parse_exp >>| Option.some)
;;

let parse_expression =
  ws
  *> fix (fun parse_full_exp ->
    let parse_cur_exp =
      choice [ skip_parens parse_full_exp; parse_exp_ident; parse_exp_constant ]
    in
    let parse_cur_exp =
      parse_chain_left_associative
        parse_cur_exp
        (return (fun exp1 exp2 -> Exp_apply (exp1, [ exp2 ])))
    in
    let parse_cur_exp = parse_operator parse_cur_exp in
    let parse_cur_exp = parse_exp_tuple parse_cur_exp <|> parse_cur_exp in
    choice
      [ parse_exp_ifthenelse parse_full_exp
      ; parse_exp_match parse_full_exp
      ; parse_cur_exp
      ])
;;

(* ==================== Value_binding ==================== *)

let parse_fun_binding =
  lift3
    (fun name args exp -> { pat = name; exp = Exp_fun (args, exp) })
    (ws *> parse_pat_var)
    (ws *> sep_by1 ws parse_pattern)
    (ws *> char '=' *> parse_expression)
;;

let parse_simple_binding =
  lift2 (fun pat exp -> { pat; exp }) parse_pattern (ws *> char '=' *> parse_expression)
;;

let parse_value_binding_list =
  sep_by1 (ws *> string "and ") (parse_fun_binding <|> parse_simple_binding)
;;

(* ==================== Structure ==================== *)

let parse_struct_value =
  string "let "
  *> lift2
       (fun rec_flag value_bindings -> Struct_value (rec_flag, value_bindings))
       parse_rec_flag
       parse_value_binding_list
;;

let parse_structure =
  let parse_structure_item =
    ws *> parse_struct_value <|> (parse_expression >>| fun exp -> Struct_eval exp)
  in
  let semicolons = many (ws *> string ";;") in
  sep_by semicolons parse_structure_item <* semicolons <* ws
;;

(* ==================== Execute ==================== *)

let parse = parse_string ~consume:All parse_structure
