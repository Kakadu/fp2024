(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Ast.Expression
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
  | '\t'
  | '\n'
  | '*'
  | '-' -> true
  | _ -> false
;;

let keyword str =
  string str
  *>
  let* char_value = peek_char_fail in
  if is_separator char_value
  then return str
  else fail (Printf.sprintf "There is no separator after %S." str)
;;

(* ==================== Ident ==================== *)

let parse_ident =
  ws
  *>
  let* start_ident =
    satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
    >>| String.of_char
  in
  let* rest_ident =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  let ident = start_ident ^ rest_ident in
  if is_keyword ident
  then fail (Printf.sprintf "Impossible name: %S." ident)
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

(* =================== Core_type =================== *)

let parse_base_type =
  ws
  *> choice
       [ keyword "_" *> return Type_any
       ; keyword "int" *> return Type_int
       ; keyword "char" *> return Type_char
       ; keyword "string" *> return Type_string
       ; keyword "bool" *> return Type_bool
       ]
;;

let parse_list_type parse_type =
  ws *> (parse_base_type <|> skip_parens parse_type)
  <* ws
  <* keyword "list"
  >>= fun t -> return (Type_list t)
;;

let parse_tuple_type parse_type =
  let* first_type = ws *> parse_type in
  let* second_type = ws *> string "*" *> ws *> parse_type in
  let* type_list = many (ws *> string "*" *> ws *> parse_type) in
  return (Type_tuple (first_type, second_type, type_list))
;;

let rec parse_arrow_type parse_type =
  let* type1 = ws *> parse_type in
  let* _ = ws *> string "->" *> ws in
  let* type2 = parse_arrow_type parse_type <|> parse_type in
  return (Type_arrow (type1, type2))
;;

let parse_core_type =
  ws
  *> fix (fun parse_full_type ->
    let parse_type =
      choice
        [ parse_list_type parse_full_type; parse_base_type; skip_parens parse_full_type ]
    in
    let parse_type = parse_tuple_type parse_type <|> parse_type in
    parse_arrow_type parse_type <|> parse_type)
;;

(* ==================== Pattern ==================== *)

let parse_pat_any = ws *> keyword "_" *> return Pat_any
let parse_pat_var = parse_ident >>| fun var -> Pat_var var
let parse_pat_constant = parse_constant >>| fun const -> Pat_constant const

let parse_pat_tuple parse_pat =
  ws
  *>
  let* first_pat = parse_pat in
  ws
  *> string ","
  *>
  let* second_pat = parse_pat in
  let* pat_list = many (ws *> string "," *> parse_pat) in
  return (Pat_tuple (first_pat, second_pat, pat_list))
;;

let parse_pat_construct_keyword parse_pat =
  choice
    [ (let* id = ws *> choice [ keyword "true"; keyword "false"; keyword "None" ] in
       return (Pat_construct (id, None)))
    ; (let* id = ws *> keyword "Some" in
       let* arg = ws *> parse_pat >>| Option.some in
       return (Pat_construct (id, arg)))
    ]
;;

let parse_pat_construct parse_pat =
  let parse_elements =
    ws *> string "[" *> sep_by (ws *> string ";") parse_pat
    <* string "]"
    >>| fun list_ ->
    List.fold_right
      list_
      ~init:(Pat_construct ("[]", None))
      ~f:(fun pat acc -> Pat_construct ("::", Some (Pat_tuple (pat, acc, []))))
  in
  choice [ parse_elements; parse_pat_construct_keyword parse_pat ]
;;

let parse_pat_constraint parse_pat =
  ws
  *> string "("
  *>
  let* q = parse_pat in
  let* type' = ws *> string ":" *> parse_core_type <* string ")" in
  return (Pat_constraint (q, type'))
;;

let parse_pattern =
  fix (fun parse_full_pat ->
    let parse_pat =
      choice
        [ parse_pat_any
        ; parse_pat_var
        ; parse_pat_constant
        ; skip_parens parse_full_pat
        ; parse_pat_construct parse_full_pat
        ; parse_pat_constraint parse_full_pat
        ]
    in
    let parse_pat = parse_pat_construct parse_pat <|> parse_pat in
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
  chain1 parse_exp (parse_fun_op >>| fun op exp1 exp2 -> Exp_apply (op, exp1, [ exp2 ]))
;;

let parse_left_bin_op = bin_op parse_chain_left_associative
let parse_right_bin_op = bin_op parse_chain_right_associative

let parse_operator op_list =
  choice (List.map ~f:(fun op -> ws *> string op *> return (Exp_ident op)) op_list)
;;

let mul_div = parse_operator [ "*"; "/" ]
let add_sub = parse_operator [ "+"; "-" ]
let cmp = parse_operator [ ">="; "<="; "<>"; "="; ">"; "<" ]
let and_ = parse_operator [ "&&" ]
let or_ = parse_operator [ "||" ]

(* -------------------- Value_binding -------------------- *)

let parse_constraint parse_exp =
  let* exp = ws *> string "(" *> parse_exp in
  let* type' = ws *> string ":" *> parse_core_type <* ws <* string ")" in
  return (Exp_constraint (exp, type'))
;;

let parse_exp_constraint parse_exp op =
  let parse_exp_with_constraint =
    let* type' = ws *> string ":" *> parse_core_type in
    let* exp = ws *> string op *> parse_exp in
    return (Exp_constraint (exp, type'))
  in
  let parse_exp_without_constraint =
    let* exp = ws *> string op *> parse_exp in
    return exp
  in
  ws *> choice [ parse_exp_with_constraint; parse_exp_without_constraint ]
;;

let parse_fun_binding parse_exp =
  let* name = ws *> parse_pat_var in
  let* first_pat = ws *> parse_pattern in
  let* pat_list = ws *> sep_by ws parse_pattern in
  let* exp = parse_exp_constraint parse_exp "=" in
  return { pat = name; exp = Exp_fun (first_pat, pat_list, exp) }
;;

let parse_simple_binding parse_exp =
  let* pat = parse_pattern in
  let* exp = parse_exp_constraint parse_exp "=" in
  return { pat; exp }
;;

let parse_value_binding parse_exp =
  parse_fun_binding parse_exp <|> parse_simple_binding parse_exp
;;

let parse_value_binding_list parse_exp =
  ws
  *> option () (string "and" *> return ())
  *> sep_by
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
  let* first_value_binding = ws *> parse_value_binding parse_exp in
  let* value_binding_list =
    ws *> parse_value_binding_list parse_exp <* ws <* keyword "in"
  in
  let* exp = ws *> parse_exp in
  return (Exp_let (rec_flag, first_value_binding, value_binding_list, exp))
;;

let parse_exp_fun parse_exp =
  ws
  *> keyword "fun"
  *>
  let* first_pat = parse_pattern in
  let* pat_list = many parse_pattern in
  let* exp = parse_exp_constraint parse_exp "->" in
  return (Exp_fun (first_pat, pat_list, exp))
;;

let parse_exp_apply_fun parse_exp =
  let* var = parse_exp in
  many parse_exp
  >>| fun exp_list ->
  if List.is_empty exp_list
  then var
  else Exp_apply (var, List.hd_exn exp_list, List.tl_exn exp_list)
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

let parse_case parse_exp =
  option () (string "|" *> return ())
  *>
  let* pat = parse_pattern <* ws <* string "->" in
  let* exp = ws *> parse_exp in
  return { left = pat; right = exp }
;;

let parse_exp_match parse_exp =
  let* exp = ws *> keyword "match" *> ws *> parse_exp <* ws <* keyword "with" in
  let* first_case = ws *> parse_case parse_exp in
  let* case_list = ws *> sep_by (ws *> string "|" *> ws) (parse_case parse_exp) in
  return (Exp_match (exp, first_case, case_list))
;;

let parse_exp_tuple parse_exp =
  ws
  *>
  let* first_exp = parse_exp in
  ws
  *> string ","
  *>
  let* second_exp = parse_exp in
  let* exp_list = many (ws *> string "," *> parse_exp) in
  return (Exp_tuple (first_exp, second_exp, exp_list))
;;

let parse_exp_construct_keyword parse_exp =
  choice
    [ (let* id = ws *> choice [ keyword "true"; keyword "false"; keyword "None" ] in
       return (Exp_construct (id, None)))
    ; (let* id = ws *> keyword "Some" in
       let* arg = ws *> parse_exp >>| Option.some in
       return (Exp_construct (id, arg)))
    ]
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
      ~f:(fun exp acc -> Exp_construct ("::", Some (Exp_tuple (exp, acc, []))))
  in
  choice [ parse_elements; parse_exp_construct_keyword parse_exp ]
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
        ; parse_exp_construct parse_full_exp
        ; parse_constraint parse_full_exp
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
  let* first_value_binding = parse_value_binding parse_expression in
  let* value_binding_list = parse_value_binding_list parse_expression in
  ws
  *> option
       (Struct_value (rec_flag, first_value_binding, value_binding_list))
       (ws *> keyword "in" *> parse_expression
        >>| fun exp ->
        Struct_eval (Exp_let (rec_flag, first_value_binding, value_binding_list, exp)))
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
