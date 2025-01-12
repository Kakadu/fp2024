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
  | '\r'
  | '\t'
  | '\n'
  | '*'
  | '-' -> true
  | _ -> false
;;

let keyword str =
  string str
  *>
  let* is_space =
    peek_char
    >>| function
    | Some c -> is_separator c
    | None -> true
  in
  if is_space
  then return str
  else fail (Printf.sprintf "There is no separator after %S." str)
;;

let safe_tl = function
  | [] -> []
  | _ :: tail -> tail
;;

let parse_chain_left_associative parse_exp parse_fun_op =
  let rec go acc_exp =
    (let* fun_op = parse_fun_op in
     let* exp = parse_exp in
     go (fun_op acc_exp exp))
    <|> return acc_exp
  in
  let* fst_exp = parse_exp in
  go fst_exp
;;

let parse_chain_right_associative parse_exp parse_fun_op =
  let rec go acc_exp =
    (let* fun_op = parse_fun_op in
     let* exp = parse_exp in
     let* next_exp = go exp in
     return (fun_op acc_exp next_exp))
    <|> return acc_exp
  in
  let* fst_exp = parse_exp in
  go fst_exp
;;

(* ==================== Ident ==================== *)

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

(* The id obtained from parser is stored without first char ', while the id from
   inferencer is stored with ', so that there is no confusion when inferring types. *)
let parse_type_name =
  ws
  *> string "'"
  *>
  let* fst_char =
    satisfy (function
      | 'a' .. 'z' -> true
      | _ -> false)
    >>| String.of_char
  in
  let* snd_char =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
      | _ -> false)
  in
  let* rest_str =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  return (Type_var (fst_char ^ snd_char ^ rest_str))
;;

let parse_base_type =
  ws
  *> choice
       [ keyword "_" *> return Type_any
       ; keyword "unit" *> return Type_unit
       ; keyword "int" *> return Type_int
       ; keyword "char" *> return Type_char
       ; keyword "string" *> return Type_string
       ; keyword "bool" *> return Type_bool
       ]
;;

let parse_list_or_option_type parse_type =
  ws
  *> let* type' = parse_type in
     ws *> sep_by1 ws (keyword "option" <|> keyword "list")
     >>| List.fold ~init:type' ~f:(fun acc_type' id ->
       match id with
       | "option" -> Type_option acc_type'
       | _ -> Type_list acc_type')
;;

let parse_tuple_type parse_type =
  let* fst_type = ws *> parse_type in
  let* snd_type = ws *> string "*" *> ws *> parse_type in
  let* type_list = many (ws *> string "*" *> ws *> parse_type) in
  return (Type_tuple (fst_type, snd_type, type_list))
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
      choice [ parse_base_type; parse_type_name; skip_parens parse_full_type ]
    in
    let parse_type = parse_list_or_option_type parse_type <|> parse_type in
    let parse_type = parse_tuple_type parse_type <|> parse_type in
    parse_arrow_type parse_type <|> parse_type)
;;

(* ==================== Pattern & Expression ==================== *)

let parse_tuple parse tuple =
  ws
  *>
  let* fst = parse in
  ws
  *> string ","
  *>
  let* snd = parse in
  let* tail = many (ws *> string "," *> parse) in
  return (tuple (fst, snd, tail))
;;

let parse_construct_keyword parse construct =
  choice
    [ (let* id =
         ws *> choice [ keyword "true"; keyword "false"; keyword "None"; keyword "()" ]
       in
       return (construct (id, None)))
    ; (let* id = ws *> keyword "Some" in
       let* arg = ws *> parse >>| Option.some in
       return (construct (id, arg)))
    ]
;;

let parse_construct_list parse construct tuple =
  parse_chain_right_associative
    parse
    (ws
     *> string "::"
     *> return (fun acc elem -> construct ("::", Some (tuple (acc, elem, [])))))
;;

let parse_construct parse construct tuple =
  let parse_elements =
    ws *> string "[" *> sep_by (ws *> string ";") parse
    <* ws
    <* string "]"
    >>| List.fold_right
          ~init:(construct ("[]", None))
          ~f:(fun elem acc -> construct ("::", Some (tuple (elem, acc, []))))
  in
  parse_elements <|> parse_construct_keyword parse construct
;;

let parse_constraint parse constraint' =
  ws
  *> string "("
  *>
  let* elem = parse in
  let* type' = ws *> string ":" *> parse_core_type <* string ")" in
  return (constraint' (elem, type'))
;;

(* -------------------- Pattern -------------------- *)

let parse_pat_any = ws *> keyword "_" *> return Pat_any
let parse_pat_var = parse_ident >>| fun var -> Pat_var var
let parse_pat_constant = parse_constant >>| fun const -> Pat_constant const

let parse_pat_tuple parse_pat =
  parse_tuple parse_pat (fun (fst_pat, snd_pat, pat_list) ->
    Pat_tuple (fst_pat, snd_pat, pat_list))
;;

let parse_pat_construct parse_pat =
  parse_construct
    parse_pat
    (fun (tag, pat_opt) -> Pat_construct (tag, pat_opt))
    (fun (fst_pat, snd_pat, pat_list) -> Pat_tuple (fst_pat, snd_pat, pat_list))
;;

let parse_pat_construct_list parse_pat =
  parse_construct_list
    parse_pat
    (fun (tag, pat_opt) -> Pat_construct (tag, pat_opt))
    (fun (fst_pat, snd_pat, pat_list) -> Pat_tuple (fst_pat, snd_pat, pat_list))
;;

let parse_pat_constraint parse_pat =
  parse_constraint parse_pat (fun (pat, type') -> Pat_constraint (pat, type'))
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
    let parse_pat = parse_pat_construct_list parse_pat in
    parse_pat_tuple parse_pat <|> parse_pat)
;;

(* -------------------- Operator -------------------- *)

let bin_op chain1 parse_exp parse_fun_op =
  chain1
    parse_exp
    (parse_fun_op >>| fun opr exp1 exp2 -> Exp_apply (opr, Exp_apply (exp1, exp2)))
;;

let parse_left_bin_op = bin_op parse_chain_left_associative
let parse_right_bin_op = bin_op parse_chain_right_associative

let parse_operator op_list =
  choice (List.map ~f:(fun opr -> ws *> string opr *> return (Exp_ident opr)) op_list)
;;

let mul_div = parse_operator [ "*"; "/" ]
let add_sub = parse_operator [ "+"; "-" ]
let cmp = parse_operator [ ">="; "<="; "<>"; "="; ">"; "<" ]
let and_ = parse_operator [ "&&" ]
let or_ = parse_operator [ "||" ]

(* -------------------- Value_binding -------------------- *)

let parse_constraint_vb parse_exp opr =
  let* type' = ws *> string ":" *> parse_core_type in
  let* exp = ws *> string opr *> parse_exp in
  return (Exp_constraint (exp, type'))
;;

let parse_fun_binding parse_exp =
  let* name = ws *> parse_pat_var in
  let* pat_list = ws *> many1 parse_pattern in
  choice
    [ (let* exp = parse_constraint_vb parse_exp "=" in
       match exp with
       | Exp_constraint (exp, type') ->
         return
           { pat = Pat_constraint (name, type')
           ; exp = Exp_fun (List.hd_exn pat_list, safe_tl pat_list, exp)
           }
       | _ ->
         return
           { pat = name; exp = Exp_fun (List.hd_exn pat_list, safe_tl pat_list, exp) })
    ; (let* exp = ws *> string "=" *> parse_exp in
       return { pat = name; exp = Exp_fun (List.hd_exn pat_list, safe_tl pat_list, exp) })
    ]
;;

let parse_simple_binding parse_exp =
  let* pat = parse_pattern in
  choice
    [ (let* exp = parse_constraint_vb parse_exp "=" in
       match exp with
       | Exp_constraint (exp, type') -> return { pat = Pat_constraint (pat, type'); exp }
       | _ -> return { pat; exp })
    ; (let* exp = ws *> string "=" *> parse_exp in
       return { pat; exp })
    ]
;;

let parse_value_binding_list parse_exp =
  ws
  *> sep_by1
       (ws *> keyword "and")
       (parse_fun_binding parse_exp <|> parse_simple_binding parse_exp)
;;

(* -------------------- Case -------------------- *)

let parse_case parse_exp =
  option () (string "|" *> return ())
  *>
  let* pat = parse_pattern <* ws <* string "->" in
  let* exp = ws *> parse_exp in
  return { left = pat; right = exp }
;;

(* -------------------- Expression -------------------- *)

let parse_exp_ident = parse_ident >>| fun ident -> Exp_ident ident
let parse_exp_constant = parse_constant >>| fun const -> Exp_constant const

let parse_exp_tuple parse_exp =
  parse_tuple parse_exp (fun (fst_exp, snd_exp, exp_list) ->
    Exp_tuple (fst_exp, snd_exp, exp_list))
;;

let parse_exp_construct parse_exp =
  parse_construct
    parse_exp
    (fun (tag, exp_opt) -> Exp_construct (tag, exp_opt))
    (fun (fst_exp, snd_exp, exp_list) -> Exp_tuple (fst_exp, snd_exp, exp_list))
;;

let parse_exp_construct_list parse_exp =
  parse_construct_list
    parse_exp
    (fun (tag, exp_opt) -> Exp_construct (tag, exp_opt))
    (fun (fst_exp, snd_exp, exp_list) -> Exp_tuple (fst_exp, snd_exp, exp_list))
;;

let parse_exp_constraint parse_exp =
  parse_constraint parse_exp (fun (exp, type') -> Exp_constraint (exp, type'))
;;

let parse_exp_let parse_exp =
  ws
  *> keyword "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* value_binding_list =
    ws *> parse_value_binding_list parse_exp <* ws <* keyword "in"
  in
  let* exp = ws *> parse_exp in
  return
    (Exp_let (rec_flag, List.hd_exn value_binding_list, safe_tl value_binding_list, exp))
;;

let parse_exp_fun parse_exp =
  ws
  *> keyword "fun"
  *>
  let* pat = parse_pattern in
  let* pat_list = ws *> many parse_pattern in
  choice
    [ (let* exp = parse_constraint_vb parse_exp "->" in
       match exp with
       | Exp_constraint (exp, type') ->
         return (Exp_fun (Pat_constraint (pat, type'), pat_list, exp))
       | _ -> return (Exp_fun (pat, pat_list, exp)))
    ; (let* exp = ws *> string "->" *> parse_exp in
       return (Exp_fun (pat, pat_list, exp)))
    ]
;;

let parse_exp_apply_fun parse_exp =
  parse_chain_left_associative
    parse_exp
    (return (fun exp1 exp2 -> Exp_apply (exp1, exp2)))
;;

let parse_exp_apply_un_op parse_exp =
  let is_not_space = function
    | '(' | '[' | '_' | '\'' | '\"' -> true
    | c -> Char.is_alphanum c
  in
  let string_un_op str =
    string str
    *>
    let* char_value = peek_char_fail in
    if is_not_space char_value
    then return str
    else fail (Printf.sprintf "There is no space after unary minus.")
  in
  ws
  *> (string_un_op "-" *> parse_exp
      >>| (fun exp -> Exp_apply (Exp_ident "~-", exp))
      <|> parse_exp)
;;

let parse_exp_apply_bin_op parse_exp =
  let parse_exp = parse_left_bin_op parse_exp mul_div in
  let parse_exp = parse_left_bin_op parse_exp add_sub in
  let parse_exp = parse_left_bin_op parse_exp cmp in
  let parse_exp = parse_right_bin_op parse_exp and_ in
  parse_right_bin_op parse_exp or_
;;

let parse_exp_apply ~with_un_op parse_exp =
  let parse_exp = parse_exp_apply_fun parse_exp in
  let parse_exp = if with_un_op then parse_exp_apply_un_op parse_exp else parse_exp in
  parse_exp_apply_bin_op parse_exp
;;

let parse_exp_function parse_exp =
  ws
  *> keyword "function"
  *>
  let* case_list = ws *> sep_by1 (ws *> string "|" *> ws) (parse_case parse_exp) in
  return (Exp_function (List.hd_exn case_list, safe_tl case_list))
;;

let parse_exp_match parse_exp =
  let* exp = ws *> keyword "match" *> ws *> parse_exp <* ws <* keyword "with" in
  let* case_list = ws *> sep_by1 (ws *> string "|" *> ws) (parse_case parse_exp) in
  return (Exp_match (exp, List.hd_exn case_list, safe_tl case_list))
;;

let parse_exp_ifthenelse parse_exp =
  let* if_exp = ws *> keyword "if" *> parse_exp in
  let* then_exp = ws *> keyword "then" *> parse_exp in
  let* else_exp =
    option None (ws *> keyword "else" >>| Option.some)
    >>= function
    | None -> return None
    | Some _ -> parse_exp >>| Option.some
  in
  return (Exp_ifthenelse (if_exp, then_exp, else_exp))
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
        ; parse_exp_function parse_full_exp
        ; parse_exp_match parse_full_exp
        ; parse_exp_ifthenelse parse_full_exp
        ; parse_exp_construct parse_full_exp
        ; parse_exp_constraint parse_full_exp
        ]
    in
    let parse_exp = parse_exp_construct parse_exp <|> parse_exp in
    let parse_exp = parse_exp_apply ~with_un_op:true parse_exp <|> parse_exp in
    let parse_exp = parse_exp_construct parse_exp <|> parse_exp in
    let parse_exp = parse_exp_apply ~with_un_op:false parse_exp <|> parse_exp in
    let parse_exp = parse_exp_sequence parse_exp <|> parse_exp in
    let parse_exp = parse_exp_construct_list parse_exp <|> parse_exp in
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
       (Struct_value (rec_flag, List.hd_exn value_binding_list, safe_tl value_binding_list))
       (ws *> keyword "in" *> parse_expression
        >>| fun exp ->
        Struct_eval
          (Exp_let
             (rec_flag, List.hd_exn value_binding_list, safe_tl value_binding_list, exp))
       )
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
