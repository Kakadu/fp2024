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
let token str = ws *> string str

let skip_parens parse =
  token "(" *> parse <* (token ")" <|> fail "There is no closing bracket.")
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

let parse_type_var =
  token "'"
  *>
  let* fst_char =
    satisfy (function
      | 'a' .. 'z' -> true
      | _ -> false)
    >>| String.of_char
  in
  let* is_valid_snd_char =
    peek_char
    >>| function
    | Some snd_char ->
      (match snd_char with
       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
       | char' when is_separator char' -> true
       | _ -> false)
    | _ -> true
  in
  let* rest_str =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  in
  let type_var = fst_char ^ rest_str in
  if is_valid_snd_char && not (is_keyword type_var)
  then return (Type_var ("'" ^ type_var))
  else fail (Printf.sprintf "Impossible type name: %S." type_var)
;;

let parse_base_type =
  choice
    [ keyword "unit" *> return Type_unit
    ; keyword "int" *> return Type_int
    ; keyword "char" *> return Type_char
    ; keyword "string" *> return Type_string
    ; keyword "bool" *> return Type_bool
    ; parse_type_var
    ]
;;

let parse_list_or_option_type parse_type =
  let f acc_ty = function
    | "list" -> Type_list acc_ty
    | _ -> Type_option acc_ty
  in
  let chain_left_associative =
    let rec go acc_ty =
      (let* ty = keyword "list" <|> keyword "option" in
       go (f acc_ty ty))
      <|> return acc_ty
    in
    let* fst_ty = parse_type in
    go fst_ty
  in
  chain_left_associative
;;

let parse_tuple_type parse_type =
  let* fst_type = parse_type in
  let* snd_type = token "*" *> parse_type in
  let* type_list = many (token "*" *> parse_type) in
  return (Type_tuple (fst_type, snd_type, type_list))
;;

let rec parse_arrow_type parse_type =
  let* type1 = parse_type in
  let* type2 = token "->" *> (parse_arrow_type parse_type <|> parse_type) in
  return (Type_arrow (type1, type2))
;;

let parse_core_type =
  ws
  *> fix (fun parse_full_type ->
    let parse_type = parse_base_type <|> skip_parens parse_full_type in
    let parse_type = parse_list_or_option_type parse_type <|> parse_type in
    let parse_type = parse_tuple_type parse_type <|> parse_type in
    parse_arrow_type parse_type <|> parse_type)
;;

(* ==================== Pattern & Expression ==================== *)

let parse_construct_base_keyword =
  choice [ keyword "true"; keyword "false"; keyword "None"; keyword "()" ]
;;

let parse_construct_keyword_some parse =
  let* tag = keyword "Some" in
  let* opt = parse >>| Option.some in
  return (tag, opt)
;;

let parse_constraint parse =
  let* elem = token "(" *> parse in
  let* type' = token ":" *> parse_core_type <* token ")" in
  return (elem, type')
;;

let parse_tuple parse tuple =
  let* fst = parse in
  let* snd = token "," *> parse in
  let* tail = many (token "," *> parse) in
  return (tuple (fst, snd, tail))
;;

let parse_construct_list_1 parse construct func =
  token "[" *> sep_by (token ";") parse
  <* token "]"
  >>| List.fold_right ~init:(construct ("[]", None)) ~f:func
;;

let parse_construct_list_2 parse construct tuple =
  parse_chain_right_associative
    parse
    (token "::" *> return (fun acc elem -> construct ("::", Some (tuple (acc, elem, [])))))
;;

(* -------------------- Pattern -------------------- *)

let parse_pat_any = keyword "_" *> return Pat_any
let parse_pat_var = parse_ident >>| fun var -> Pat_var var
let parse_pat_constant = parse_constant >>| fun const -> Pat_constant const

let parse_pat_construct_base_keyword =
  parse_construct_base_keyword >>| fun tag -> Pat_construct (tag, None)
;;

let parse_base_pat =
  choice
    [ parse_pat_any; parse_pat_var; parse_pat_constant; parse_pat_construct_base_keyword ]
;;

let parse_pat_construct_keyword_some parse_pat =
  parse_construct_keyword_some (parse_base_pat <|> skip_parens parse_pat)
  >>| fun (tag, pat_opt) -> Pat_construct (tag, pat_opt)
;;

let parse_pat_constraint parse_pat =
  parse_constraint parse_pat >>| fun (pat, type') -> Pat_constraint (pat, type')
;;

let parse_pat_tuple parse_pat =
  parse_tuple parse_pat (fun (fst_pat, snd_pat, pat_list) ->
    Pat_tuple (fst_pat, snd_pat, pat_list))
;;

let parse_pat_construct_list_1 parse_pat =
  parse_construct_list_1
    parse_pat
    (fun (tag, pat_opt) -> Pat_construct (tag, pat_opt))
    (fun pat acc_pat -> Pat_construct ("::", Some (Pat_tuple (pat, acc_pat, []))))
;;

let parse_pat_construct_list_2 parse_pat =
  parse_construct_list_2
    parse_pat
    (fun (tag, pat_opt) -> Pat_construct (tag, pat_opt))
    (fun (fst_pat, snd_pat, pat_list) -> Pat_tuple (fst_pat, snd_pat, pat_list))
;;

let parse_pattern =
  ws
  *> fix (fun parse_full_pat ->
    let parse_pat =
      choice
        [ parse_base_pat
        ; parse_pat_construct_keyword_some parse_full_pat
        ; parse_pat_constraint parse_full_pat
        ; skip_parens parse_full_pat
        ]
    in
    let parse_pat = parse_pat_construct_list_1 parse_pat <|> parse_pat in
    let parse_pat = parse_pat_construct_list_2 parse_pat <|> parse_pat in
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
  choice (List.map ~f:(fun opr -> token opr *> return (Exp_ident opr)) op_list)
;;

let mul_div = parse_operator [ "*"; "/" ]
let add_sub = parse_operator [ "+"; "-" ]
let cmp = parse_operator [ ">="; "<="; "<>"; "="; ">"; "<" ]
let and_ = parse_operator [ "&&" ]
let or_ = parse_operator [ "||" ]

(* -------------------- Value_binding -------------------- *)

let parse_constraint_vb parse_exp opr =
  let* type' = token ":" *> parse_core_type in
  let* exp = token opr *> parse_exp in
  return (Exp_constraint (exp, type'))
;;

let parse_fun_binding parse_exp =
  let* pat_var = parse_pat_var in
  let* pat_list = many1 parse_pattern in
  choice
    [ (let* exp = parse_constraint_vb parse_exp "=" in
       match exp with
       | Exp_constraint (exp, type') ->
         return
           { pat = Pat_constraint (pat_var, type')
           ; exp = Exp_fun (List.hd_exn pat_list, safe_tl pat_list, exp)
           }
       | _ ->
         return
           { pat = pat_var; exp = Exp_fun (List.hd_exn pat_list, safe_tl pat_list, exp) })
    ; (let* exp = token "=" *> parse_exp in
       return
         { pat = pat_var; exp = Exp_fun (List.hd_exn pat_list, safe_tl pat_list, exp) })
    ]
;;

let parse_simple_binding parse_exp =
  let* pat = parse_pattern in
  choice
    [ (let* exp = parse_constraint_vb parse_exp "=" in
       match exp with
       | Exp_constraint (exp, type') -> return { pat = Pat_constraint (pat, type'); exp }
       | _ -> return { pat; exp })
    ; (let* exp = token "=" *> parse_exp in
       return { pat; exp })
    ]
;;

let parse_value_binding_list parse_exp =
  sep_by1
    (keyword "and")
    (ws *> (parse_fun_binding parse_exp <|> parse_simple_binding parse_exp))
;;

(* -------------------- Case -------------------- *)

let parse_case parse_exp =
  ws
  *> option () (token "|" *> return ())
  *>
  let* pat = parse_pattern in
  let* exp = token "->" *> parse_exp in
  return { left = pat; right = exp }
;;

(* -------------------- Expression -------------------- *)

let parse_exp_ident = parse_ident >>| fun id -> Exp_ident id
let parse_exp_constant = parse_constant >>| fun const -> Exp_constant const

let parse_exp_construct_base_keyword =
  parse_construct_base_keyword >>| fun tag -> Exp_construct (tag, None)
;;

let parse_base_exp =
  choice [ parse_exp_ident; parse_exp_constant; parse_exp_construct_base_keyword ]
;;

let parse_exp_sequence parse_exp =
  parse_chain_left_associative
    parse_exp
    (token ";" *> return (fun exp1 exp2 -> Exp_sequence (exp1, exp2)))
;;

let parse_exp_construct_keyword_some parse_exp =
  parse_construct_keyword_some (parse_base_exp <|> skip_parens parse_exp)
  >>| fun (tag, exp_opt) -> Exp_construct (tag, exp_opt)
;;

let parse_exp_constraint parse_exp =
  parse_constraint parse_exp >>| fun (exp, type') -> Exp_constraint (exp, type')
;;

let parse_exp_tuple parse_exp =
  parse_tuple parse_exp (fun (fst_exp, snd_exp, exp_list) ->
    Exp_tuple (fst_exp, snd_exp, exp_list))
;;

let parse_exp_construct_list_1 parse_exp =
  let parse_exp_sequence =
    skip_parens (parse_exp_sequence parse_exp) >>| fun exp -> true, exp
  in
  let parse_exp_list = parse_exp >>| fun exp -> false, exp in
  parse_construct_list_1
    (parse_exp_sequence <|> parse_exp_list)
    (fun (tag, exp_opt) -> Exp_construct (tag, exp_opt))
    (fun opt_exp acc_exp ->
      let rec fix_exp_sequence opt_exp acc_exp =
        match opt_exp with
        | false, Exp_sequence (exp1, (Exp_sequence _ as exp2)) ->
          fix_exp_sequence (false, exp1) (fix_exp_sequence (true, exp2) acc_exp)
        | false, Exp_sequence (exp1, exp2) ->
          fix_exp_sequence (false, exp1) (fix_exp_sequence (false, exp2) acc_exp)
        | _, exp -> Exp_construct ("::", Some (Exp_tuple (exp, acc_exp, [])))
      in
      fix_exp_sequence opt_exp acc_exp)
;;

let parse_exp_construct_list_2 parse_exp =
  parse_construct_list_2
    parse_exp
    (fun (tag, exp_opt) -> Exp_construct (tag, exp_opt))
    (fun (fst_exp, snd_exp, exp_list) -> Exp_tuple (fst_exp, snd_exp, exp_list))
;;

let parse_exp_let parse_exp =
  keyword "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* value_binding_list = parse_value_binding_list parse_exp <* keyword "in" in
  let* exp = parse_exp in
  return
    (Exp_let (rec_flag, List.hd_exn value_binding_list, safe_tl value_binding_list, exp))
;;

let parse_exp_fun parse_exp =
  keyword "fun"
  *>
  let* pat = parse_pattern in
  let* pat_list = many parse_pattern in
  choice
    [ (let* exp = parse_constraint_vb parse_exp "->" in
       match exp with
       | Exp_constraint (exp, type') ->
         return (Exp_fun (Pat_constraint (pat, type'), pat_list, exp))
       | _ -> return (Exp_fun (pat, pat_list, exp)))
    ; (let* exp = token "->" *> parse_exp in
       return (Exp_fun (pat, pat_list, exp)))
    ]
;;

let parse_exp_function parse_exp =
  keyword "function"
  *>
  let* case_list = sep_by1 (token "|") (parse_case parse_exp) in
  return (Exp_function (List.hd_exn case_list, safe_tl case_list))
;;

let parse_exp_match parse_exp =
  let* exp = keyword "match" *> parse_exp <* keyword "with" in
  let* case_list = sep_by1 (token "|") (parse_case parse_exp) in
  return (Exp_match (exp, List.hd_exn case_list, safe_tl case_list))
;;

let parse_exp_ifthenelse parse_exp =
  let* if_exp = keyword "if" *> parse_exp in
  let* then_exp = keyword "then" *> parse_exp in
  let* else_exp =
    option None (keyword "else" >>| Option.some)
    >>= function
    | None -> return None
    | Some _ -> parse_exp >>| Option.some
  in
  return (Exp_ifthenelse (if_exp, then_exp, else_exp))
;;

let parse_top_exp parse_exp =
  choice
    [ parse_exp_let parse_exp
    ; parse_exp_fun parse_exp
    ; parse_exp_function parse_exp
    ; parse_exp_match parse_exp
    ; parse_exp_ifthenelse parse_exp
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
    token str
    *>
    let* char_value = peek_char_fail in
    if is_not_space char_value
    then return str
    else fail (Printf.sprintf "There is no space after unary minus.")
  in
  string_un_op "-" *> parse_exp
  >>| (fun exp -> Exp_apply (Exp_ident "~-", exp))
  <|> parse_exp
;;

let parse_exp_apply_bin_op parse_exp =
  let parse_exp = parse_left_bin_op parse_exp mul_div in
  let parse_exp = parse_left_bin_op parse_exp add_sub in
  let parse_exp = parse_left_bin_op parse_exp cmp in
  let parse_exp = parse_right_bin_op parse_exp and_ in
  parse_right_bin_op parse_exp or_
;;

let parse_exp_apply parse_exp =
  let parse_exp = parse_exp_apply_fun parse_exp in
  let parse_exp = parse_exp_apply_un_op parse_exp in
  parse_exp_apply_bin_op parse_exp
;;

let parse_expression =
  ws
  *> fix (fun parse_full_exp ->
    let parse_exp =
      choice
        [ parse_base_exp
        ; parse_exp_construct_keyword_some parse_full_exp
        ; parse_exp_constraint parse_full_exp
        ; parse_exp_construct_list_1 parse_full_exp
        ; parse_top_exp parse_full_exp
        ; skip_parens parse_full_exp
        ]
    in
    let parse_exp = parse_exp_apply parse_exp <|> parse_exp in
    let parse_exp = parse_exp_construct_list_2 parse_exp <|> parse_exp in
    let parse_exp = parse_exp_tuple parse_exp <|> parse_exp in
    let parse_exp = parse_exp_sequence parse_exp <|> parse_exp in
    parse_top_exp parse_full_exp <|> parse_exp)
;;

(* ==================== Structure ==================== *)

let parse_struct_value =
  keyword "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* value_binding_list = parse_value_binding_list parse_expression in
  option
    (Struct_value (rec_flag, List.hd_exn value_binding_list, safe_tl value_binding_list))
    (keyword "in" *> parse_expression
     >>| fun exp ->
     Struct_eval
       (Exp_let (rec_flag, List.hd_exn value_binding_list, safe_tl value_binding_list, exp))
    )
;;

let parse_structure =
  ws
  *>
  let parse_structure_item =
    parse_struct_value <|> (parse_expression >>| fun exp -> Struct_eval exp)
  in
  let semicolons = many (token ";;") in
  sep_by semicolons parse_structure_item <* semicolons <* ws
;;

(* ==================== Execute ==================== *)

let parse = parse_string ~consume:All parse_structure
