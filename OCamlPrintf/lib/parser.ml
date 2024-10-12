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

let parse_ident =
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
  >>= fun ident ->
  match ident with
  | "let" | "fun" | "function" | "if" | "then" | "else" -> fail ident
  | _ -> return ident
;;

let skip_parens exp = ws *> char '(' *> ws *> exp <* ws <* char ')'

(* ==================== Rec_flag ==================== *)

let parse_rec_flag = ws *> option Nonrecursive (string "rec" *> return Recursive)

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

let parse_constant = choice [ parse_const_int; parse_const_char; parse_const_string ]

(* ==================== Pattern ==================== *)

let parse_pat_any = char '_' *> return Pat_any
let parse_pat_var = parse_ident >>| fun var -> Pat_var var
let parse_pat_const = parse_constant >>| fun const -> Pat_constant const
let parse_pat_tuple parse_pat = lift (fun pat_list -> Pat_tuple pat_list) (many parse_pat)

(** [TODO] add "parse_pat" *)
let parse_pat_construct = lift (fun ident -> Pat_construct (ident, None)) parse_ident

let parse_pattern =
  fix (fun parse_pat ->
    ws
    *> choice
         [ parse_pat_any
         ; parse_pat_var
         ; parse_pat_const
         (* ; parse_pat_tuple parse_pat *)
         (* ; parse_pat_construct *)
         ])
;;

(* ==================== Expression ==================== *)

let parse_bin_op =
  take_while1 (function
    | '+' | '-' | '*' | '/' | '=' | '<' | '>' -> true
    | _ -> false)
  >>| fun bin_op -> Exp_ident bin_op
;;

let parse_exp_ident = parse_ident >>| fun ident -> Exp_ident ident
let parse_exp_const = parse_constant >>| fun const -> Exp_constant const

let parse_exp_fun parse_exp =
  lift2
    (fun pat_list exp -> Exp_fun (pat_list, exp))
    (many parse_pattern)
    (skip_parens parse_exp)
    (* parse_exp *)
;;

let parse_exp_apply parse_exp =
  lift2
    (fun exp exp_list -> Exp_apply (exp, exp_list))
    (skip_parens parse_exp)
    (* parse_exp *)
    (skip_parens (many parse_exp))
    (* (many parse_exp) *)
;;

let parse_exp_tuple parse_exp = lift (fun exp_list -> Exp_tuple exp_list) (many parse_exp)

(** [TODO]: add "parse_exp" *)
let parse_exp_construct = lift (fun ident -> Exp_construct (ident, None)) parse_ident

(** [TODO] add "else" *)
let parse_exp_ifthen parse_exp =
  lift2
    (fun if_ then_ -> Exp_ifthenelse (if_, then_, None))
    (ws *> string "if" *> parse_exp)
    (ws *> string "then" *> parse_exp)
;;

let parse_expression =
  fix (fun parse_exp ->
    ws
    *> choice
         [ parse_bin_op
         ; parse_exp_ident
         ; parse_exp_const
         ; parse_exp_fun parse_exp
         ; parse_exp_apply parse_exp
         (* ; parse_exp_tuple parse_exp *)
         (* ; parse_exp_construct *)
         (* ; parse_exp_ifthen parse_exp *)
         ])
;;

(* ==================== Case ==================== *)

let parse_case = 
  lift2 (fun left right -> { left; right } )
  (parse_pattern)
  (ws *> string "->" *> parse_expression)

(* ==================== Value_binding ==================== *)

let parse_fun_binding =
  lift3
    (fun name args exp -> { pat = name; exp = Exp_fun (args, exp) })
    (ws *> parse_pat_var)
    (ws *> sep_by1 ws parse_pattern)
    (ws *> char '=' *> parse_expression)
;;

let parse_simple_binding =
  lift2
    (fun pat exp -> { pat; exp })
    parse_pattern
    (ws *> char '=' *> ws *> parse_expression)
;;

let parse_value_binding_list =
  sep_by1 (ws *> string "and") (parse_fun_binding <|> parse_simple_binding)
;;

(* ==================== Structure ==================== *)

let parse_struct_value =
  string "let"
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

let parse str =
  match parse_string ~consume:All parse_structure str with
  | Ok value -> value
  | Error message -> failwith message
;;
