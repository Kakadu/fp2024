(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Angstrom

(* Utils *)

let skip_whitespaces = skip_while Char.is_whitespace

let parse_comments =
  skip_whitespaces *> string "(*" *> many_till any_char (string "*)") *> return ()
;;

let ws = many parse_comments *> skip_whitespaces

let parse_ident =
  let parse_first =
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
  lift2 String.( ^ ) parse_first parse_rest >>= fun name -> return name
;;

let parens p = ws *> char '(' *> ws *> p <* ws *> char ')'

(* Rec_flag *)

let parse_rec_flag = ws *> option Nonrecursive (string "rec" *> return Recursive)

(* Constant *)

let parse_int =
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
  ws *> choice [ parse_int; parse_const_char; parse_const_string ] <* ws
;;

(* Pattern *)

let parse_pat_any = char '_' *> return Pat_any
let parse_pat_ident = parse_ident >>| fun const_value -> Pat_var const_value
let parse_pat_const = parse_constant >>| fun const_value -> Pat_constant const_value
let parse_pattern = ws *> choice [ parse_pat_any; parse_pat_ident; parse_pat_const ] <* ws

(* Expression *)

let parse_exp_ident = parse_ident >>| fun ident_value -> Exp_ident ident_value
let parse_exp_const = parse_constant >>| fun const_value -> Exp_constant const_value
let parse_expression = choice [ parse_exp_ident; parse_exp_const ] <* ws

let parse_value_binding_list =
  let parse_value_binding =
    let parse_fun_binding =
      lift3
        (fun name args exp -> { pat = name; exp = Exp_fun (args, exp) })
        (ws *> parse_pat_ident)
        (ws *> sep_by1 ws parse_pattern)
        (ws *> char '=' *> parse_expression)
    in
    let parse_simple_binding =
      lift2
        (fun pat exp -> { pat; exp })
        parse_pattern
        (ws *> char '=' *> ws *> parse_expression)
    in
    parse_fun_binding <|> parse_simple_binding
  in
  sep_by1 (ws *> string "and") parse_value_binding
;;

(* Structure *)

let parse_struct_value =
  string "let"
  *> lift2
       (fun rec_flag value_bindings -> Struct_value (rec_flag, value_bindings))
       parse_rec_flag
       parse_value_binding_list
;;

let parse_structure =
  let parse_structure_item =
    ws *> parse_struct_value <|> (parse_expression >>| fun e -> Struct_eval e)
  in
  let semicolons = many (ws *> string ";;") in
  sep_by semicolons parse_structure_item <* semicolons <* ws
;;

(* Run *)

let parse str = parse_string ~consume:All parse_structure str |> Result.ok
