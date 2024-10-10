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

let skip_many = many parse_comments *> skip_whitespaces
let skip_let = skip_many *> string "let"
let skip_equals = skip_many *> char '='

(* Rec_flag *)

let parse_rec_flag = skip_many *> option Nonrecursive (string "rec" *> return Recursive)

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
  skip_many *> choice [ parse_int; parse_const_char; parse_const_string ] <* skip_many
;;

(* Pattern *)

let parse_pat_any = char '_' *> return Pat_any

let parse_pat_ident =
  take_while1 (function
    | 'a' .. 'z' -> true
    | _ -> false)
  >>| fun ident -> Pat_var ident
;;

let parse_pat_const = parse_constant >>| fun const_value -> Pat_constant const_value

let parse_pattern =
  skip_many
  *> skip_let
  *> skip_many
  *> choice [ parse_pat_any; parse_pat_ident; parse_pat_const ]
  <* skip_many
;;

(* Expression *)

let parse_exp_ident =
  take_while1 (function
    | 'a' .. 'z' -> true
    | _ -> false)
  >>| fun ident -> Exp_ident ident
;;

let parse_pat_const = parse_constant >>| fun const_value -> Exp_constant const_value

let parse_expression =
  skip_many *> skip_equals *> skip_many *> choice [ parse_exp_ident; parse_pat_const ]
  <* skip_many
;;

let parse_binding =
  lift2 (fun ident const -> { pat = ident; exp = const }) parse_pattern parse_expression
;;

(* Run *)

let parse str = parse_string ~consume:All parse_binding str |> Result.ok
