(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base

(* basic stuff *)

let is_ws = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let ws = take_while is_ws

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let token str = ws *> string str

let newline =
  skip_while (function
    | ' ' | '\t' -> true
    | _ -> false)
  *> char '\n'
;;

let newlines = skip_many1 newline

(* more complex stuff *)

(* exprs *)
let p_pattern = return PAny
let p_expression = return (EConst (CInt 23))
let p_structure_eval = p_expression

let p_rec_flag =
  choice [ take_while1 is_ws *> token "rec" *> return Recursive; return Nonrecursive ]
;;

let p_binding = lift2 (fun p e -> p, e) p_pattern p_expression

(* struct, top level stuff *)

let p_structure_value =
  lift3
    (fun rf b bl -> return (SValue (rf, b, bl)))
    (token "let" *> p_rec_flag)
    p_binding
    (many (token "and" *> p_binding))
;;

let p_structure_item =
  p_structure_value <|> (p_structure_eval >>| fun e -> return (SEval e))
;;

let p_structure = sep_by newlines p_structure_item

(* actuall parser function *)

let parse = parse_string ~consume:All p_structure
