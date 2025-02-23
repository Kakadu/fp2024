(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base

(* basic *)

let is_ws = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let ws = take_while is_ws

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let p_sign = option '+' (char '-' <|> char '+') 

let token str = ws *> string str

let parens s = token "(" *> s <* token ")"

let newline =
  skip_while (function
    | ' ' | '\t' -> true
    | _ -> false)
  *> char '\n'
;;

let newlines = skip_many1 newline

let p_digits = take_while1 is_digit 

(* patterns *)

let p_pattern = return PAny

(* exprs *)

let p_string = token "\"" *> take_till (Char.equal '"') <* "\"" >>| fun s -> CString s

let p_integer = 
  lift2 (fun s n -> CInt (Int.of_string s ^ n))
  p_sign
  p_digits

let p_boolean = 
  let t = token "true" *> return (CBool true) in
  let f = token "false" *> return (CBool false) in
choice [ t; f ]

let p_unit = token "()" *> return CUnit 

let p_unop = choice [ token "-" *> return Neg; token "+" *> return Pos ]

let p_expression = fix @@ fun e ->
  let p_e = parens e <|> p_e in (* ( expr ) *)
  let p_e = p_string <|> p_e in (* "asd" *)
  let p_e = p_integer <|> p_e in (* 234 *)
  let p_e = p_boolean <|> p_e in (* true *) 
  let p_e = p_unit <|> p_e in (* () *)
  let p_e = 

let p_structure_eval = p_expression

let p_rec_flag =
  choice [ take_while1 is_ws *> token "rec" *> return Recursive; return Nonrecursive ]
;;

let p_binding = lift2 (fun p e -> p, e) p_pattern p_expression

(* struct, top level *)

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

let p_program = sep_by newlines p_structure_item

(* actuall parser function *)

let parse = parse_string ~consume:All p_program
