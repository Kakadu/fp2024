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

let is_digit = function '0' .. '9' -> true | _ -> false
;;

let token str = ws *> string str <* ws

let newline =
  skip_while (function
    | ' ' | '\t' -> true
    | _ -> false)
  *> char '\n'
;;

let newlines = skip_many1 newline

(* more complex stuff *)

let p_sign =
  peek_char
  >>= function
    | Some ('-' | '+') as s -> advance 1 >>| fun () -> Option.get s |> String.make 1
    | Some c when is_digit c -> return "+"  (* Default to '+' if no explicit sign *)
    | _ -> fail "Sign or digit expected"

let p_int =
  let* sign = p_sign in
  let* digits = take_while1 is_digit in
  return (EConst (CInt (int_of_string (sign ^ digits))))

(* exprs *)

let p_expr s = s
let p_structure_eval s = s

let p_rec_flag = 

(* struct, top level stuff *)

let p_structure_value s =
  let* _ = token "let" in
  let* rec_flag = p_rec_flag in 
  let* name = p_ident in 
  s
;;

let p_structure_item s =
  let structure_item_value = p_structure_value s in
  let structure_item_eval = p_structure_eval s in
  structure_item_value <|> (structure_item_eval >>= fun e -> return (SEval e))
;;

let p_structure = sep_by newlines p_structure_item;;

(* actuall parser function *)

let parse str = parse_string ~consume:All p_structure str;;
