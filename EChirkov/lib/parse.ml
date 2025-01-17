(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base

let is_ws = function
  | ' ' -> true
  | '\n' -> true
  | '\t' -> true
  | _ -> false
;;

let ws = skip_while is_ws

let safe_tl = function
  | [] -> []
  | _ :: tail -> tail
;;

let token str = ws *> string str (* TODO *)

let parse_rec_flag = option Nonrecursive (token "rec" *> return Recursive)

let parse_expression = (*TODO*)
let parse_struct_value =
  token "let"
  *>
  let* rec_flag = parse_rec_flag in
  let* value_binding_list = parse_value_binding_list parse_expression in
  option
    (SValue (rec_flag, List.hd_exn value_binding_list, safe_tl value_binding_list))
    (token "in" *> parse_expression
     >>| fun exp ->
     SEval
       (ELet (rec_flag, List.hd_exn value_binding_list, safe_tl value_binding_list, exp))
    )
;;

let parse_expression =
  ws
  *> fix (fun exp -> (* TODO *) )
;;

let parse_structure =
  ws
  *>
  let parse_structure_item =
    parse_struct_value <|> (parse_expression >>| fun exp -> SEval exp)
  in
  let semicolons = many (token ";;") in
  sep_by semicolons parse_structure_item <* semicolons <* ws
;;

let parse str = parse_string ~consume:All (ws *> parse_structure <* ws) str
