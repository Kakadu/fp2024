(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast
open PrsAuxilary
open Expressions

let prs_structure =
  let p_struct_binding =
    let* _ = token "let" in
    let* rec_state = choice [ string "rec " *> return Rec; return NonRec ] in
    let* binding = prs_let_binding prs_expr in
    let+ bindings_list = many (token "and" *> prs_let_binding prs_expr) in
    Value (rec_state, binding, bindings_list)
  in
  let p_struct_eval =
    let+ eval = prs_expr in
    Eval eval
  in
  p_struct_binding <|> p_struct_eval
;;

let prs_program = sep_by (many (token ";;")) prs_structure <* many (token ";;") <* skip_ws
let parse str = parse_string ~consume:All prs_program str
