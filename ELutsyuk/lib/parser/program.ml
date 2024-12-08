(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open Auxiliaries
open Ast
open Expressions

let parse_program : program t =
  let parse_eval =
    let* eval = parse_expr in
    return @@ Evaluation eval
  in
  let parse_binding =
    let* is_rec, pat, expr = parse_let_binding parse_expr in
    return @@ Binding { is_rec; pat; expr }
  in
  let meow = parse_binding <|> parse_eval in
  sep_by (token ";;") meow
;;
