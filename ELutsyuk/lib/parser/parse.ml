(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Aux
open Expr

let pprogram =
  let p_binding =
    let* _ = token "let" in
    let* is_rec, pat, expr = plet_binding pexpr in
    return @@ Binding { is_rec; pat; expr }
  in
  let p_eval =
    let* eval = pexpr in
    return @@ EvalExpr eval
  in
  sep_by (token ";;") (p_binding <|> p_eval) <* token ";;"
;;

let parse str = parse_string ~consume:All pprogram str

let parse_to_string input =
  match parse input with
  | Ok program -> show_program program
  | Error err -> err
;;
