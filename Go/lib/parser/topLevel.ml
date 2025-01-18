(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Angstrom
open Common
open Expr
open Stmt

let parse_func_decl : func_decl t =
  let* () = string "func" *> ws in
  let* func_name = parse_ident <* ws_line in
  let* args_returns_and_body = parse_func_args_returns_and_body parse_block in
  return (func_name, args_returns_and_body)
;;

let parse_top_decl =
  parse_long_var_decl parse_block
  >>| (fun decl -> Decl_var decl)
  <|> (parse_func_decl >>| fun decl -> Decl_func decl)
;;

let parse_file : file t = ws *> sep_by parse_stmt_sep parse_top_decl <* ws
