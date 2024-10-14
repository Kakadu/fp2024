(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common
open Expr
open Stmt

let parse_func_decl =
  lift2
    (fun name args_returns_and_body -> name, args_returns_and_body)
    (string "func" *> ws *> parse_ident <* ws_line)
    (parse_anon_func parse_block)
;;

let parse_top_decl =
  parse_var_decl_top_level
  >>| (fun decl -> Decl_var decl)
  <|> (parse_func_decl >>| fun decl -> Decl_func decl)
;;

let parse_file : file t = many_sep ~sep:parse_stmt_sep ~parser:parse_top_decl
