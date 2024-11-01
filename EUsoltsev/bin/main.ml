(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib
open Ast
open Parser
open Printf

let parse_test input =
  match parse input with
  | Ok ast -> printf "%s\n" (show_program ast)
  | Error fail -> printf "Ошибка: %s\n" fail

let() = parse_test "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1);;"