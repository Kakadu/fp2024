(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Inferencer
open MiniML.Ast

let test_inferencer s =
  match parse s with
  | Ok s ->
    (* print_endline (show_program s); *)
    (match inference s with
     | Ok env -> MiniML.Inferencer.print_env env
     | Error e -> Format.printf "Type inference error: %a" MiniML.Inferencer.pp_error e)
  | Error _ -> print_endline "Parsing error"
;;

let%expect_test "inference factorial function" =
  test_inferencer "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1)";
  [%expect {|val factorial : (int -> int)|}]
;;
