(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Interpreter
open MiniML.Ast

let test_interpreter s =
  match parse s with
  | Ok s ->
    (* print_endline (show_program s); *)
    (match interpret s with
     | Ok _ -> ()
     | Error e -> print_endline ("Interpretation error: " ^ pp_error e))
  | Error _ -> print_endline "Parsing error"
;;

let%expect_test "parse simple let" =
  test_interpreter "let () = print_int 23";
  [%expect {|23|}]
;;
