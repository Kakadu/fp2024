(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Parser
open Miniml.Parser_utility
open Miniml.Printer

(* Test for literal parsers *)
let%expect_test _ =
  Format.printf "%s" (string_of_literal_parse_result (parse integer "123"));
  [%expect {| (IntLiteral 123) |}];
  Format.printf
    "%s"
    (string_of_literal_parse_result (parse integer (string_of_int Int.max_int)));
  [%expect {| (IntLiteral 4611686018427387903) |}]
;;
