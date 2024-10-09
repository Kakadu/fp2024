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
  [%expect {| (IntLiteral 4611686018427387903) |}];
  Format.printf "%s" (string_of_literal_parse_result (parse boolean "true"));
  [%expect {| (BoolLiteral true) |}];
  Format.printf "%s" (string_of_literal_parse_result (parse boolean "false"));
  [%expect {| (BoolLiteral false) |}];
  Format.printf "%s" (string_of_literal_parse_result (parse boolean "faalse"));
  [%expect {| Parse process failed |}];
  Format.printf "%s" (string_of_literal_parse_result (parse boolean "trua"));
  [%expect {| Parse process failed |}]
;;

(* Test for expressin parsers *)
let%expect_test _ =
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr {|  123|}));
  [%expect {| (Const (IntLiteral 123)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr {|    true|}));
  [%expect {| (Const (BoolLiteral true)) |}];
  Format.printf "%s" (string_of_expression_parse_result (parse const_expr "trua"));
  [%expect {| Parse process failed |}]
;;
