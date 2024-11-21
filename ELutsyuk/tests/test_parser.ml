(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open MiniML.Ast
open MiniML.Parser

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error _ -> print_endline "Syntax error"
;;

let%expect_test "parse_normal_id" =
  pp Format.pp_print_string parse_id "id";
  [%expect {| id |}]
;;

let%expect_test "parse_keyword_id" =
  pp Format.pp_print_string parse_id "while";
  [%expect {| Syntax error |}]
;;

let%expect_test "parse_num_with_plus" =
  pp pp_literal parse_int "+2024";
  [%expect {| (Int 2024) |}]
;;

let%expect_test "parse_num_with_minus" =
  pp pp_literal parse_int "-2024";
  [%expect {| (Int -2024) |}]
;;

let%expect_test "parse_num_without_sign_before" =
  pp pp_literal parse_int "2024\n";
  [%expect {| (Int 2024) |}]
;;

let%expect_test "parse_two_signs_before_number" =
  pp pp_literal parse_int "+-2024";
  [%expect {| Syntax error |}]
;;

let%expect_test "parse_string_with_//" =
  pp pp_literal parse_str "{|str//|}";
  [%expect {| (Str "str//") |}]
;;

let%expect_test "parse_wrong_string" =
  pp pp_literal parse_str "str//";
  [%expect {| Syntax error |}]
;;

let%expect_test "parse_empty_string" =
  pp pp_literal parse_str "{||}";
  [%expect {| (Str "") |}]
;;

let%expect_test "parse_one_space" =
  pp pp_literal parse_str "{| |}";
  [%expect {| (Str " ") |}]
;;

let%expect_test "parse_true" =
  pp pp_literal parse_bool "true";
  [%expect {| (Bool true) |}]
;;

let%expect_test "parse_false" =
  pp pp_literal parse_bool "false";
  [%expect {| (Bool false) |}]
;;

let%expect_test "parse_wrong_bool_with_char_after" =
  pp pp_literal parse_bool "truee";
  [%expect {| Syntax error |}]
;;

let%expect_test "parse_\n_after_bool" =
  pp pp_literal parse_bool "true\n";
  [%expect {| (Bool true) |}]
;;
