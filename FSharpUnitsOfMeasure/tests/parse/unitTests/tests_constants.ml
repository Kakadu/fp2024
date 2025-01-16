(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Common
open Parse.Constants

let%expect_test "parse int as const int" =
  pp pp_constant parse_const_u {|5|};
  [%expect {| (Const_int 5) |}]
;;

let%expect_test "parse negative int as const int" =
  pp pp_constant parse_const_s {|-5|};
  [%expect {| (Const_int -5) |}]
;;

let%expect_test "parse int with letters should fail" =
  pp pp_constant parse_const_u {|123a|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse char as const char" =
  pp pp_constant parse_const_u {|'a'|};
  [%expect {| (Const_char 'a') |}]
;;

let%expect_test "parse string as const string" =
  pp pp_constant parse_const_u {|"abcde"|};
  [%expect {| (Const_string "abcde") |}]
;;

let%expect_test "parse true as const bool" =
  pp pp_constant parse_const_u {|true|};
  [%expect {| (Const_bool true) |}]
;;

let%expect_test "parse false as const bool" =
  pp pp_constant parse_const_u {|false|};
  [%expect {| (Const_bool false) |}]
;;

let%expect_test "parse simple float as const float" =
  pp pp_constant parse_const_u {|1.0|};
  [%expect {| (Const_float 1.) |}]
;;

let%expect_test "parse simple negative float as const float" =
  pp pp_constant parse_const_s {|-1.0|};
  [%expect {| (Const_float -1.) |}]
;;
