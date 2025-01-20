(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base
open Pp
open Parse.Constants
open Pprint.Pprinter

let%expect_test "parse int as const int" =
  pp pprint_const puconst {|5|};
  [%expect {| 5 |}]
;;

let%expect_test "parse negative int as const int" =
  pp pprint_const psconst {|-5|};
  [%expect {| -5 |}]
;;

let%expect_test "parse int with letters should fail" =
  pp pprint_const puconst {|123a|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse char as const char" =
  pp pprint_const puconst {|'a'|};
  [%expect {| 'a' |}]
;;

let%expect_test "parse string as const string" =
  pp pprint_const puconst {|"abcde"|};
  [%expect {| "abcde" |}]
;;

let%expect_test "parse true as const bool" =
  pp pprint_const puconst {|true|};
  [%expect {| true |}]
;;

let%expect_test "parse false as const bool" =
  pp pprint_const puconst {|false|};
  [%expect {| false |}]
;;

let%expect_test "parse simple float as const float" =
  pp pprint_const puconst {|1.0|};
  [%expect {|
    1. |}]
;;

let%expect_test "parse simple negative float as const float" =
  pp pprint_const psconst {|-1.0|};
  [%expect {|
    -1. |}]
;;
