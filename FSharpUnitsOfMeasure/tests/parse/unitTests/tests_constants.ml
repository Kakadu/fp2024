(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pp
open Parse.Constants
open Pprint.Pprinter

let run_s const = pp pprint_const psconst const
let run_u const = pp pprint_const puconst const
let _, _ = run_s, run_u

let%expect_test "parse int as const int" =
  run_u {|5|};
  [%expect {| 5 |}]
;;

let%expect_test "parse negative int as const int" =
  run_s {|-5|};
  [%expect {| -5 |}]
;;

let%expect_test "parse int with letters should fail" =
  run_u {|123a|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse char as const char" =
  run_u {|'a'|};
  [%expect {| 'a' |}]
;;

let%expect_test "parse string as const string" =
  run_u {|"abcde"|};
  [%expect {| "abcde" |}]
;;

let%expect_test "parse true as const bool" =
  run_u {|true|};
  [%expect {| true |}]
;;

let%expect_test "parse false as const bool" =
  run_u {|false|};
  [%expect {| false |}]
;;

let%expect_test "parse simple float as const float" =
  run_u {|1.0|};
  [%expect {|
    1. |}]
;;

let%expect_test "parse simple negative float as const float" =
  run_u {|-1.0|};
  [%expect {|
    : no more choices |}]
;;
