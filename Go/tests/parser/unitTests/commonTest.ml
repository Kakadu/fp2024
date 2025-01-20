(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Pprinter.Printer
open Pp

(********** ident **********)

let%expect_test "ident with only letters" =
  pp print_ident parse_ident {|myIdent|};
  [%expect {| myIdent |}]
;;

let%expect_test "ident with first capital letter and underscore" =
  pp print_ident parse_ident {|My_ident|};
  [%expect {| My_ident |}]
;;

let%expect_test "blank ident" =
  pp print_ident parse_ident {|_|};
  [%expect {| _ |}]
;;

let%expect_test "ident with numbers" =
  pp print_ident parse_ident {|a1b2c3|};
  [%expect {| a1b2c3 |}]
;;

let%expect_test "ident with first char that is digit" =
  pp print_ident parse_ident {|1abc|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword break" =
  pp print_ident parse_ident {|break|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword func" =
  pp print_ident parse_ident {|func|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword defer" =
  pp print_ident parse_ident {|defer|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword go" =
  pp print_ident parse_ident {|go|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword chan" =
  pp print_ident parse_ident {|chan|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword if" =
  pp print_ident parse_ident {|if|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword else" =
  pp print_ident parse_ident {|else|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword continue" =
  pp print_ident parse_ident {|continue|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword for" =
  pp print_ident parse_ident {|for|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword return" =
  pp print_ident parse_ident {|return|};
  [%expect {| : syntax error |}]
;;

let%expect_test "ident keyword var" =
  pp print_ident parse_ident {|var|};
  [%expect {| : syntax error |}]
;;

(********** type **********)

let%expect_test "incorrect type" =
  pp print_type parse_type {|blablablablabla|};
  [%expect {| : syntax error |}]
;;

let%expect_test "type int" =
  pp print_type parse_type {|int|};
  [%expect {| int |}]
;;

let%expect_test "type bool" =
  pp print_type parse_type {|bool|};
  [%expect {| bool |}]
;;

let%expect_test "type string" =
  pp print_type parse_type {|string|};
  [%expect {| string |}]
;;

let%expect_test "type array of arrays" =
  pp print_type parse_type {|[4][0]string|};
  [%expect {| [4][0]string |}]
;;

let%expect_test "type array of functions" =
  pp print_type parse_type {|[4]func()|};
  [%expect {| [4]func() |}]
;;

let%expect_test "type simple func" =
  pp print_type parse_type {|func()|};
  [%expect {| func() |}]
;;

let%expect_test "type simple func with brackets" =
  pp print_type parse_type {|func()()|};
  [%expect {| func() |}]
;;

let%expect_test "type simple func with brackets and ws" =
  pp print_type parse_type {|func()  /* some comment */  ()|};
  [%expect {| func() |}]
;;

let%expect_test "type func with one arg and without returns" =
  pp print_type parse_type {|func(int)|};
  [%expect {| func(int) |}]
;;

let%expect_test "type func with mult args and without returns" =
  pp print_type parse_type {|func(int, string, bool, [4]int)|};
  [%expect {|
    func(int, string, bool, [4]int) |}]
;;

let%expect_test "type func with one return" =
  pp print_type parse_type {|func() int|};
  [%expect {| func() int |}]
;;

let%expect_test "type func with multiple returns" =
  pp print_type parse_type {|func() (int, string)|};
  [%expect {| func() (int, string) |}]
;;

let%expect_test "type func that gets func and returns func" =
  pp print_type parse_type {|func(func(int) string) func([4][5]int)|};
  [%expect {|
    func(func(int) string) func([4][5]int) |}]
;;

let%expect_test "type func that returns func that returns func..." =
  pp print_type parse_type {|func() func() func() func() func() func()|};
  [%expect {|
    func() func() func() func() func() func() |}]
;;

let%expect_test "type bidirectional chanel" =
  pp print_type parse_type {|chan int|};
  [%expect {|
    chan int |}]
;;

let%expect_test "type with parens" =
  pp print_type parse_type {|[3]([2]((func())))|};
  [%expect {|
    [3][2]func() |}]
;;
