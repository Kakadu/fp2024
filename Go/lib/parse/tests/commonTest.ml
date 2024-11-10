(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Pp
open Common

(********** ident **********)

let%expect_test "ident with only letters" =
  pp pp_ident parse_ident {|myIdent|};
  [%expect {| "myIdent" |}]
;;

let%expect_test "ident with uderscore" =
  pp pp_ident parse_ident {|my_ident|};
  [%expect {| "my_ident" |}]
;;

let%expect_test "blank ident" =
  pp pp_ident parse_ident {|_|};
  [%expect {| "_" |}]
;;

let%expect_test "ident with numbers" =
  pp pp_ident parse_ident {|a1b2c3|};
  [%expect {| "a1b2c3" |}]
;;

let%expect_test "ident with first char that is digit" =
  pp pp_ident parse_ident {|1abc|};
  [%expect {| : Invalid ident |}]
;;

(********** type **********)

let%expect_test "incorrect type" =
  pp pp_type' parse_type {|blablablablabla|};
  [%expect {| : no more choices |}]
;;

let%expect_test "type int" =
  pp pp_type' parse_type {|int|};
  [%expect {| Type_int |}]
;;

let%expect_test "type bool" =
  pp pp_type' parse_type {|bool|};
  [%expect {| Type_bool |}]
;;

let%expect_test "type string" =
  pp pp_type' parse_type {|string|};
  [%expect {| Type_string |}]
;;

let%expect_test "type array of arrays" =
  pp pp_type' parse_type {|[4][0]string|};
  [%expect {| (Type_array (4, (Type_array (0, Type_string)))) |}]
;;

let%expect_test "type array of functions" =
  pp pp_type' parse_type {|[4]func()|};
  [%expect {| (Type_array (4, (Type_func ([], [])))) |}]
;;

let%expect_test "type simple func" =
  pp pp_type' parse_type {|func()|};
  [%expect {| (Type_func ([], [])) |}]
;;

let%expect_test "type simple func with brackets" =
  pp pp_type' parse_type {|func()()|};
  [%expect {| (Type_func ([], [])) |}]
;;

let%expect_test "type simple func with brackets and ws" =
  pp pp_type' parse_type {|func()  /* some comment */  ()|};
  [%expect {| (Type_func ([], [])) |}]
;;

let%expect_test "type func with one arg and without returns" =
  pp pp_type' parse_type {|func(int)|};
  [%expect {| (Type_func ([Type_int], [])) |}]
;;

let%expect_test "type func with mult args and without returns" =
  pp pp_type' parse_type {|func(int, string, bool, [4]int)|};
  [%expect
    {|
    (Type_func ([Type_int; Type_string; Type_bool; (Type_array (4, Type_int))],
       [])) |}]
;;

let%expect_test "type func with one return" =
  pp pp_type' parse_type {|func() int|};
  [%expect {| (Type_func ([], [Type_int])) |}]
;;

let%expect_test "type func with multiple returns" =
  pp pp_type' parse_type {|func() (int, string)|};
  [%expect {| (Type_func ([], [Type_int; Type_string])) |}]
;;

let%expect_test "type func that gets func and returns func" =
  pp pp_type' parse_type {|func(func(int) string) func([4][5]int)|};
  [%expect
    {|
    (Type_func ([(Type_func ([Type_int], [Type_string]))],
       [(Type_func ([(Type_array (4, (Type_array (5, Type_int))))], []))])) |}]
;;

let%expect_test "type func that returns func that returns func..." =
  pp pp_type' parse_type {|func() func() func() func() func() func()|};
  [%expect
    {|
    (Type_func ([],
       [(Type_func ([],
           [(Type_func ([],
               [(Type_func ([], [(Type_func ([], [(Type_func ([], []))]))]))]))
             ]
           ))
         ]
       )) |}]
;;

let%expect_test "type bidirectional channel" =
  pp pp_type' parse_type {|chan int|};
  [%expect {|
    (Type_chan (Chan_bidirectional Type_int)) |}]
;;

let%expect_test "type receive-only channel" =
  pp pp_type' parse_type {|<- chan func()|};
  [%expect {|
    (Type_chan (Chan_receive (Type_func ([], [])))) |}]
;;

let%expect_test "type send-only channel" =
  pp pp_type' parse_type {|chan<- [0]string|};
  [%expect {|
    (Type_chan (Chan_send (Type_array (0, Type_string)))) |}]
;;

let%expect_test "type send-only channel of bidirectional channel" =
  pp pp_type' parse_type {|chan <- chan int|};
  [%expect {|
    (Type_chan (Chan_send (Type_chan (Chan_bidirectional Type_int)))) |}]
;;

let%expect_test "type with parens" =
  pp pp_type' parse_type {|[3]([2]((func())))|};
  [%expect {|
    (Type_array (3, (Type_array (2, (Type_func ([], [])))))) |}]
;;
