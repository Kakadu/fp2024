(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Pprinter.Printer
open Pp

(********** const int and string **********)

let%expect_test "const int" =
  pp print_expr parse_expr {|256|};
  [%expect {| 256 |}]
;;

let%expect_test "zero" =
  pp print_expr parse_expr {|0|};
  [%expect {| 0 |}]
;;

let%expect_test "not digit in int" =
  pp print_expr parse_expr {|123,321|};
  [%expect {| : syntax error |}]
;;

let%expect_test "very big int" =
  pp print_expr parse_expr {|9999999999999999999999999999999999999999|};
  [%expect {| : syntax error |}]
;;

let%expect_test "const string" =
  pp print_expr parse_expr {|"my_string"|};
  [%expect {| "my_string" |}]
;;

let%expect_test "const empty string" =
  pp print_expr parse_expr {|""|};
  [%expect {| "" |}]
;;

let%expect_test "string with '\n'" =
  pp print_expr parse_expr {|"Hello\n"|};
  [%expect {| "Hello\n" |}]
;;

let%expect_test "const string with escaped backslash" =
  pp print_expr parse_expr {|"\\"|};
  [%expect {| "\\" |}]
;;

let%expect_test "const string with escaped quote" =
  pp print_expr parse_expr {|"\""|};
  [%expect {| "\"" |}]
;;

(********** arithmetics **********)

let%expect_test "unary plus" =
  pp print_expr parse_expr {|+5|};
  [%expect {| +5 |}]
;;

let%expect_test "unary minus" =
  pp print_expr parse_expr {|-5|};
  [%expect {| -5 |}]
;;

let%expect_test "unary not" =
  pp print_expr parse_expr {|!t|};
  [%expect {| !t |}]
;;

let%expect_test "unary receive" =
  pp print_expr parse_expr {|<-t|};
  [%expect {| <-t |}]
;;

let%expect_test "multiple unary operators" =
  pp print_expr parse_expr {|-+!--!+t|};
  [%expect {|
    -+!--!+t |}]
;;

let%expect_test "sum binop test" =
  pp print_expr parse_expr {|4 + i|};
  [%expect {| 4 + i |}]
;;

let%expect_test "sub binop test" =
  pp print_expr parse_expr {|a - 5|};
  [%expect {| a - 5 |}]
;;

let%expect_test "mul binop test" =
  pp print_expr parse_expr {|t * 5|};
  [%expect {| t * 5 |}]
;;

let%expect_test "div binop test" =
  pp print_expr parse_expr {|t / 5|};
  [%expect {| t / 5 |}]
;;

let%expect_test "modulus binop test" =
  pp print_expr parse_expr {|t % 5|};
  [%expect {| t % 5 |}]
;;

let%expect_test "equality binop test" =
  pp print_expr parse_expr {|t == 5|};
  [%expect {| t == 5 |}]
;;

let%expect_test "non equality binop test" =
  pp print_expr parse_expr {|t != 5|};
  [%expect {| t != 5 |}]
;;

let%expect_test "less binop test" =
  pp print_expr parse_expr {|t < 5|};
  [%expect {| t < 5 |}]
;;

let%expect_test "greater binop test" =
  pp print_expr parse_expr {|t > 5|};
  [%expect {| t > 5 |}]
;;

let%expect_test "greater or equal binop test" =
  pp print_expr parse_expr {|t >= 5|};
  [%expect {|
      t >= 5 |}]
;;

let%expect_test "less or equal binop test" =
  pp print_expr parse_expr {|t <= 5|};
  [%expect {|
      t <= 5 |}]
;;

let%expect_test "and binop test" =
  pp print_expr parse_expr {|t && 5|};
  [%expect {|
      t && 5 |}]
;;

let%expect_test "or binop test" =
  pp print_expr parse_expr {|t || 5|};
  [%expect {|
      t || 5 |}]
;;

let%expect_test "expr with multiple unary minuses with parens" =
  pp print_expr parse_expr {|+(+(+1))|};
  [%expect {|
    +++1|}]
;;

let%expect_test "expr with multiple unary minuses with parens" =
  pp print_expr parse_expr {|-(-(-1))|};
  [%expect {|
    ---1|}]
;;

let%expect_test "unary and binary exprs combined" =
  pp print_expr parse_expr {|-(5 + 2) / +-(2 + 5)|};
  [%expect {|
    -(5 + 2) / +-(2 + 5)|}]
;;

(********** const array **********)

let%expect_test "expr simple array" =
  pp print_expr parse_expr {|[3]int{}|};
  [%expect {| [3]int{} |}]
;;

let%expect_test "expr array with init" =
  pp print_expr parse_expr {|[3]int{1, 2}|};
  [%expect {|
    [3]int{1, 2} |}]
;;

let%expect_test "expr array with ..." =
  pp print_expr parse_expr {|[...]int{1, 2, 3, 4}|};
  [%expect {|
    [4]int{1, 2, 3, 4} |}]
;;

let%expect_test "expr const array with very big size" =
  pp print_expr parse_expr {|[9999999999999999999999999999999]int{}|};
  [%expect {| : syntax error |}]
;;

(********** ident **********)

let%expect_test "expr ident false" =
  pp print_expr parse_expr {|false|};
  [%expect {|
    false|}]
;;

let%expect_test "expr ident nil" =
  pp print_expr parse_expr {|nil|};
  [%expect {|
    nil|}]
;;

let%expect_test "expr ident" =
  pp print_expr parse_expr {|abcdefg__|};
  [%expect {|
    abcdefg__|}]
;;

let%expect_test "expr ident in parens" =
  pp print_expr parse_expr {|(abc)|};
  [%expect {|
    abc|}]
;;

let%expect_test "expr ident in multiple parens" =
  pp print_expr parse_expr {|(((abc)))|};
  [%expect {|
    abc|}]
;;

(********** func call **********)

let%expect_test "simple func call" =
  pp print_expr parse_expr "a()";
  [%expect {|
    a()|}]
;;

let%expect_test "func call with multiple complex arguments" =
  pp print_expr parse_expr "three(abc, 2 + 3, fac(25))";
  [%expect {|
    three(abc, 2 + 3, fac(25))|}]
;;

let%expect_test "nested func call" =
  pp print_expr parse_expr "a()()()";
  [%expect {|
    a()()()|}]
;;

(********** index **********)

let%expect_test "index with idents" =
  pp print_expr parse_expr {|array[i]|};
  [%expect {| array[i] |}]
;;

let%expect_test "index with int" =
  pp print_expr parse_expr {|array[1]|};
  [%expect {| array[1] |}]
;;

let%expect_test "index with constant array" =
  pp print_expr parse_expr {|[3]int{1, 2, 3}[0]|};
  [%expect {|
    [3]int{1, 2, 3}[0] |}]
;;

let%expect_test "index with function call in index" =
  pp print_expr parse_expr {|array[get_index(a, b)]|};
  [%expect {|
    array[get_index(a, b)] |}]
;;

let%expect_test "index with function call as an array" =
  pp print_expr parse_expr {|get_array(a, b)[1]|};
  [%expect {|
    get_array(a, b)[1] |}]
;;

let%expect_test "nested indicies" =
  pp print_expr parse_expr {|a[1][2][3]|};
  [%expect {|
    a[1][2][3] |}]
;;

(********** complex exprs **********)

let%expect_test "bin operators precedence test" =
  pp print_expr parse_expr "1 + 2 * 3 >= -1 - <-a / 2 || true && check()";
  [%expect {|
    1 + 2 * 3 >= -1 - <-a / 2 || true && check()|}]
;;

let%expect_test "bin operators with parens precedence test" =
  pp print_expr parse_expr "(1 + 2) * +((3 || 2 - a() / 4) == (true && false))";
  [%expect {|
    (1 + 2) * +((3 || 2 - a() / 4) == (true && false))|}]
;;

let%expect_test "expr right associativity test 1" =
  pp print_expr parse_expr {|(a || b) || c|};
  [%expect {| (a || b) || c |}]
;;

let%expect_test "expr right associativity test 2" =
  pp print_expr parse_expr {|a || (b || c)|};
  [%expect {| a || b || c |}]
;;

let%expect_test "expr left associativity test 1" =
  pp print_expr parse_expr {|a + (b + c)|};
  [%expect {| a + (b + c) |}]
;;

let%expect_test "expr left associativity test 2" =
  pp print_expr parse_expr {|(a + b) + c|};
  [%expect {| a + b + c |}]
;;

let%expect_test "expr logical operations" =
  pp print_expr parse_expr {|a && (b || c)|};
  [%expect {|
    a && (b || c)|}]
;;

let%expect_test "expr logical operations with binops" =
  pp print_expr parse_expr {|a > b + 1 && (b + 2 <= c)|};
  [%expect {|
    a > b + 1 && b + 2 <= c|}]
;;

let%expect_test "expr with multiple redundant parens" =
  pp print_expr parse_expr {|((((((((4)) + i * ((5) + ((8) + p))))))))|};
  [%expect {|
    4 + i * (5 + (8 + p))|}]
;;

let%expect_test "expr bin mult and sum" =
  pp print_expr parse_expr {|-5 * _r + 8|};
  [%expect {|
    -5 * _r + 8|}]
;;

let%expect_test "expr un and bin opers" =
  pp print_expr parse_expr {|5 - -4|};
  [%expect {|
    5 - -4|}]
;;

let%expect_test "expr_call test" =
  pp print_expr parse_expr "fac(4 + fac(4 + 4))";
  [%expect {|
    fac(4 + fac(4 + 4))|}]
;;

let%expect_test "fac_piece1 test" =
  pp print_expr parse_expr "n * fac(n-1)";
  [%expect {|
    n * fac(n - 1)|}]
;;

let%expect_test "unary_min test" =
  pp print_expr parse_expr "-n + 2 + -1";
  [%expect {|
    -n + 2 + -1|}]
;;

let%expect_test "channel receive test" =
  pp print_expr parse_expr "<-c";
  [%expect {|
    <-c|}]
;;

let%expect_test "channel receive with unop test" =
  pp print_expr parse_expr "-<-c";
  [%expect {|
    -<-c|}]
;;

let%expect_test "channel receive with binop test" =
  pp print_expr parse_expr "-<-c + 1";
  [%expect {|
    -<-c + 1|}]
;;

let%expect_test "channel neseted receive test" =
  pp print_expr parse_expr "<-<-<-c";
  [%expect {|
    <-<-<-c|}]
;;

(********** anon func **********)

let%expect_test "empty anon func" =
  pp print_expr parse_expr {|func() {}|};
  [%expect {| func() {} |}]
;;

let%expect_test "anon func with one arg and one return value" =
  pp print_expr parse_expr {|func(a int) int { return a }|};
  [%expect {|
    func(a int) int {
        return a
    } |}]
;;

let%expect_test "anon func with mult args and return values" =
  pp print_expr parse_expr {|func(a int, b string) (int, string) { return a, b }|};
  [%expect {|
    func(a int, b string) (int, string) {
        return a, b
    } |}]
;;

let%expect_test "anon func with mult args and named return values" =
  pp
    print_expr
    parse_expr
    {|func(a int, b string) (res1 int, res2 string) { res1, res2 = a, b; return }|};
  [%expect
    {|
    func(a int, b string) (res1 int, res2 string) {
        res1, res2 = a, b
        return
    } |}]
;;
