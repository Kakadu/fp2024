(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Expressions

(************************** Identificators **************************)

let%expect_test "single underscore should not be parsed as an expression" =
  pp pp_expression parse_expr {| _ |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse ident starting with underscore" =
  pp pp_expression parse_expr {| _foo |};
  [%expect {| (Expr_ident_or_op "_foo") |}]
;;

let%expect_test "parse ident with apostrophe" =
  pp pp_expression parse_expr {| x' |};
  [%expect {| (Expr_ident_or_op "x'") |}]
;;

let%expect_test "parse ident with digits not first" =
  pp pp_expression parse_expr {| foobar123 |};
  [%expect {| (Expr_ident_or_op "foobar123") |}]
;;

let%expect_test "parse ident with digit first should fail" =
  pp pp_expression parse_expr {| 7myname |};
  [%expect {| : end_of_input |}]
;;

(************************** Constants **************************)

let%expect_test "parse int as expr const int" =
  pp pp_expression parse_expr {|42|};
  [%expect {| (Expr_const (Const_int 42)) |}]
;;

let%expect_test "parse true as expr const bool true" =
  pp pp_expression parse_expr {|true|};
  [%expect {| (Expr_const (Const_bool true)) |}]
;;

let%expect_test "parse false as expr const bool false" =
  pp pp_expression parse_expr {|false|};
  [%expect {| (Expr_const (Const_bool false)) |}]
;;

let%expect_test "parse char as expr const char" =
  pp pp_expression parse_expr {|'a'|};
  [%expect {| (Expr_const (Const_char 'a')) |}]
;;

let%expect_test "parse char without closing single quote should fail" =
  pp pp_expression parse_expr {|'a|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse char with more than one char in quotes should fail" =
  pp pp_expression parse_expr {|'ab'|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse nonempty string as expr const string" =
  pp pp_expression parse_expr {|"mystring"|};
  [%expect {| (Expr_const (Const_string "mystring")) |}]
;;

let%expect_test "parse empty string as expr const string" =
  pp pp_expression parse_expr {|""|};
  [%expect {| (Expr_const (Const_string "")) |}]
;;

let%expect_test "parse string without opening double quote should fail" =
  pp pp_expression parse_expr {|mystring"|};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse string without closing double quotes should fail" =
  pp pp_expression parse_expr {|"mystring|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse simple float with rational part" =
  pp pp_expression parse_expr {| 3.14 |};
  [%expect {| (Expr_const (Const_float 3.14))  |}]
;;

let%expect_test "parse simple float with rational part and (f|F)" =
  pp pp_expression parse_expr {| 3.14f |};
  [%expect {| (Expr_const (Const_float 3.14)) |}]
;;

let%expect_test "parse simple float without rational part" =
  pp pp_expression parse_expr {| 5. |};
  [%expect {| (Expr_const (Const_float 5.))  |}]
;;

let%expect_test "parse float with rational part, (e|E) and signed exponent" =
  pp pp_expression parse_expr {| 1.23e-2 |};
  [%expect {| (Expr_const (Const_float 0.0123))  |}]
;;

let%expect_test "parse float with rational part, (e|E), signed exponent and (f|F)" =
  pp pp_expression parse_expr {| 1.23e-2F |};
  [%expect {| (Expr_const (Const_float 0.0123)) |}]
;;

let%expect_test "parse float with rational part, (e|E) and unsigned exponent" =
  pp pp_expression parse_expr {| 1.23e2 |};
  [%expect {| (Expr_const (Const_float 123.))  |}]
;;

let%expect_test "parse float without rational part, with (e|E) and signed exponent" =
  pp pp_expression parse_expr {| 1e+2 |};
  [%expect {| : end_of_input  |}]
;;

let%expect_test "parse float with rational part, (e|E) but without exponent should fail" =
  pp pp_expression parse_expr {| 1.23E+ |};
  [%expect {| : no more choices  |}]
;;

(* Need to try to test measures *)

(************************** If then else **************************)

let%expect_test "parse ite with else branch" =
  pp pp_expression parse_expr {| if true then 5 else 7 |};
  [%expect
    {|
    (Expr_ifthenelse ((Expr_const (Const_bool true)), (Expr_const (Const_int 5)),
       (Some (Expr_const (Const_int 7))))) |}]
;;

let%expect_test "parse ite without else branch" =
  pp pp_expression parse_expr {| if true then 5 |};
  [%expect
    {|
    (Expr_ifthenelse ((Expr_const (Const_bool true)), (Expr_const (Const_int 5)),
       None)) |}]
;;

let%expect_test "parse ite without then branch should fail" =
  pp pp_expression parse_expr {| if 5 |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse ite with nested ite" =
  pp pp_expression parse_expr {| if if true then true else true then 5 else 7 |};
  [%expect
    {|
    (Expr_ifthenelse (
       (Expr_ifthenelse ((Expr_const (Const_bool true)),
          (Expr_const (Const_bool true)), (Some (Expr_const (Const_bool true))))),
       (Expr_const (Const_int 5)), (Some (Expr_const (Const_int 7))))) |}]
;;

(************************** Application **************************)

let%expect_test "parse application of function to 1 argument" =
  pp pp_expression parse_expr {| f a |};
  [%expect {| (Expr_apply ((Expr_ident_or_op "f"), (Expr_ident_or_op "a"))) |}]
;;

let%expect_test "parse application of function to 2 arguments" =
  pp pp_expression parse_expr {| f a b |};
  [%expect
    {|
    (Expr_apply ((Expr_apply ((Expr_ident_or_op "f"), (Expr_ident_or_op "a"))),
       (Expr_ident_or_op "b"))) |}]
;;

let%expect_test "parse application of function to 5 arguments" =
  pp pp_expression parse_expr {| f a b c d e |};
  [%expect
    {|
    (Expr_apply (
       (Expr_apply (
          (Expr_apply (
             (Expr_apply (
                (Expr_apply ((Expr_ident_or_op "f"), (Expr_ident_or_op "a"))),
                (Expr_ident_or_op "b"))),
             (Expr_ident_or_op "c"))),
          (Expr_ident_or_op "d"))),
       (Expr_ident_or_op "e"))) |}]
;;

(************************** Binary operations **************************)

let%expect_test "parse a+b" =
  pp pp_expression parse_expr {| a+b |};
  [%expect
    {|
    (Expr_apply ((Expr_apply ((Expr_ident_or_op "+"), (Expr_ident_or_op "a"))),
       (Expr_ident_or_op "b"))) |}]
;;

let%expect_test " parse a + + should fail " =
  pp pp_expression parse_expr {| a + + |};
  [%expect {|
    : end_of_input |}]
;;

let%expect_test "parse 1+b" =
  pp pp_expression parse_expr {| 1+b |};
  [%expect
    {|
    (Expr_apply (
       (Expr_apply ((Expr_ident_or_op "+"), (Expr_const (Const_int 1)))),
       (Expr_ident_or_op "b"))) |}]
;;

let%expect_test "parse a+b+c" =
  pp pp_expression parse_expr {| a+b+c |};
  [%expect
    {|
    (Expr_apply (
       (Expr_apply ((Expr_ident_or_op "+"),
          (Expr_apply (
             (Expr_apply ((Expr_ident_or_op "+"), (Expr_ident_or_op "a"))),
             (Expr_ident_or_op "b")))
          )),
       (Expr_ident_or_op "c"))) |}]
;;

let%expect_test "parse n-1 " =
  pp pp_expression parse_expr {| n-1 |};
  [%expect
    {|
    (Expr_apply ((Expr_apply ((Expr_ident_or_op "-"), (Expr_ident_or_op "n"))),
       (Expr_const (Const_int 1)))) |}]
;;

let%expect_test "parse a+b*c" =
  pp pp_expression parse_expr {| a+b*c |};
  [%expect
    {|
      (Expr_apply ((Expr_apply ((Expr_ident_or_op "+"), (Expr_ident_or_op "a"))),
         (Expr_apply (
            (Expr_apply ((Expr_ident_or_op "*"), (Expr_ident_or_op "b"))),
            (Expr_ident_or_op "c")))
         )) |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse ident inside parentheses" =
  pp pp_expression parse_expr {| (a) |};
  [%expect {| (Expr_ident_or_op "a") |}]
;;

let%expect_test "parse expression inside parentheses" =
  pp pp_expression parse_expr {| (a b) |};
  [%expect {| (Expr_apply ((Expr_ident_or_op "a"), (Expr_ident_or_op "b"))) |}]
;;

let%expect_test "parse expression in parentheses without closing parenthesis should fail" =
  pp pp_expression parse_expr {| (a b |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse expression in parentheses without opening parenthesis should fail" =
  pp pp_expression parse_expr {| a b) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse expression inside nested parentheses" =
  pp pp_expression parse_expr {| (((a b))) |};
  [%expect {| (Expr_apply ((Expr_ident_or_op "a"), (Expr_ident_or_op "b"))) |}]
;;

let%expect_test "parse expression inside unbalanced nested parentheses should fail" =
  pp pp_expression parse_expr {| (((a b)))))) |};
  [%expect {| : end_of_input |}]
;;

(************************** Mix **************************)

let%expect_test _ =
  pp pp_expression parse_expr {| if a then (if f b then c) else g d |};
  [%expect
    {|
    (Expr_ifthenelse ((Expr_ident_or_op "a"),
       (Expr_ifthenelse (
          (Expr_apply ((Expr_ident_or_op "f"), (Expr_ident_or_op "b"))),
          (Expr_ident_or_op "c"), None)),
       (Some (Expr_apply ((Expr_ident_or_op "g"), (Expr_ident_or_op "d")))))) |}]
;;

let%expect_test _ =
  pp pp_expression parse_expr {| 1 + if a then b else c + 2 |};
  [%expect
    {|
    (Expr_apply (
       (Expr_apply ((Expr_ident_or_op "+"), (Expr_const (Const_int 1)))),
       (Expr_ifthenelse ((Expr_ident_or_op "a"), (Expr_ident_or_op "b"),
          (Some (Expr_apply (
                   (Expr_apply ((Expr_ident_or_op "+"), (Expr_ident_or_op "c"))),
                   (Expr_const (Const_int 2)))))
          ))
       )) |}]
;;
