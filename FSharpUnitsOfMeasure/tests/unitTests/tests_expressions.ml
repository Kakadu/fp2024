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
  [%expect {| : no more choices |}]
;;

let%expect_test "parse + operator inside parentheses" =
  pp pp_expression parse_expr {| (+) |};
  [%expect {| (Expr_ident_or_op "+") |}]
;;

let%expect_test "parse +. operator inside parentheses" =
  pp pp_expression parse_expr {| (+.) |};
  [%expect {| (Expr_ident_or_op "+.") |}]
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
  [%expect {| (Expr_const (Const_float 100.))  |}]
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

let%expect_test "parse application (+)a b" =
  pp pp_expression parse_expr {| (+)a b |};
  [%expect
    {|
    (Expr_apply ((Expr_apply ((Expr_ident_or_op "+"), (Expr_ident_or_op "a"))),
       (Expr_ident_or_op "b"))) |}]
;;

let%expect_test "parse application (+.) a b" =
  pp pp_expression parse_expr {| (+.) a b |};
  [%expect
    {|
    (Expr_apply ((Expr_apply ((Expr_ident_or_op "+."), (Expr_ident_or_op "a"))),
       (Expr_ident_or_op "b"))) |}]
;;

let%expect_test "parse negative int" =
  pp pp_expression parse_expr {| -1 |};
  [%expect
    {|
    (Expr_apply ((Expr_ident_or_op "-"), (Expr_const (Const_int 1)))) |}]
;;

let%expect_test "parse negative float" =
  pp pp_expression parse_expr {| -1.0 |};
  [%expect
    {|
    (Expr_apply ((Expr_ident_or_op "-"), (Expr_const (Const_float 1.)))) |}]
;;

let%expect_test "parse negative ident" =
  pp pp_expression parse_expr {| -a |};
  [%expect
    {|
    (Expr_apply ((Expr_ident_or_op "-"), (Expr_ident_or_op "a"))) |}]
;;

let%expect_test "parse unary minuses w/o parentheses should fail" =
  pp pp_expression parse_expr {| ---a |};
  [%expect
    {|
    : no more choices |}]
;;

let%expect_test "parse unary minuses with parentheses" =
  pp pp_expression parse_expr {| -(-(-a)) |};
  [%expect
    {|
    (Expr_apply ((Expr_ident_or_op "-"),
       (Expr_apply ((Expr_ident_or_op "-"),
          (Expr_apply ((Expr_ident_or_op "-"), (Expr_ident_or_op "a")))))
       )) |}]
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

let%expect_test "parse a <= b <= c" =
  pp pp_expression parse_expr {| a <= b <= c |};
  [%expect
    {|
      (Expr_apply (
         (Expr_apply ((Expr_ident_or_op "<="),
            (Expr_apply (
               (Expr_apply ((Expr_ident_or_op "<="), (Expr_ident_or_op "a"))),
               (Expr_ident_or_op "b")))
            )),
         (Expr_ident_or_op "c"))) |}]
;;

let%expect_test "parse a || b || c" =
  pp pp_expression parse_expr {| a || b || c |};
  [%expect
    {|
      (Expr_apply ((Expr_apply ((Expr_ident_or_op "||"), (Expr_ident_or_op "a"))),
         (Expr_apply (
            (Expr_apply ((Expr_ident_or_op "||"), (Expr_ident_or_op "b"))),
            (Expr_ident_or_op "c")))
         )) |}]
;;

let%expect_test "parse a && b && c" =
  pp pp_expression parse_expr {| a && b && c |};
  [%expect
    {|
      (Expr_apply ((Expr_apply ((Expr_ident_or_op "&&"), (Expr_ident_or_op "a"))),
         (Expr_apply (
            (Expr_apply ((Expr_ident_or_op "&&"), (Expr_ident_or_op "b"))),
            (Expr_ident_or_op "c")))
         )) |}]
;;

(************************** Lambdas **************************)

let%expect_test "parse anon function with 0 arguments should fail" =
  pp pp_expression parse_expr {| fun -> e |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse anon function with 1 argument" =
  pp pp_expression parse_expr {| fun x -> e |};
  [%expect {| (Expr_lam ((Pattern_ident_or_op "x"), (Expr_ident_or_op "e"))) |}]
;;

let%expect_test "parse anon function with 2 arguments" =
  pp pp_expression parse_expr {| fun x y -> e |};
  [%expect
    {|
    (Expr_lam ((Pattern_ident_or_op "x"),
       (Expr_lam ((Pattern_ident_or_op "y"), (Expr_ident_or_op "e"))))) |}]
;;

let%expect_test "parse anon function chain argument" =
  pp pp_expression parse_expr {| fun x -> fun y -> e |};
  [%expect
    {|
      (Expr_lam ((Pattern_ident_or_op "x"),
         (Expr_lam ((Pattern_ident_or_op "y"), (Expr_ident_or_op "e"))))) |}]
;;

(************************** Let ... in expressions **************************)

let%expect_test "parse let ... in with single variable" =
  pp pp_expression parse_expr {| let a = 5 in a |};
  [%expect
    {|
    (Expr_let (Nonrecursive,
       (Bind ((Pattern_ident_or_op "a"), (Expr_const (Const_int 5)))), [],
       (Expr_ident_or_op "a"))) |}]
;;

let%expect_test "parse let without in expression should fail" =
  pp pp_expression parse_expr {| let a = 5 |};
  [%expect {|
    : no more choices |}]
;;

let%expect_test "parse let rec a = 5 in a expression" =
  pp pp_expression parse_expr {| let rec a = 5 in a |};
  [%expect
    {|
    (Expr_let (Recursive,
       (Bind ((Pattern_ident_or_op "a"), (Expr_const (Const_int 5)))), [],
       (Expr_ident_or_op "a"))) |}]
;;

let%expect_test "parse let ... in expression with function application" =
  pp pp_expression parse_expr {| let a = 5 in f a |};
  [%expect
    {|
    (Expr_let (Nonrecursive,
       (Bind ((Pattern_ident_or_op "a"), (Expr_const (Const_int 5)))), [],
       (Expr_apply ((Expr_ident_or_op "f"), (Expr_ident_or_op "a"))))) |}]
;;

let%expect_test "parse let a = 5 and b = 4 in e expression" =
  pp pp_expression parse_expr {| let a = 5 and b = 4 in e |};
  [%expect
    {|
    (Expr_let (Nonrecursive,
       (Bind ((Pattern_ident_or_op "a"), (Expr_const (Const_int 5)))),
       [(Bind ((Pattern_ident_or_op "b"), (Expr_const (Const_int 4))))],
       (Expr_ident_or_op "e"))) |}]
;;

(* Doesn't halt on 4+ nested let ... in's, for some reason... *)

let%expect_test "parse nested let .. in expressions" =
  pp pp_expression parse_expr {| let a = 1 in let b = 2 in let c = 3 in E |};
  [%expect
    {|
    (Expr_let (Nonrecursive,
       (Bind ((Pattern_ident_or_op "a"), (Expr_const (Const_int 1)))), [],
       (Expr_let (Nonrecursive,
          (Bind ((Pattern_ident_or_op "b"), (Expr_const (Const_int 2)))),
          [],
          (Expr_let (Nonrecursive,
             (Bind ((Pattern_ident_or_op "c"), (Expr_const (Const_int 3)))),
             [], (Expr_ident_or_op "E")))
          ))
       )) |}]
;;

let%expect_test "parse let f a b c = x in e" =
  pp pp_expression parse_expr {| let f a b c = x in e |};
  [%expect
    {|
    (Expr_let (Nonrecursive,
       (Bind ((Pattern_ident_or_op "f"),
          (Expr_lam ((Pattern_ident_or_op "a"),
             (Expr_lam ((Pattern_ident_or_op "b"),
                (Expr_lam ((Pattern_ident_or_op "c"), (Expr_ident_or_op "x")))))
             ))
          )),
       [], (Expr_ident_or_op "e"))) |}]
;;

(************************** Tuples **************************)

let%expect_test "parse expression tuple with 0 elements should fail" =
  pp pp_expression parse_expr {| , |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse expression tuple with 1 element should fail" =
  pp pp_expression parse_expr {| 1, |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse expression tuple with 2 elements" =
  pp pp_expression parse_expr {| 1,2 |};
  [%expect
    {|
      (Expr_tuple ((Expr_const (Const_int 1)), (Expr_const (Const_int 2)), [])) |}]
;;

let%expect_test "parse expression tuple with 3 elements" =
  pp pp_expression parse_expr {| 1, 2, myname |};
  [%expect
    {|
    (Expr_tuple ((Expr_const (Const_int 1)), (Expr_const (Const_int 2)),
       [(Expr_ident_or_op "myname")])) |}]
;;

let%expect_test "parse expression tuple of tuples" =
  pp pp_expression parse_expr {| (1, 2), (3, 4) |};
  [%expect
    {|
    (Expr_tuple (
       (Expr_tuple ((Expr_const (Const_int 1)), (Expr_const (Const_int 2)), [])),
       (Expr_tuple ((Expr_const (Const_int 3)), (Expr_const (Const_int 4)), [])),
       [])) |}]
;;

let%expect_test "parse expression tuple of lists" =
  pp pp_expression parse_expr {| ( [1; 2], [3; 4] ) |};
  [%expect
    {|
    (Expr_tuple (
       (Expr_list [(Expr_const (Const_int 1)); (Expr_const (Const_int 2))]),
       (Expr_list [(Expr_const (Const_int 3)); (Expr_const (Const_int 4))]),
       []))  |}]
;;

(************************** Lists **************************)

let%expect_test "parse expression empty list" =
  pp pp_expression parse_expr {| [] |};
  [%expect {| (Expr_list [])  |}]
;;

let%expect_test "parse expression list of 1 element" =
  pp pp_expression parse_expr {| [a] |};
  [%expect {| (Expr_list [(Expr_ident_or_op "a")])  |}]
;;

let%expect_test "parse expression list of 2 elements" =
  pp pp_expression parse_expr {| [a; b] |};
  [%expect {| (Expr_list [(Expr_ident_or_op "a"); (Expr_ident_or_op "b")])  |}]
;;

let%expect_test "parse expression list of list" =
  pp pp_expression parse_expr {| [ [ 1; 2; 3] ] |};
  [%expect
    {|
    (Expr_list
       [(Expr_list
           [(Expr_const (Const_int 1)); (Expr_const (Const_int 2));
             (Expr_const (Const_int 3))])
         ])  |}]
;;

let%expect_test "parse expression list of tuples without parentheses" =
  pp pp_expression parse_expr {| [ 1, 2; 3, 4 ] |};
  [%expect
    {|
    (Expr_list
       [(Expr_tuple ((Expr_const (Const_int 1)), (Expr_const (Const_int 2)), []));
         (Expr_tuple ((Expr_const (Const_int 3)), (Expr_const (Const_int 4)),
            []))
         ])  |}]
;;

(************************** Typed **************************)

let%expect_test "parse typed expression" =
  pp pp_expression parse_expr {| (a : int) |};
  [%expect {|
    (Expr_typed ((Expr_ident_or_op "a"), (Type_ident "int")))  |}]
;;

let%expect_test "parse doubly typed expression" =
  pp pp_expression parse_expr {| ((a : int) : int) |};
  [%expect {|
    (Expr_typed ((Expr_typed ((Expr_ident_or_op "a"), (Type_ident "int"))),
       (Type_ident "int")))  |}]
;;

let%expect_test "parse typed expression without parentheses should fail" =
  pp pp_expression parse_expr {| a : int |};
  [%expect {|
    : end_of_input  |}]
;;

(************************** Match expressions **************************)

let%expect_test "parse match with one rule" =
  pp pp_expression parse_expr {| match x with | P1 -> E2 |};
  [%expect
    {|
    (Expr_match ((Expr_ident_or_op "x"),
       (Rule ((Pattern_ident_or_op "P1"), (Expr_ident_or_op "E2"))), []))  |}]
;;

let%expect_test "parse match with two rules" =
  pp pp_expression parse_expr {| match x with | P1 -> E2 | P2 -> E2 |};
  [%expect
    {|
    (Expr_match ((Expr_ident_or_op "x"),
       (Rule ((Pattern_ident_or_op "P1"), (Expr_ident_or_op "E2"))),
       [(Rule ((Pattern_ident_or_op "P2"), (Expr_ident_or_op "E2")))]))  |}]
;;

let%expect_test "parse match with rules containing OR pattern as the first" =
  pp pp_expression parse_expr {| match x with | P1 | P2 | P3 -> E1 | P4 -> E2 |};
  [%expect
    {|
    (Expr_match ((Expr_ident_or_op "x"),
       (Rule (
          (Pattern_or (
             (Pattern_or ((Pattern_ident_or_op "P1"), (Pattern_ident_or_op "P2")
                )),
             (Pattern_ident_or_op "P3"))),
          (Expr_ident_or_op "E1"))),
       [(Rule ((Pattern_ident_or_op "P4"), (Expr_ident_or_op "E2")))]))  |}]
;;

let%expect_test "parse match with rules containing OR pattern as the last" =
  pp pp_expression parse_expr {| match x with | P1 -> E1 | P2 | P3 | P4 -> E2 |};
  [%expect
    {|
    (Expr_match ((Expr_ident_or_op "x"),
       (Rule ((Pattern_ident_or_op "P1"), (Expr_ident_or_op "E1"))),
       [(Rule (
           (Pattern_or (
              (Pattern_or ((Pattern_ident_or_op "P2"), (Pattern_ident_or_op "P3")
                 )),
              (Pattern_ident_or_op "P4"))),
           (Expr_ident_or_op "E2")))
         ]
       ))  |}]
;;

let%expect_test "parse match without argument should fail" =
  pp pp_expression parse_expr {| match with | P1 -> E2 | P2 -> E2 |};
  [%expect {|
    : no more choices  |}]
;;

(************************** Function expressions **************************)

let%expect_test "parse match with one rule" =
  pp pp_expression parse_expr {| function | P1 -> E2 |};
  [%expect
    {|
    (Expr_function ((Rule ((Pattern_ident_or_op "P1"), (Expr_ident_or_op "E2"))),
       []))  |}]
;;

let%expect_test "parse match with two rules" =
  pp pp_expression parse_expr {| function | P1 -> E2 | P2 -> E2 |};
  [%expect
    {|
    (Expr_function ((Rule ((Pattern_ident_or_op "P1"), (Expr_ident_or_op "E2"))),
       [(Rule ((Pattern_ident_or_op "P2"), (Expr_ident_or_op "E2")))]))  |}]
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

let%expect_test "parse (a+b)*c with priorities" =
  pp pp_expression parse_expr {| (a+b)*c |};
  [%expect
    {|
    (Expr_apply (
       (Expr_apply ((Expr_ident_or_op "*"),
          (Expr_apply (
             (Expr_apply ((Expr_ident_or_op "+"), (Expr_ident_or_op "a"))),
             (Expr_ident_or_op "b")))
          )),
       (Expr_ident_or_op "c")))  |}]
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
