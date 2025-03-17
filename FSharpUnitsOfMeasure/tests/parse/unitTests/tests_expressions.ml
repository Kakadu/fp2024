(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Pprint.Pp
open Parse.Expressions
open Pprint.Pprinter

let run expr = pp pprint_expr pexpr expr
let run2 expr = pp2 pp_expression pexpr expr

(************************** Identificators **************************)

let%expect_test "single underscore should not be parsed as an expression" =
  run {| _ |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse ident starting with underscore" =
  run {| _foo |};
  [%expect {| _foo |}]
;;

let%expect_test "parse ident with apostrophe" =
  run {| x' |};
  [%expect {| x' |}]
;;

let%expect_test "parse ident with digits not first" =
  run {| foobar123 |};
  [%expect {| foobar123 |}]
;;

let%expect_test "parse ident with digit first should fail" =
  run {| 7myname |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse + operator inside parentheses" =
  run {| (+) |};
  [%expect {| + |}]
;;

let%expect_test "parse +. operator inside parentheses" =
  run {| (+.) |};
  [%expect {| +. |}]
;;

(************************** Constants **************************)

let%expect_test "parse int as expr const int" =
  run {|42|};
  [%expect {| 42 |}]
;;

let%expect_test "parse true as expr const bool true" =
  run {|true|};
  [%expect {| true |}]
;;

let%expect_test "parse false as expr const bool false" =
  run {|false|};
  [%expect {| false |}]
;;

let%expect_test "parse char as expr const char" =
  run {|'a'|};
  [%expect {| 'a' |}]
;;

let%expect_test "parse char without closing single quote should fail" =
  run {|'a|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse char with more than one char in quotes should fail" =
  run {|'ab'|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse nonempty string as expr const string" =
  run {|"mystring"|};
  [%expect {| "mystring" |}]
;;

let%expect_test "parse empty string as expr const string" =
  run {|""|};
  [%expect {| "" |}]
;;

let%expect_test "parse string without opening double quote should fail" =
  run {|mystring"|};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse string without closing double quotes should fail" =
  run {|"mystring|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse simple float with rational part" =
  run {| 3.14 |};
  [%expect {|
    3.14  |}]
;;

let%expect_test "parse simple float with rational part and (f|F)" =
  run {| 3.14f |};
  [%expect {|
    3.14 |}]
;;

let%expect_test "parse simple float without rational part" =
  run {| 5. |};
  [%expect {|
    5.  |}]
;;

let%expect_test "parse float with rational part, (e|E) and signed exponent" =
  run {| 1.23e-2 |};
  [%expect {|
    0.0123  |}]
;;

let%expect_test "parse float with rational part, (e|E), signed exponent and (f|F)" =
  run {| 1.23e-2F |};
  [%expect {|
    0.0123 |}]
;;

let%expect_test "parse float with rational part, (e|E) and unsigned exponent" =
  run {| 1.23e2 |};
  [%expect {|
    123.  |}]
;;

let%expect_test "parse float without rational part, with (e|E) and signed exponent" =
  run {| 1e+2 |};
  [%expect {|
    100.  |}]
;;

let%expect_test "parse float with rational part, (e|E) but without exponent should fail" =
  run {| 1.23E+ |};
  [%expect {| : no more choices  |}]
;;

(************************** If then else **************************)

let%expect_test "parse ite with else branch" =
  run {| if true then 5 else 7 |};
  [%expect {|
    if true then 5 else 7 |}]
;;

let%expect_test "parse ite without else branch" =
  run {| if true then 5 |};
  [%expect {|
    if true then 5 |}]
;;

let%expect_test "parse ite without then branch should fail" =
  run {| if 5 |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse ite with nested ite" =
  run {| if if true then true else true then 5 else 7 |};
  [%expect {|
    if if true then true else true then 5 else 7 |}]
;;

(************************** Application **************************)

let%expect_test "parse application of function to 1 argument" =
  run {| f a |};
  [%expect {| f a |}]
;;

(* Should omit parentheses *)
let%expect_test "parse application of function to 2 arguments" =
  run {| f a b |};
  [%expect {|
(f a) b |}]
;;

let%expect_test "parse application of function to 5 arguments" =
  run {| f a b c d e |};
  [%expect {|
((((f a) b) c) d) e |}]
;;

let%expect_test "parse if a then b else c d" =
  run2 {| if a then b else c d |};
  [%expect
    {|
(Expr_ifthenelse ((Expr_ident_or_op "a"), (Expr_ident_or_op "b"),
   (Some (Expr_apply ((Expr_ident_or_op "c"), (Expr_ident_or_op "d")))))) |}]
;;

let%expect_test "parse (if a then b else c) d" =
  run2 {| (if a then b else c) d |};
  [%expect
    {|
(Expr_apply (
   (Expr_ifthenelse ((Expr_ident_or_op "a"), (Expr_ident_or_op "b"),
      (Some (Expr_ident_or_op "c")))),
   (Expr_ident_or_op "d"))) |}]
;;

let%expect_test "parse application (+)a b" =
  run {| (+)a b |};
  [%expect {|
    a + b |}]
;;

let%expect_test "parse application (+.) a b" =
  run {| (+.) a b |};
  [%expect {|
    a +. b |}]
;;

let%expect_test "parse negative int" =
  run {| -1 |};
  [%expect {|
    -1 |}]
;;

let%expect_test "parse negative float" =
  run {| -1.0 |};
  [%expect {|
    -1. |}]
;;

let%expect_test "parse negative ident" =
  run {| -a |};
  [%expect {|
    - a |}]
;;

let%expect_test "parse unary minuses w/o parentheses should fail" =
  run {| ---a |};
  [%expect {|
    : no more choices |}]
;;

let%expect_test "parse unary minuses with parentheses" =
  run {| -(-(-a)) |};
  [%expect {|
    - (- (- a)) |}]
;;

(************************** Binary operations **************************)

let%expect_test "parse a+b" =
  run {| a+b |};
  [%expect {|
    a + b |}]
;;

let%expect_test " parse a + + should fail " =
  run {| a + + |};
  [%expect {|
    : end_of_input |}]
;;

let%expect_test "parse 1+b" =
  run {| 1+b |};
  [%expect {|
    1 + b |}]
;;

let%expect_test "parse a+b+c" =
  run {| a+b+c |};
  [%expect {|
    (a + b) + c |}]
;;

let%expect_test "parse n-1 " =
  run {| n-1 |};
  [%expect {|
    n - 1 |}]
;;

let%expect_test "parse a+b*c" =
  run {| a+b*c |};
  [%expect {|
      a + (b * c) |}]
;;

let%expect_test "parse a <= b <= c" =
  run {| a <= b <= c |};
  [%expect {|
      (a <= b) <= c |}]
;;

let%expect_test "parse a || b || c" =
  run {| a || b || c |};
  [%expect {|
      a || (b || c) |}]
;;

let%expect_test "parse a && b && c" =
  run {| a && b && c |};
  [%expect {|
      a && (b && c) |}]
;;

let%expect_test "parse a :: b" =
  run2 {| a :: b |};
  [%expect
    {|
      (Expr_apply ((Expr_apply ((Expr_ident_or_op "::"), (Expr_ident_or_op "a"))),
         (Expr_ident_or_op "b"))) |}]
;;

let%expect_test "parse a :: b :: c" =
  run2 {| a :: b :: c |};
  [%expect
    {|
      (Expr_apply ((Expr_apply ((Expr_ident_or_op "::"), (Expr_ident_or_op "a"))),
         (Expr_apply (
            (Expr_apply ((Expr_ident_or_op "::"), (Expr_ident_or_op "b"))),
            (Expr_ident_or_op "c")))
         )) |}]
;;

(************************** Lambdas **************************)

let%expect_test "parse anon function with 0 arguments should fail" =
  run {| fun -> e |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse anon function with 1 argument" =
  run {| fun x -> e |};
  [%expect {| fun x -> e |}]
;;

let%expect_test "parse anon function with 2 arguments" =
  run {| fun x y -> e |};
  [%expect {|
    fun x -> fun y -> e |}]
;;

let%expect_test "parse anon function chain argument" =
  run {| fun x -> fun y -> e |};
  [%expect {|
      fun x -> fun y -> e |}]
;;

(************************** Let ... in expressions **************************)

let%expect_test "parse let ... in with single variable" =
  run {| let a = 5 in a |};
  [%expect {|
    let a = 5 in a |}]
;;

let%expect_test "parse let without in expression should fail" =
  run {| let a = 5 |};
  [%expect {|
    : no more choices |}]
;;

let%expect_test "parse let rec a = 5 in a expression" =
  run {| let rec a = 5 in a |};
  [%expect {|
    let rec a = 5 in a |}]
;;

let%expect_test "parse let ... in expression with function application" =
  run {| let a = 5 in f a |};
  [%expect {|
    let a = 5 in f a |}]
;;

let%expect_test "parse let a = 5 and b = 4 and c = 3 and d = 2 in e expression" =
  run {| let a = 5 and b = 4 and c = 3 and d = 2 in e |};
  [%expect {|
    let a = 5 and b = 4 and c = 3 and d = 2 in e |}]
;;

let%expect_test "parse nested let .. in expressions" =
  run {|
  let a = 1 in let b = 2 in let c = 3 in let d = 4 in E |};
  [%expect {|
    let a = 1 in let b = 2 in let c = 3 in let d = 4 in E |}]
;;

let%expect_test "parse let f a b c = x in e" =
  run {| let f a b c = x in e |};
  [%expect {|
    let f = fun a -> fun b -> fun c -> x in e |}]
;;

(************************** Tuples **************************)

let%expect_test "parse expression tuple with 0 elements should fail" =
  run {| , |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse expression tuple with 1 element should fail" =
  run {| 1, |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse expression tuple with 2 elements" =
  run {| 1,2 |};
  [%expect {|
      (1, 2) |}]
;;

let%expect_test "parse expression tuple with 3 elements" =
  run {| 1, 2, myname |};
  [%expect {|
    (1, 2, myname) |}]
;;

let%expect_test "parse expression tuple of tuples" =
  run {| (1, 2), (3, 4) |};
  [%expect {|
    ((1, 2), (3, 4)) |}]
;;

let%expect_test "parse expression tuple of lists" =
  run {| ( [1; 2], [3; 4] ) |};
  [%expect {|
    ([1; 2], [3; 4])  |}]
;;

(************************** Lists **************************)

let%expect_test "parse expression empty list" =
  run {| [] |};
  [%expect {| []  |}]
;;

let%expect_test "parse expression list of 1 element" =
  run {| [a] |};
  [%expect {| [a]  |}]
;;

let%expect_test "parse expression list of 2 elements" =
  run {| [a; b] |};
  [%expect {| [a; b]  |}]
;;

let%expect_test "parse expression list of list" =
  run {| [ [ 1; 2; 3] ] |};
  [%expect {|
    [[1; 2; 3]]  |}]
;;

let%expect_test "parse expression list of tuples without parentheses" =
  run {| [ 1, 2; 3, 4 ] |};
  [%expect {|
    [(1, 2); (3, 4)]  |}]
;;

(************************** Option **************************)

let%expect_test "parse None" =
  run2 {| None |};
  [%expect {|
    (Expr_option None)  |}]
;;

let%expect_test "parse Some x" =
  run2 {| Some x |};
  [%expect {|
    (Expr_option (Some (Expr_ident_or_op "x")))  |}]
;;

let%expect_test "parse Some should fail" =
  run2 {| Some |};
  [%expect {|
    : no more choices  |}]
;;

let%expect_test "parse Some None" =
  run2 {| Some None |};
  [%expect {|
    (Expr_option (Some (Expr_option None)))  |}]
;;

let%expect_test "parse Some (Some x)" =
  run2 {| Some (Some x) |};
  [%expect {|
  (Expr_option (Some (Expr_option (Some (Expr_ident_or_op "x")))))  |}]
;;

(************************** Typed **************************)

let%expect_test "parse typed expression" =
  run {| (a : int) |};
  [%expect {|
    (a : int)  |}]
;;

let%expect_test "parse doubly typed expression" =
  run {| ((a : int) : int) |};
  [%expect {|
    ((a : int) : int)  |}]
;;

let%expect_test "parse typed expression without parentheses should fail" =
  run {| a : int |};
  [%expect {|
    : end_of_input  |}]
;;

(************************** Match expressions **************************)

let%expect_test "parse match with one rule" =
  run {| match x with | P1 -> E2 |};
  [%expect {|
    match x with P1 -> E2  |}]
;;

let%expect_test "parse match with two rules" =
  run {| match x with | P1 -> E2 | P2 -> E2 |};
  [%expect {|
    match x with P1 -> E2 | P2 -> E2  |}]
;;

let%expect_test "parse match with rules containing OR pattern as the first" =
  run {| match x with | P1 | P2 | P3 -> E1 | P4 -> E2 |};
  [%expect {|
    match x with P1 | P2 | P3 -> E1 | P4 -> E2  |}]
;;

let%expect_test "parse match with rules containing OR pattern as the last" =
  run {| match x with | P1 -> E1 | P2 | P3 | P4 -> E2 |};
  [%expect {|
    match x with P1 -> E1 | P2 | P3 | P4 -> E2  |}]
;;

let%expect_test "parse match without argument should fail" =
  run {| match with | P1 -> E2 | P2 -> E2 |};
  [%expect {|
    : no more choices  |}]
;;

(************************** Function expressions **************************)

let%expect_test "parse match with one rule" =
  run {| function | P1 -> E2 |};
  [%expect {|
    function P1 -> E2  |}]
;;

let%expect_test "parse match with two rules" =
  run {| function | P1 -> E2 | P2 -> E2 |};
  [%expect {|
    function P1 -> E2 | P2 -> E2  |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse ident inside parentheses" =
  run {| (a) |};
  [%expect {| a |}]
;;

let%expect_test "parse expression inside parentheses" =
  run {| (a b) |};
  [%expect {| a b |}]
;;

let%expect_test "parse expression in parentheses without closing parenthesis should fail" =
  run {| (a b |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse expression in parentheses without opening parenthesis should fail" =
  run {| a b) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse expression inside nested parentheses" =
  run {| (((a b))) |};
  [%expect {| a b |}]
;;

let%expect_test "parse expression inside unbalanced nested parentheses should fail" =
  run {| (((a b)))))) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse (a+b)*c with priorities" =
  run {| (a+b)*c |};
  [%expect {|
    (a + b) * c  |}]
;;

(************************** Mix **************************)

let%expect_test _ =
  run {| if a then (if f b then c) else g d |};
  [%expect {|
    if a then if f b then c else g d |}]
;;

let%expect_test _ =
  run {| 1 + if a then b else c + 2 |};
  [%expect {|
    1 + (if a then b else c + 2) |}]
;;
