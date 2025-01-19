(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base
open Pp
open Parse.Expressions
open Pprint.Pprinter

(************************** Identificators **************************)

let%expect_test "single underscore should not be parsed as an expression" =
  pp pprint_expr parse_expr {| _ |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse ident starting with underscore" =
  pp pprint_expr parse_expr {| _foo |};
  [%expect {| _foo |}]
;;

let%expect_test "parse ident with apostrophe" =
  pp pprint_expr parse_expr {| x' |};
  [%expect {| x' |}]
;;

let%expect_test "parse ident with digits not first" =
  pp pprint_expr parse_expr {| foobar123 |};
  [%expect {| foobar123 |}]
;;

let%expect_test "parse ident with digit first should fail" =
  pp pprint_expr parse_expr {| 7myname |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse + operator inside parentheses" =
  pp pprint_expr parse_expr {| (+) |};
  [%expect {| + |}]
;;

let%expect_test "parse +. operator inside parentheses" =
  pp pprint_expr parse_expr {| (+.) |};
  [%expect {| +. |}]
;;

(************************** Constants **************************)

let%expect_test "parse int as expr const int" =
  pp pprint_expr parse_expr {|42|};
  [%expect {| 42 |}]
;;

let%expect_test "parse true as expr const bool true" =
  pp pprint_expr parse_expr {|true|};
  [%expect {| true |}]
;;

let%expect_test "parse false as expr const bool false" =
  pp pprint_expr parse_expr {|false|};
  [%expect {| false |}]
;;

let%expect_test "parse char as expr const char" =
  pp pprint_expr parse_expr {|'a'|};
  [%expect {| 'a' |}]
;;

let%expect_test "parse char without closing single quote should fail" =
  pp pprint_expr parse_expr {|'a|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse char with more than one char in quotes should fail" =
  pp pprint_expr parse_expr {|'ab'|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse nonempty string as expr const string" =
  pp pprint_expr parse_expr {|"mystring"|};
  [%expect {| "mystring" |}]
;;

let%expect_test "parse empty string as expr const string" =
  pp pprint_expr parse_expr {|""|};
  [%expect {| "" |}]
;;

let%expect_test "parse string without opening double quote should fail" =
  pp pprint_expr parse_expr {|mystring"|};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse string without closing double quotes should fail" =
  pp pprint_expr parse_expr {|"mystring|};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse simple float with rational part" =
  pp pprint_expr parse_expr {| 3.14 |};
  [%expect {|
    3.14  |}]
;;

let%expect_test "parse simple float with rational part and (f|F)" =
  pp pprint_expr parse_expr {| 3.14f |};
  [%expect {|
    3.14 |}]
;;

let%expect_test "parse simple float without rational part" =
  pp pprint_expr parse_expr {| 5. |};
  [%expect {|
    5.  |}]
;;

let%expect_test "parse float with rational part, (e|E) and signed exponent" =
  pp pprint_expr parse_expr {| 1.23e-2 |};
  [%expect {|
    0.0123  |}]
;;

let%expect_test "parse float with rational part, (e|E), signed exponent and (f|F)" =
  pp pprint_expr parse_expr {| 1.23e-2F |};
  [%expect {|
    0.0123 |}]
;;

let%expect_test "parse float with rational part, (e|E) and unsigned exponent" =
  pp pprint_expr parse_expr {| 1.23e2 |};
  [%expect {|
    123.  |}]
;;

let%expect_test "parse float without rational part, with (e|E) and signed exponent" =
  pp pprint_expr parse_expr {| 1e+2 |};
  [%expect {|
    100.  |}]
;;

let%expect_test "parse float with rational part, (e|E) but without exponent should fail" =
  pp pprint_expr parse_expr {| 1.23E+ |};
  [%expect {| : no more choices  |}]
;;

(* Need to try to test measures *)

(************************** If then else **************************)

let%expect_test "parse ite with else branch" =
  pp pprint_expr parse_expr {| if true then 5 else 7 |};
  [%expect {|
    if true then 5 else 7 |}]
;;

let%expect_test "parse ite without else branch" =
  pp pprint_expr parse_expr {| if true then 5 |};
  [%expect {|
    if true then 5 |}]
;;

let%expect_test "parse ite without then branch should fail" =
  pp pprint_expr parse_expr {| if 5 |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse ite with nested ite" =
  pp pprint_expr parse_expr {| if if true then true else true then 5 else 7 |};
  [%expect {|
    if if true then true else true then 5 else 7 |}]
;;

(************************** Application **************************)

let%expect_test "parse application of function to 1 argument" =
  pp pprint_expr parse_expr {| f a |};
  [%expect {| f a |}]
;;

(* Should omit parentheses *)
let%expect_test "parse application of function to 2 arguments" =
  pp pprint_expr parse_expr {| f a b |};
  [%expect {|
(f a) b |}]
;;

let%expect_test "parse application of function to 5 arguments" =
  pp pprint_expr parse_expr {| f a b c d e |};
  [%expect {|
((((f a) b) c) d) e |}]
;;

(* Should print them either as (+) a b or a+b *)
let%expect_test "parse application (+)a b" =
  pp pprint_expr parse_expr {| (+)a b |};
  [%expect {|
    (+ a) b |}]
;;

let%expect_test "parse application (+.) a b" =
  pp pprint_expr parse_expr {| (+.) a b |};
  [%expect {|
    (+. a) b |}]
;;

let%expect_test "parse negative int" =
  pp pprint_expr parse_expr {| -1 |};
  [%expect {|
    -1 |}]
;;

let%expect_test "parse negative float" =
  pp pprint_expr parse_expr {| -1.0 |};
  [%expect {|
    -1. |}]
;;

let%expect_test "parse negative ident" =
  pp pprint_expr parse_expr {| -a |};
  [%expect {|
    - a |}]
;;

let%expect_test "parse unary minuses w/o parentheses should fail" =
  pp pprint_expr parse_expr {| ---a |};
  [%expect {|
    : no more choices |}]
;;

let%expect_test "parse unary minuses with parentheses" =
  pp pprint_expr parse_expr {| -(-(-a)) |};
  [%expect {|
    - (- (- a)) |}]
;;

(************************** Binary operations **************************)

let%expect_test "parse a+b" =
  pp pprint_expr parse_expr {| a+b |};
  [%expect {|
    (+ a) b |}]
;;

let%expect_test " parse a + + should fail " =
  pp pprint_expr parse_expr {| a + + |};
  [%expect {|
    : end_of_input |}]
;;

let%expect_test "parse 1+b" =
  pp pprint_expr parse_expr {| 1+b |};
  [%expect {|
    (+ 1) b |}]
;;

let%expect_test "parse a+b+c" =
  pp pprint_expr parse_expr {| a+b+c |};
  [%expect {|
    (+ ((+ a) b)) c |}]
;;

let%expect_test "parse n-1 " =
  pp pprint_expr parse_expr {| n-1 |};
  [%expect {|
    (- n) 1 |}]
;;

let%expect_test "parse a+b*c" =
  pp pprint_expr parse_expr {| a+b*c |};
  [%expect {|
      (+ a) ((* b) c) |}]
;;

let%expect_test "parse a <= b <= c" =
  pp pprint_expr parse_expr {| a <= b <= c |};
  [%expect {|
      (<= ((<= a) b)) c |}]
;;

let%expect_test "parse a || b || c" =
  pp pprint_expr parse_expr {| a || b || c |};
  [%expect {|
      (|| a) ((|| b) c) |}]
;;

let%expect_test "parse a && b && c" =
  pp pprint_expr parse_expr {| a && b && c |};
  [%expect {|
      (&& a) ((&& b) c) |}]
;;

(************************** Lambdas **************************)

let%expect_test "parse anon function with 0 arguments should fail" =
  pp pprint_expr parse_expr {| fun -> e |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse anon function with 1 argument" =
  pp pprint_expr parse_expr {| fun x -> e |};
  [%expect {| fun x -> e |}]
;;

let%expect_test "parse anon function with 2 arguments" =
  pp pprint_expr parse_expr {| fun x y -> e |};
  [%expect {|
    fun x -> fun y -> e |}]
;;

let%expect_test "parse anon function chain argument" =
  pp pprint_expr parse_expr {| fun x -> fun y -> e |};
  [%expect {|
      fun x -> fun y -> e |}]
;;

(************************** Let ... in expressions **************************)

let%expect_test "parse let ... in with single variable" =
  pp pprint_expr parse_expr {| let a = 5 in a |};
  [%expect {|
    let a = 5 in a |}]
;;

let%expect_test "parse let without in expression should fail" =
  pp pprint_expr parse_expr {| let a = 5 |};
  [%expect {|
    : no more choices |}]
;;

let%expect_test "parse let rec a = 5 in a expression" =
  pp pprint_expr parse_expr {| let rec a = 5 in a |};
  [%expect {|
    let rec a = 5 in a |}]
;;

let%expect_test "parse let ... in expression with function application" =
  pp pprint_expr parse_expr {| let a = 5 in f a |};
  [%expect {|
    let a = 5 in f a |}]
;;

let%expect_test "parse let a = 5 and b = 4 and c = 3 and d = 2 in e expression" =
  pp pprint_expr parse_expr {| let a = 5 and b = 4 and c = 3 and d = 2 in e |};
  [%expect {|
    let a = 5 and b = 4 and c = 3 and d = 2 in e |}]
;;

(* Is very slow on 5+ nested let ... in *)
let%expect_test "parse nested let .. in expressions" =
  pp
    pprint_expr
    parse_expr
    {|
  let a = 1 in let b = 2 in let c = 3 in let d = 4 in E |};
  [%expect {|
    let a = 1 in let b = 2 in let c = 3 in let d = 4 in E |}]
;;

let%expect_test "parse let f a b c = x in e" =
  pp pprint_expr parse_expr {| let f a b c = x in e |};
  [%expect {|
    let f = fun a -> fun b -> fun c -> x in e |}]
;;

(************************** Tuples **************************)

let%expect_test "parse expression tuple with 0 elements should fail" =
  pp pprint_expr parse_expr {| , |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse expression tuple with 1 element should fail" =
  pp pprint_expr parse_expr {| 1, |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse expression tuple with 2 elements" =
  pp pprint_expr parse_expr {| 1,2 |};
  [%expect {|
      (1, 2) |}]
;;

let%expect_test "parse expression tuple with 3 elements" =
  pp pprint_expr parse_expr {| 1, 2, myname |};
  [%expect {|
    (1, 2, myname) |}]
;;

let%expect_test "parse expression tuple of tuples" =
  pp pprint_expr parse_expr {| (1, 2), (3, 4) |};
  [%expect {|
    ((1, 2), (3, 4)) |}]
;;

let%expect_test "parse expression tuple of lists" =
  pp pprint_expr parse_expr {| ( [1; 2], [3; 4] ) |};
  [%expect {|
    ([1; 2], [3; 4])  |}]
;;

(************************** Lists **************************)

let%expect_test "parse expression empty list" =
  pp pprint_expr parse_expr {| [] |};
  [%expect {| []  |}]
;;

let%expect_test "parse expression list of 1 element" =
  pp pprint_expr parse_expr {| [a] |};
  [%expect {| [a]  |}]
;;

let%expect_test "parse expression list of 2 elements" =
  pp pprint_expr parse_expr {| [a; b] |};
  [%expect {| [a; b]  |}]
;;

let%expect_test "parse expression list of list" =
  pp pprint_expr parse_expr {| [ [ 1; 2; 3] ] |};
  [%expect {|
    [[1; 2; 3]]  |}]
;;

let%expect_test "parse expression list of tuples without parentheses" =
  pp pprint_expr parse_expr {| [ 1, 2; 3, 4 ] |};
  [%expect {|
    [(1, 2); (3, 4)]  |}]
;;

(************************** Typed **************************)

let%expect_test "parse typed expression" =
  pp pprint_expr parse_expr {| (a : int) |};
  [%expect {|
    (a : int)  |}]
;;

let%expect_test "parse doubly typed expression" =
  pp pprint_expr parse_expr {| ((a : int) : int) |};
  [%expect {|
    ((a : int) : int)  |}]
;;

let%expect_test "parse typed expression without parentheses should fail" =
  pp pprint_expr parse_expr {| a : int |};
  [%expect {|
    : end_of_input  |}]
;;

(************************** Match expressions **************************)

let%expect_test "parse match with one rule" =
  pp pprint_expr parse_expr {| match x with | P1 -> E2 |};
  [%expect {|
    match x with P1 -> E2  |}]
;;

let%expect_test "parse match with two rules" =
  pp pprint_expr parse_expr {| match x with | P1 -> E2 | P2 -> E2 |};
  [%expect {|
    match x with P1 -> E2 | P2 -> E2  |}]
;;

let%expect_test "parse match with rules containing OR pattern as the first" =
  pp pprint_expr parse_expr {| match x with | P1 | P2 | P3 -> E1 | P4 -> E2 |};
  [%expect {|
    match x with P1 | P2 | P3 -> E1 | P4 -> E2  |}]
;;

let%expect_test "parse match with rules containing OR pattern as the last" =
  pp pprint_expr parse_expr {| match x with | P1 -> E1 | P2 | P3 | P4 -> E2 |};
  [%expect {|
    match x with P1 -> E1 | P2 | P3 | P4 -> E2  |}]
;;

let%expect_test "parse match without argument should fail" =
  pp pprint_expr parse_expr {| match with | P1 -> E2 | P2 -> E2 |};
  [%expect {|
    : no more choices  |}]
;;

(************************** Function expressions **************************)

let%expect_test "parse match with one rule" =
  pp pprint_expr parse_expr {| function | P1 -> E2 |};
  [%expect {|
    function P1 -> E2  |}]
;;

let%expect_test "parse match with two rules" =
  pp pprint_expr parse_expr {| function | P1 -> E2 | P2 -> E2 |};
  [%expect {|
    function P1 -> E2 | P2 -> E2  |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse ident inside parentheses" =
  pp pprint_expr parse_expr {| (a) |};
  [%expect {| a |}]
;;

let%expect_test "parse expression inside parentheses" =
  pp pprint_expr parse_expr {| (a b) |};
  [%expect {| a b |}]
;;

let%expect_test "parse expression in parentheses without closing parenthesis should fail" =
  pp pprint_expr parse_expr {| (a b |};
  [%expect {| : no more choices |}]
;;

let%expect_test "parse expression in parentheses without opening parenthesis should fail" =
  pp pprint_expr parse_expr {| a b) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse expression inside nested parentheses" =
  pp pprint_expr parse_expr {| (((a b))) |};
  [%expect {| a b |}]
;;

let%expect_test "parse expression inside unbalanced nested parentheses should fail" =
  pp pprint_expr parse_expr {| (((a b)))))) |};
  [%expect {| : end_of_input |}]
;;

let%expect_test "parse (a+b)*c with priorities" =
  pp pprint_expr parse_expr {| (a+b)*c |};
  [%expect {|
    (* ((+ a) b)) c  |}]
;;

(************************** Mix **************************)

let%expect_test _ =
  pp pprint_expr parse_expr {| if a then (if f b then c) else g d |};
  [%expect {|
    if a then if f b then c else g d |}]
;;

let%expect_test _ =
  pp pprint_expr parse_expr {| 1 + if a then b else c + 2 |};
  [%expect {|
    (+ 1) (if a then b else (+ c) 2) |}]
;;
