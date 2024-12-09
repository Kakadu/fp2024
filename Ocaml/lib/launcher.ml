(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Inferencer
open Interpreter

let run input =
  match parse input with
  | Ok ast ->
    (match w ast with
     | Ok t ->
       (match interpret ast with
        | Ok v -> Pprint.pp_val Format.std_formatter t v
        | Error e -> Interpreter.pp_error Format.std_formatter e)
     | Error err -> Inferencer.pp_error Format.std_formatter err)
  | Error e -> Parser.pp_error Format.std_formatter e
;;

let%expect_test "basic arithmetic" =
  run "1 + 2 * 3";
  [%expect {| - : int = 7 |}]
;;

let%expect_test "arithmetic with parentheses" =
  run "(1 + 2) * 3";
  [%expect {| - : int = 9 |}]
;;

let%expect_test "basic logic" =
  run "true && false || true";
  [%expect {| - : bool = true |}]
;;

let%expect_test "if-then-else true branch" =
  run "if 1 < 2 then 42 else 0";
  [%expect {| - : int = 42 |}]
;;

let%expect_test "if-then-else false branch" =
  run "if 2 < 1 then 42 else 0";
  [%expect {| - : int = 0 |}]
;;

let%expect_test "simple function application" =
  run "let f = fun x -> x + 1 in f 5";
  [%expect {| - : int = 6 |}]
;;

let%expect_test "recursive function - factorial" =
  run "let rec fact = fun n -> if n = 0 then 1 else n * fact (n - 1) in fact 5";
  [%expect {| - : int = 120 |}]
;;

let%expect_test "tuple" =
  run "(1, true, \"hello\")";
  [%expect {| - : (int * bool * string) = (1, true, "hello") |}]
;;

let%expect_test "list of integers" =
  run "[1; 2; 3]";
  [%expect {| - : int list = [1; 2; 3] |}]
;;

let%expect_test "some and none" =
  run "Some 42";
  [%expect {| - : int option = Some 42 |}]
;;

let%expect_test "none option" =
  run "None";
  [%expect {| - : '_0 option = None |}]
;;

let%expect_test "closure captures external variable" =
  run "let x = 10 in let f = fun y -> x + y in f 5";
  [%expect {| - : int = 15 |}]
;;

let%expect_test "closure captures and modifies external variable" =
  run "let x = 3 in let f = fun y -> let x = 5 in x + y in f 2";
  [%expect {| - : int = 7 |}]
;;

let%expect_test "closure with recursive function" =
  run
    "let rec counter = fun n -> if n = 0 then 0 else 1 + counter (n - 1) in let f = fun \
     x -> counter x in f 3";
  [%expect {| - : int = 3 |}]
;;

let%expect_test "nested closures" =
  run "let add = fun x -> fun y -> x + y in let add_five = add 5 in add_five 10";
  [%expect {| - : int = 15 |}]
;;

let%expect_test "closure with nested functions and shadowing" =
  run
    "let x = 1 in let f = fun y -> let g = fun z -> x + y + z in g 5 in let x = 10 in f 3";
  [%expect {| - : int = 9 |}]
;;

let%expect_test "closure with argument passing" =
  run
    "let make_adder = fun x -> fun y -> x + y in let add_ten = make_adder 10 in add_ten \
     20";
  [%expect {| - : int = 30 |}]
;;

let%expect_test "closure remembers state at creation" =
  run "let x = 2 in let f = fun y -> x + y in let x = 10 in f 3";
  [%expect {| - : int = 5 |}]
;;

let%expect_test "partial application returns closure" =
  run "let add = fun x -> fun y -> x + y in add 5";
  [%expect {| - : (int -> int) = <fun> |}]
;;

let%expect_test "partial application within nested functions" =
  run
    "let multiply = fun x -> fun y -> x * y in let double = multiply 2 in let triple = \
     multiply 3 in (double 10, triple 10)";
  [%expect {| - : (int * int) = (20, 30) |}]
;;

let%expect_test "partial application type inference" =
  run "let make_adder = fun x -> fun y -> x + y in let add_ten = make_adder 10 in add_ten";
  [%expect {| - : (int -> int) = <fun> |}]
;;

let%expect_test "partial application followed by application" =
  run
    "let subtract = fun x -> fun y -> x - y in let minus_five = subtract 5 in minus_five \
     3";
  [%expect {| - : int = 2 |}]
;;

let%expect_test "returning closure as function result" =
  run
    "let create_multiplier = fun x -> fun y -> x * y in let multiplier_of_two = \
     create_multiplier 2 in multiplier_of_two";
  [%expect {| - : (int -> int) = <fun> |}]
;;

let%expect_test "simple non-recursive value declaration" =
  run "let x = 42";
  [%expect {| val x : int = 42 |}]
;;

let%expect_test "recursive function declaration" =
  run "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) ";
  [%expect {| val f : (int -> int) = <fun> |}]
;;

let%expect_test "simple non-recursive function declaration" =
  run "let f = fun x -> x + 1";
  [%expect {| val f : (int -> int) = <fun> |}]
;;

let%expect_test "simple non-recursive value and function declarations" =
  run "let x = 10 and f = fun y -> x + y";
  [%expect {|
    val x : int = 10
    val f : (int -> int) = <fun> |}]
;;

let%expect_test "multiple recursive function declarations" =
  run
    "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1) in let rec g = fun x -> if \
     x = 0 then 2 else x + g (x - 1)";
  [%expect {| val g : (int -> int) = <fun> |}]
;;

let%expect_test "recursive function declaration without body" =
  run "let rec f = fun x -> if x = 0 then 1 else x * f (x - 1)";
  [%expect {| val f : (int -> int) = <fun> |}]
;;

let%expect_test "multiple non-recursive value declarations" =
  run "let x = 10 and y = 20 and z = x + y";
  [%expect {|
    val x : int = 10
    val y : int = 20
    val z : int = 30 |}]
;;

let%expect_test "multiple non-recursive function declarations" =
  run "let f = fun x -> x + 1 and g = fun y -> y * 2";
  [%expect {|
    val f : (int -> int) = <fun>
    val g : (int -> int) = <fun> |}]
;;

let%expect_test "multiple non-recursive function declarations" =
  run "let f = fun x -> x + 1";
  [%expect {|
    val f : (int -> int) = <fun> |}]
;;

let%expect_test "fixed point combinator test" =
  run
    "let rec y f x = f (y f) x in let factorial fac n = if n = 1 then 1 else n * fac (n \
     - 1) in y factorial 5";
  [%expect {|
    - : int = 120 |}]
;;
