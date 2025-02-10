(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib
open Interpreter

let test_interpret s =
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    (match Inter.eval_structure parsed with
     | Ok _ -> ()
     | Error e -> printf "Interpreter error: %a\n" pp_value_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;

let%expect_test "test_unit" =
  test_interpret "let () = print_int(10 / 10 + 2 * 50 + 89 - 89)";
  [%expect {|101|}]
;;

let%expect_test "test_base_operation1" =
  test_interpret "let x = print_int(10 / 10 + 2 * 50 + 89 - 89)";
  [%expect {|101|}]
;;

let%expect_test "test_base_operation2" =
  test_interpret
    "let truth = false || false || true || false || false || false;; let result = \
     print_bool truth";
  [%expect {|true|}]
;;

let%expect_test "test_base_operation3" =
  test_interpret "let lie = print_bool(true && true && false && true && true && true)";
  [%expect {|false|}]
;;

let%expect_test "test_base_operation4" =
  test_interpret
    "let create_adder x =\n\
    \                  let adder y = x + y in\n\
    \                  adder;;\n\
    \                  let sum_two_arg = print_int(create_adder 10 20)";
  [%expect {|30|}]
;;

let%expect_test "test_lambda" =
  test_interpret
    "let create_adder = fun x -> fun y -> x + y;; let () = print_int(create_adder 7 8)";
  [%expect {|15|}]
;;

let%expect_test "test_print_string" =
  test_interpret "let () = print_endline \"I like OCaml\"";
  [%expect {|I like OCaml|}]
;;

let%expect_test "test_not_print" =
  test_interpret
    "let create_adder x =\n\
    \                  let adder y = x + y in\n\
    \                  adder;;\n\
    \                  let fac n = if n < 2 then 1 else n * fac(n-1);; let x = 1;; let y \
     = true";
  [%expect {||}]
;;

let%expect_test "test_factorial" =
  test_interpret
    "let rec fac n = if n < 2 then 1 else n * fac(n-1) ;; let result = print_int(fac 5)";
  [%expect {|120|}]
;;

let%expect_test "test_factorial_cps" =
  test_interpret
    "let rec fac_cps n k =\n\
    \                  if n=1 then k 1 else\n\
    \                  fac_cps (n-1) (fun p -> k (p*n));;\n\
    \                  let result = print_int(fac_cps 5 (fun x -> x));;";
  [%expect {|120|}]
;;

let%expect_test "test_fibonacci" =
  test_interpret
    "let rec fibo n = if n < 2 then 1 else fibo(n-1) + fibo(n-2)\n\
    \                  let result = print_int(fibo 5)";
  [%expect {|8|}]
;;

let%expect_test "test_fix" =
  test_interpret
    "let rec fix f x = f (fix f) x\n\
    \                  let fac self n = if n<=1 then 1 else n * self (n-1)\n\
    \                  let f = print_int (fix fac 5)";
  [%expect {|120|}]
;;

let%expect_test "test_nested_recursive_closure" =
  test_interpret
    "\n\
    \    let rec outer x =\n\
    \      let rec inner y = x + y in\n\
    \      inner\n\
    \    ;;\n\
    \    let inner = outer 10;;\n\
    \    let () = print_int (inner 5)";
  [%expect {|15|}]
;;

let%expect_test "test_annotate_sum" =
  test_interpret "let sum (x : int) (y : int) = x + y ;; let res = print_int(sum 10 20)";
  [%expect {|30|}]
;;

let%expect_test "test_annotate_fac" =
  test_interpret
    "let rec fac (n : int) (acc : int) = if n < 2 then acc else fac (n-1) (acc * n)\n\
    \                  let res = print_int (fac 5 1)";
  [%expect {|120|}]
;;

let%expect_test "test_tuple" =
  test_interpret
    "let (a,b) = (1 + 1 * 10,2 - 1 * 5);; let () = print_int a ;; let () = print_int b";
  [%expect {|
    11
    -3|}]
;;

let%expect_test "test_tuple" =
  test_interpret
    "let rec test5 n =\n\
    \  if n <= 0 then (0, 0)\n\
    \  else\n\
    \    let (sum, count) = test5 (n - 1) in\n\
    \    (sum + n, count + 1)\n\
    \  ;; let (a, b) = test5 5;; let () = print_int a";
  [%expect {|15|}]
;;

let%expect_test "test_nested_tuple" =
  test_interpret
    "\n\
    \    let (a, b) = (1 + 2, 3 * 4);;\n\
    \    let (c, d) = (a + b, b - a);;\n\
    \    let () = print_int c;;\n\
    \    let () = print_int d";
  [%expect {|
    15
    9|}]
;;

let%expect_test "test_closure" =
  test_interpret
    "let x = \n\
    \      let y = \n\
    \        let z = \n\
    \          let w = 1\n\
    \          in w\n\
    \        in z\n\
    \      in y\n\
    \    \n\
    \    let () = print_int x";
  [%expect {|1|}]
;;

let%expect_test "test_and" =
  test_interpret
    "let rec fac n = if n < 2 then 1 else fac(n-1) * n and fibo n = if n < 2 then 1 else \
     fibo(n-1) + fibo(n-2);; print_int (fac 5 + fibo 5)";
  [%expect {|128|}]
;;

let%expect_test "test_div_error" =
  test_interpret "let div = fun x y -> x / y;; let res = div 10 0";
  [%expect {|Interpreter error: DivisionByZeroError|}]
;;

let%expect_test "test_div_error" =
  test_interpret "let div = fun x y -> x / y;; let res = div 10 0";
  [%expect {|Interpreter error: DivisionByZeroError|}]
;;

let%expect_test "test_pm_error" =
  test_interpret "let (a, b) = (1,2,3)";
  [%expect {|Interpreter error: PatternMatchingError|}]
;;

let%expect_test "test_pm_error" =
  test_interpret "let x = x + 1";
  [%expect {|Interpreter error: UnboundVariable: "x"|}]
;;
