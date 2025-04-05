(** Copyright 2024-2025, Ram Prosad Chandra Sutra Dhar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EChandraSutraDhar_lib
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

let%expect_test "integer literals" =
  test_interpret "let() = print_int(42)";
  [%expect {| 42 |}];
  test_interpret "let() = print_int(-17)";
  [%expect {| -17 |}]
;;

let%expect_test "boolean literals" =
  test_interpret "let() = print_bool(true)";
  [%expect {| true |}];
  test_interpret "let() = print_bool(false)";
  [%expect {| false |}]
;;

let%expect_test "basic arithmetic" =
  test_interpret "let() = print_int(1 + 2 * 3)";
  [%expect {| 7 |}];
  test_interpret "let() = print_int((1 + 2) * 3)";
  [%expect {| 9 |}];
  test_interpret "let() = print_int(10 / 2)";
  [%expect {| 5 |}]
;;

let%expect_test "division by zero" =
  test_interpret "1 / 0";
  [%expect {| Interpreter error: DivisionByZeroError |}]
;;

let%expect_test "if expressions" =
  test_interpret "let () = print_int(if true then 1 else 2)";
  [%expect {| 1 |}];
  test_interpret "let () = print_int(if false then 1 else 2)";
  [%expect {| 2 |}]
;;

let%expect_test "logical operators" =
  test_interpret "let () = print_bool(true && false)";
  [%expect {| false |}];
  test_interpret "print_bool(true || false)";
  [%expect {| true |}];
  test_interpret "print_bool(not true)";
  [%expect {| false |}]
;;

let%expect_test "pattern matching" =
  test_interpret "match None with Some x -> x";
  [%expect {| Parsing error: : end_of_input |}]
;;

let%expect_test "test_unit" =
  test_interpret "let () = print_int(10 / 10 + 2 * 50 + 89 - 89)";
  [%expect {|101|}]
;;

let%expect_test "test_adder" =
  test_interpret
    "let create_adder x =\n\
    \                  let adder y = x + y in\n\
    \                  adder\n\
    \                  let sum_two_arg = print_int(create_adder 10 20)";
  [%expect {|30|}]
;;

let%expect_test "test_lambda" =
  test_interpret
    "let create_adder = fun x -> fun y -> x + y\n\
    \     let () = print_int(create_adder 7 8)";
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
    \                  adder\n\
    \                  let fac n = if n < 2 then 1 else n * fac(n-1) \n\
    \                      let x = 1\n\
    \                      let y = true";
  [%expect {||}]
;;

let%expect_test "test_factorial" =
  test_interpret
    "let rec fac n = if n < 2 then 1 else n * fac(n-1) \n\
    \     let result = print_int(fac 5)";
  [%expect {|120|}]
;;

let%expect_test "test_factorial_cps" =
  test_interpret
    "let rec fac_cps n k =\n\
    \                  if n=1 then k 1 else\n\
    \                  fac_cps (n-1) (fun p -> k (p*n))\n\
    \                  let result = print_int(fac_cps 5 (fun x -> x))";
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
    \    let inner = outer 10\n\
    \    let () = print_int (inner 5)";
  [%expect {|15|}]
;;

let%expect_test "test_annotate_sum" =
  test_interpret "let sum (x : int) (y : int) = x + y let res = print_int(sum 10 20)";
  [%expect {|30|}]
;;

let%expect_test "test_annotate_fac" =
  test_interpret
    "let rec fac (n : int) (acc : int) = if n < 2 then acc else fac (n-1) (acc * n)\n\
    \                  let res = print_int (fac 5 1)";
  [%expect {|120|}]
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

let%expect_test "test_div_error" =
  test_interpret "let div = fun x y -> x / y\n                  let res = div 10 0";
  [%expect {|Interpreter error: DivisionByZeroError|}]
;;

let%expect_test "test_pm_error" =
  test_interpret "let x = x + 1";
  [%expect {|Interpreter error: UnboundVariable: "x"|}]
;;

let%expect_test "test_type_error_addition" =
  test_interpret "let x = 10 + true";
  [%expect {|Interpreter error: TypeError|}]
;;
