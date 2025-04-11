(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

let test_interpret str =
  let open Stdlib.Format in
  match Parser.parse_program str with
  | Ok parsed ->
    (match Interpreter.interpret_program parsed with
     | Ok _ -> ()
     | Error err -> printf "Interpreter error: %a\n" Forest.ValuesTree.pp_error err)
  | Error err -> printf "Parsing error: %s\n" err
;;

let%expect_test "test_unit" =
  test_interpret "let () = print_int(10 / 10 + 2 * 50 + 89 - 89)";
  [%expect {|101|}]
;;

let%expect_test "test_bool" =
  test_interpret
    "let () = print(true) in\n\
    \                  let () = print(false) in\n\
    \                  let () = print(not true) in \n\
    \                  let () = print(not false) in\n\
    \                  let () = print(true && false) in\n\
    \                  let () = print(true || false ) in 9";
  [%expect {|
    Parsing error: : end_of|}]
;;

let%expect_test "test_bin_oper" =
  test_interpret
    "let a = 1\n\
    \                  let b = 2\n\
    \                  let () = print(a = a)\n\
    \                  let () = print(b > a)\n\
    \                  let () = print(a < b)\n\
    \                  let () = print(a <> b)\n\
    \                  let () = print(a <> a)\n\
    \                  let () = print(a <= a)\n\
    \                  let () = print(a >= a)\n\
    \                  let () = print(a <= b)\n\
    \                  let () = print(a >= b)";
  [%expect {|
    Interpreter error: No variable wite "print_bool"|}]
;;

let%expect_test "test_adder" =
  test_interpret
    "let create_adder x =\n\
    \ let adder y = x + y in\n\
    \ adder\n\
    \ let sum_two_arg = print_int(create_adder 10 20)";
  [%expect {|30|}]
;;

let%expect_test "test_lambda" =
  test_interpret
    "let create_adder = fun x -> fun y -> x + y\n\
    \     let () = print_int(create_adder 7 8)";
  [%expect {|15|}]
;;

let%expect_test "test_factorial" =
  test_interpret
    "let rec fac n = if n < 2 then 1 else n * fac(n-1) \n\
    \     let result = print_int(fac 5)";
  [%expect {|120|}]
;;
