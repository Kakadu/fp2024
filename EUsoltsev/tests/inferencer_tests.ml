(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib
open Inferencer
open Typing

let pretty_printer_infer e =
  match run_inference e with
  | Ok ty -> Stdlib.Format.printf "%a" pp_ty ty
  | Error err -> Stdlib.Format.printf "%a" pp_error err
;;

let pretty_printer_parse_and_infer input =
  match Parser.parse_string_expr input with
  | Ok e -> pretty_printer_infer e
  | Error _ -> Stdlib.print_endline "Failed to parse"
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer "10/2 + 56*2 - 10 / 10 / 20 + 666 - 777 + 1";
  [%expect {|int|}]
;;

let%expect_test "test_bool" =
  pretty_printer_parse_and_infer "false";
  [%expect {|bool|}]
;;

let%expect_test "test_string" =
  pretty_printer_parse_and_infer "\"I like OCaml\" ";
  [%expect {|string|}]
;;

let%expect_test "test_option" =
  pretty_printer_parse_and_infer "Some 10";
  [%expect {|int option|}]
;;

let%expect_test "test_binary_oper_and_arg" =
  pretty_printer_parse_and_infer "fun x -> x * 69 + 100 - 201 / 777";
  [%expect {|a -> int|}]
;;

let%expect_test "test_binary_oper_and_arg" =
  pretty_printer_parse_and_infer "fun x -> x * 69 + 100 - 201 / 777";
  [%expect {|a -> int|}]
;;

let%expect_test "test_rec" =
  pretty_printer_parse_and_infer "let rec func arg = func arg";
  [%expect {|b -> c|}]
;;

let%expect_test "test_func_apply_some_args" =
  pretty_printer_parse_and_infer "let func a1 a2 a3 = a1 a2 a3";
  [%expect {|a -> b -> c -> e|}]
;;

let%expect_test "test_tuple" =
  pretty_printer_parse_and_infer "fun x y z -> (x + 10, y / 2 , z)";
  [%expect {|a -> b -> c -> (int * int * c)|}]
;;

let%expect_test "test_list" =
  pretty_printer_parse_and_infer "let arr = [1;2;3]";
  [%expect {|int list|}]
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer "let is_above_10 x = if x > 10 then true else false ";
  [%expect {|a -> bool|}]
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer "let is_above_10 x = x > 10";
  [%expect {|a -> bool|}]
;;

let%expect_test "test_factorial" =
  pretty_printer_parse_and_infer "let rec fac n = if n < 2 then 1 else n * fac (n - 1)";
  [%expect {|int -> int|}]
;;

let%expect_test "test_fibonacci" =
  pretty_printer_parse_and_infer
    "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2)";
  [%expect {|int -> int|}]
;;

let%expect_test "test_unbound_var" =
  pretty_printer_parse_and_infer "let f = x";
  [%expect {|Error: Unbound variable 'x'.|}]
;;

let%expect_test "test_unification_types" =
  pretty_printer_parse_and_infer "fun x -> x + true";
  [%expect {|Error: Failed to unify types: bool and int.|}]
;;
