(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib
open Inferencer
open Parser
open Printf
open Ast

let parse_test input =
  match parse input with
  | Ok ast -> printf "%s\n" (show_program ast)
  | Error fail -> printf "%s\n" fail
;;

let pretty_printer_parse_and_infer s =
  match Parser.parse s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       let filtered_env =
         Base.Map.filter_keys env ~f:(fun key ->
           not (List.mem key [ "print_int"; "print_endline"; "print_bool" ]))
       in
       Base.Map.iteri filtered_env ~f:(fun ~key:_ ~data:(S (_, ty)) ->
         Format.printf "%a\n" pp_ty ty)
     | Error e -> Format.printf "Infer error. %a\n" pp_error e)
  | Error e -> Format.printf "Parsing error. %s\n" e
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
  [%expect {|a -> b -> c -> d|}]
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
  [%expect {|Infer error. Unbound variable 'x'.|}]
;;

let%expect_test "test_annotate" =
  pretty_printer_parse_and_infer "let sum (x : int) (y : int) = x + y";
  [%expect {|int -> int -> int|}]
;;

let%expect_test "test_annotate_fac" =
  pretty_printer_parse_and_infer
    "let rec fac (n : int) (acc : int) = if n < 2 then acc else fac (n-1) (acc * n);;";
  [%expect {|int -> int -> int|}]
;;

let%expect_test "test_program_1" =
  pretty_printer_parse_and_infer
    "let div = fun x y -> x / y;; let sum = fun x y -> x + y;; let res = fun x y z ->  \
     div x (sum y z)";
  [%expect {|
    a -> b -> int
    e -> f -> g -> int
    c -> d -> int|}]
;;

let%expect_test "test_program_2" =
  pretty_printer_parse_and_infer "let square = fun x -> x * x ;; let result = square 10";
  [%expect {|
    int
    a -> int|}]
;;

let%expect_test "test_annotate_error" =
  pretty_printer_parse_and_infer "let sum (x : int) (y : string) = x + y";
  [%expect {|Infer error. Failed to unify types: string and int.|}]
;;

let%expect_test "test_unification_types" =
  pretty_printer_parse_and_infer "fun x -> x + true";
  [%expect {|Infer error. Failed to unify types: bool and int.|}]
;;
