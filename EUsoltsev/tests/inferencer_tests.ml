(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib
open Inferencer
open Ast

let pretty_printer_parse_and_infer s =
  match Parser.parse s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       let filtered_env =
         Base.Map.filter_keys env ~f:(fun key ->
           not (List.mem key [ "print_int"; "print_endline"; "print_bool" ]))
       in
       Base.Map.iteri filtered_env ~f:(fun ~key ~data:(S (_, ty)) ->
         Format.printf "val %s: %a\n" key pp_ty ty)
     | Error e -> Format.printf "Infer error. %a\n" pp_error e)
  | Error e -> Format.printf "Parsing error. %s\n" e
;;

let pretty_printer_parse_and_infer_simple s =
  match Parser.parse s with
  | Ok parsed ->
    (match parsed with
     | [ SEval expr ] ->
       (match infer_simple_expression expr with
        | Ok ty -> Format.printf "%a\n" pp_ty ty
        | Error e -> Format.printf "Infer error. %a\n" pp_error e)
     | _ ->
       Format.printf
         "Expected a single expression, but got a program with multiple structures.\n")
  | Error e -> Format.printf "Parsing error. %s\n" e
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer_simple "10/2 + 56*2 - 10 / 10 / 20 + 666 - 777 + 1";
  [%expect {|int|}]
;;

let%expect_test "test_bool" =
  pretty_printer_parse_and_infer_simple "false";
  [%expect {|bool|}]
;;

let%expect_test "test_string" =
  pretty_printer_parse_and_infer_simple "\"I like OCaml\" ";
  [%expect {|string|}]
;;

let%expect_test "test_option" =
  pretty_printer_parse_and_infer_simple "Some 10";
  [%expect {|int option|}]
;;

let%expect_test "test_binary_oper_and_arg" =
  pretty_printer_parse_and_infer_simple "fun x -> x * 69 + 100 - 201 / 777";
  [%expect {|int -> int|}]
;;

let%expect_test "test_rec" =
  pretty_printer_parse_and_infer "let rec func arg = func arg";
  [%expect {|val func: '1 -> '2|}]
;;

let%expect_test "test_func_apply_some_args" =
  pretty_printer_parse_and_infer "let func a1 a2 a3 = a1 a2 a3";
  [%expect {|val func: ('1 -> '2 -> '4) -> '1 -> '2 -> '4|}]
;;

let%expect_test "test_tuple" =
  pretty_printer_parse_and_infer_simple "fun x y z -> (x + 10, y / 2 , z)";
  [%expect {|int -> int -> '2 -> (int * int * '2)|}]
;;

let%expect_test "test_list" =
  pretty_printer_parse_and_infer "let arr = [1;2;3]";
  [%expect {|val arr: int list|}]
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer "let is_above_10 x = if x > 10 then true else false ";
  [%expect {|val is_above_10: int -> bool|}]
;;

let%expect_test "test_binary_oper" =
  pretty_printer_parse_and_infer "let is_above_10 x = x > 10";
  [%expect {|val is_above_10: int -> bool|}]
;;

let%expect_test "test_factorial" =
  pretty_printer_parse_and_infer "let rec fac n = if n < 2 then 1 else n * fac (n - 1)";
  [%expect {|val fac: int -> int|}]
;;

let%expect_test "test_nested_list_function" =
  pretty_printer_parse_and_infer "let f x = [ [x; x]; [x] ]";
  [%expect {|val f: '0 -> '0 list list|}]
;;

let%expect_test "test_nested_option_function" =
  pretty_printer_parse_and_infer "let f x = Some x";
  [%expect {|val f: '0 -> '0 option|}]
;;

let%expect_test "test_fibonacci" =
  pretty_printer_parse_and_infer
    "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2)";
  [%expect {|val fibo: int -> int|}]
;;

let%expect_test "test_unbound_var" =
  pretty_printer_parse_and_infer "let f = x";
  [%expect {|Infer error. Unbound variable 'x'.|}]
;;

let%expect_test "test_annotate" =
  pretty_printer_parse_and_infer "let sum = fun (x : int) (y : int) -> x + y";
  [%expect {|val sum: int -> int -> int|}]
;;

let%expect_test "test_annotate_fac" =
  pretty_printer_parse_and_infer
    "let rec fac = fun (n : int) (acc : int) -> if n < 2 then acc else fac (n-1) (acc * \
     n);;";
  [%expect {|val fac: int -> int -> int|}]
;;

let%expect_test "test_program_1" =
  pretty_printer_parse_and_infer
    "let div = fun x y -> x / y \n\
    \     let sum = fun x y -> x + y\n\
    \     let res = fun x y z -> div x (sum y z)";
  [%expect
    {|
    val div: int -> int -> int
    val res: int -> int -> int -> int
    val sum: int -> int -> int|}]
;;

let%expect_test "test_program_2" =
  pretty_printer_parse_and_infer
    "let square = fun x -> x * x\n\
    \                                  let result = square 10";
  [%expect {|
    val result: int
    val square: int -> int|}]
;;

let%expect_test "test_annotate_error" =
  pretty_printer_parse_and_infer "let sum (x : int) (y : string) = x + y";
  [%expect {|Infer error. Failed to unify types: string and int.|}]
;;

let%expect_test "test_unification_types" =
  pretty_printer_parse_and_infer "fun x -> x + true";
  [%expect {|Infer error. Failed to unify types: bool and int.|}]
;;

let%expect_test "test_option_type_error" =
  pretty_printer_parse_and_infer
    "let f x = Some (x + 1) in let g y = Some (y && true) in f = g";
  [%expect {|Infer error. Failed to unify types: bool and int.|}]
;;
