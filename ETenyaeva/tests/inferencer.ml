(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ETenyaeva_lib.Parser
open ETenyaeva_lib.Ast
open ETenyaeva_lib.Inferencer

let run input =
  match parse input with
  | Ok ast -> 
    (match run_inferencer empty_env ast with
     | Ok (_, out_list) ->
       List.iter
         (function
           | Some id, type' -> Format.printf "val %s : %a\n" id pp_type type'
           | None, type' -> Format.printf "- : %a\n" pp_type type')
         out_list
     | Error e -> Format.printf "Inferencer error: %a\n" pp_error e)
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "parsing error" =
  run {|
  let a = ;;
  |};
  [%expect {|
  Parsing error
  |}]
;;

let%expect_test "const" =
  run {|
  1;;
  |};
  [%expect {|
  - : int
  |}]
;;

let%expect_test "const list" =
  run {|
  [1; 2; 3; 4];;
  |};
  [%expect {|
  - : int list
  |}]
;;

let%expect_test "binary oper with const" =
  run {|
  1 + 3 - 400 / 3 * 2;;
  |};
  [%expect {|
  - : int
  |}]
;;

let%expect_test "unary oper with const" =
  run {|
  not false;; -2
  |};
  [%expect {|
  - : bool
  - : int
  |}]
;;