(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Inferencer
open Stdlib.Format

let test_inference str =
  match Parser.parse_program str with
  | Ok parsed ->
    (match inference_program parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(Scheme (_, typ)) ->
         if String.equal key "print_int" || String.equal key "print_endline"
         then ()
         else Stdlib.Format.printf "val %s : %a\n" key Forest.TypesTree.pp_typ typ)
     | Error err -> printf "Infer error: %a\n" Forest.TypesTree.pp_error err)
  | Error err -> printf "Parsing error: %s\n" err
;;

let%expect_test "inference_arithmetic" =
  test_inference "let cat = 1 + 2 * 3 / 10";
  [%expect {|
    val cat : int |}]
;;

let%expect_test "inference_fun_with_argument" =
  test_inference {| let foo x = x + 100 |};
  [%expect {|
    val foo : int -> int |}]
;;

let%expect_test "inference_rec_fun_with_argument" =
  test_inference {| let rec foo x = foo 5 - 1 |};
  [%expect {|
    val foo : int -> int |}]
;;

let%expect_test "inference_fun_with_nesting" =
  test_inference {| let add_one x = let double y = y * 2 in double (x + 1) |};
  [%expect {| val add_one : int -> int |}]
;;

let%expect_test "inference_polymorphic_fun" =
  test_inference "let identity x = x";
  [%expect {| val identity : '0 -> '0 |}]
;;

let%expect_test "inference_fun_with_tuple_argument" =
  test_inference {| let sum (x, y) = x + y|};
  [%expect {| val sum : (int * int) -> int |}]
;;

let%expect_test "inference_unbound_variable" =
  test_inference {| let foo x = x + a |};
  [%expect {|
    Infer error: Unbound variable: a |}]
;;

let%expect_test "inference_many_fun" =
  test_inference {| let a = fun x y -> fun z -> fun w -> fun c -> x + y + z + w+ c |};
  [%expect {|
val a : int -> int -> int -> int -> int -> int |}]
;;

let%expect_test "inference_unit" =
  test_inference {| let x = () |};
  [%expect {|
    val x : unit |}]
;;

let%expect_test "inference_tuple" =
  test_inference {| let meow = 5;; let x = (1, "a", meow ) |};
  [%expect {|
    val meow : int
    val x : (int * string * int) |}]
;;

let%expect_test "inference_combined_type" =
  test_inference {| let foo x = if x then [Some x; Some x] else [None; None]|};
  [%expect {|
    val foo : bool -> bool option list |}]
;;
