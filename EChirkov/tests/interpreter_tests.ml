(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Interpreter

let test_interpreter s =
  match parse s with
  | Ok s ->
    (* print_endline (show_program s); *)
    (match interpret s with
     | Ok _ -> ()
     | Error e -> print_endline ("Interpretation error: " ^ pp_error e))
  | Error _ -> print_endline "Parsing error"
;;

let%expect_test "interpret factorial function" =
  test_interpreter
    "let () = let rec factorial n = if n < 2 then 1 else n * factorial(n - 1) in \
     print_int (factorial 17)";
  [%expect {|355687428096000|}]
;;

(* ========== const ========== *)

let%expect_test "interpret int" =
  test_interpreter "let () = print_int 23";
  [%expect {|23|}]
;;

(* ========== bop ========== *)

let%expect_test "interpret addition" =
  test_interpreter "let () = print_int (23 + 23)";
  [%expect {|
    46|}]
;;

let%expect_test "interpret subtraction" =
  test_interpreter "let () = print_int (23 - 21)";
  [%expect {|
    2|}]
;;

let%expect_test "interpret multiplication" =
  test_interpreter "let () = print_int (123 * 23)";
  [%expect {|
    2829|}]
;;

let%expect_test "interpret division whole" =
  test_interpreter "let () = print_int (240 / 4)";
  [%expect {|
    60|}]
;;

let%expect_test "interpret division" =
  test_interpreter "let () = print_int (123 / 23)";
  [%expect {|
    5|}]
;;

let%expect_test "interpret addition multiplication division" =
  test_interpreter "let () = print_int (6 / 2 * (1 + 2))";
  [%expect {|9|}]
;;

(* ========== unop ========== *)

let%expect_test "interpret neg" =
  test_interpreter "let () = print_int (-1)";
  [%expect {|
    -1|}]
;;

let%expect_test "interpret neg" =
  test_interpreter "let () = print_int -1";
  [%expect {|
    Interpretation error: Type error|}]
;;

let%expect_test "interpret neg add" =
  test_interpreter "let () = print_int ((-500) + (+100))";
  [%expect {|
    -400|}]
;;

let%expect_test "interpret neg add no parentheses" =
  test_interpreter "let () = print_int (-500 + +100)";
  [%expect {|
    -400|}]
;;

(* ========== tuples ========== *)

let%expect_test "interpret tuple fst" =
  test_interpreter "let fst (x, y) = x let () = print_int (fst (23, 34))";
  [%expect {|
    23|}]
;;

let%expect_test "interpret tuple multiple patterns" =
  test_interpreter
    {|
    let () =
    let a, b = 23, 34 in
    let c, d = a, b in
    let _ = print_int a in
    let _ = print_int b in
    let _ = print_int c in
    print_int d
  |};
  [%expect {|
    23
    34
    23
    34|}]
;;

(* ========== list ========== *)

let%expect_test "interpret list empty" =
  test_interpreter "let () = print_int (if [] = [] then 1 else 0)";
  [%expect {|
    1|}]
;;

let%expect_test "interpret list 1" =
  test_interpreter "let () = let [a] = [23] in print_int a";
  [%expect {|
    23|}]
;;

(* ========== option ========== *)

let%expect_test "interpret var simple" =
  test_interpreter
    {|
    let x = None 
    let y = Some 23
    let z = Some 34
    let w = Some 23
    let () = print_int (if y = z then 1 else 0)
    let () = print_int (if x = y then 1 else 0)
    let () = print_int (if y = w then 1 else 0)
    let () = print_int (if x = w then 1 else 0)
  |};
  [%expect {|
    0
    0
    1
    0|}]
;;

(* ========== vars ========== *)

let%expect_test "interpret var simple" =
  test_interpreter "let asd = 23 let () = print_int asd";
  [%expect {|
    23|}]
;;

(* ========== fun ========== *)

let%expect_test "interpret fun list" =
  test_interpreter
    {|
  let empty = fun f -> fun x -> x
  let cons x xs = fun f -> fun x0 -> xs f (f x x0)
  let print_list l = l (fun x rest -> let _ = print_int x in rest) ()
  let my_list = cons 1 (cons 2 (cons 3 empty))
  let () = print_list my_list
  |};
  [%expect {|
    1
    2
    3|}]
;;

(* ========== let ========== *)

let%expect_test "interpret fun simple" =
  test_interpreter "let () = let x = 5 in let y = 2 in print_int (x + y)";
  [%expect {|
    7|}]
;;
