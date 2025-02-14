(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Interpreter

let test_interpret s =
  let open Stdlib.Format in
  match Parser.parse_expr s with
  | Ok parsed ->
    (match Inferencer.Infer.infer_program parsed with
     | Ok env_inf ->
       (match Interpreter.eval_structure parsed with
        | Ok env_int -> pp_env env_inf env_int
        | Error e -> printf "Interpreter error: %a\n" Values.pp_error e)
     | Error e -> printf "Infer error: %a\n" Typedtree.pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;

let%expect_test "interpret pattern-matching" =
  let _ =
    test_interpret
      {| 
    let a x = match x with 1 -> true | _ -> false
    let b = a 3
  |}
  in
  [%expect {|
    {
    val a : int -> bool = <fun>
    val b : bool = false
    } |}]
;;

let%expect_test "interpet non-exhaustive pattern-matching" =
  let _ = test_interpret {| 
    let (0, b) = (4, 3)
  |} in
  [%expect
    {| Interpreter error: Ill left-hand side Pattern not acceptable for variable name |}]
;;

let%expect_test "interpret tuple pattern" =
  let _ = test_interpret {| 
    let (a, b) = (4, 3)
  |} in
  [%expect {|
    {
    val a : int = 4
    val b : int = 3
    } |}]
;;

let%expect_test "interpret simple pattern-matching" =
  let _ =
    test_interpret
      {| 
      let f = true
      let g = 
        match f with
        | true -> true
        | false -> false
      let n = not g
  |}
  in
  [%expect
    {|
    {
    val f : bool = true
    val g : bool = true
    val n : bool = false
    } |}]
;;

let%expect_test "interpret non-exhaustive match" =
  let _ =
    test_interpret
      {| 
      let f x = 
        (match x with
        | [] -> ""
        | hd :: snd :: tl -> hd)
      in
      f ["oops"]
  |}
  in
  [%expect {| Interpreter error: Pattern-matching failure |}]
;;

let%expect_test "interpret correct match" =
  let _ =
    test_interpret
      {| 
      let f x = 
        (match x with
        | [] -> 0
        | h::tl -> 1
        | hd :: snd :: tl -> hd)
      in
      print_int (f [1])
  |}
  in
  [%expect {|
    1
    {
    } |}]
;;

let%expect_test "interpret values using cons" =
  let _ =
    test_interpret
      {| 
      let a = 1 :: 2 :: 3 :: []
      let b = (1, "one") :: (2, "two") :: [(3, "three")]
      let c = [1; 2] :: [3; 4] :: []
  |}
  in
  [%expect
    {|
    {
    val a : int list = [1; 2; 3]
    val b : (int * string) list = [(1, "one"); (2, "two"); (3, "three")]
    val c : int list list = [[1; 2]; [3; 4]]
    } |}]
;;

let%expect_test "interpret mutual recursion" =
  let _ =
    test_interpret
      {| 
      let rec is_even n =
        if n = 0 then true else is_odd (n - 1)
      and is_odd n =
        if n = 0 then false else is_even (n - 1)
      let a  = is_even 4
  |}
  in
  [%expect
    {|
    {
    val a : bool = true
    val is_even : int -> bool = <fun>
    val is_odd : int -> bool = <fun>
    } |}]
;;

let%expect_test "interpret simple function with fun" =
  let _ = test_interpret {| 
      let f = fun x -> x + 3
      let a = f 3
  |} in
  [%expect {|
    {
    val a : int = 6
    val f : int -> int = <fun>
    } |}]
;;

let%expect_test "interpret simple function" =
  let _ = test_interpret {| 
      let f x y = x + y
      let a = f 3 4
  |} in
  [%expect {|
    {
    val a : int = 7
    val f : int -> (int -> int) = <fun>
    } |}]
;;

let%expect_test "interpret division by zero" =
  let _ = test_interpret {| 
      let a = 0
      let b = 30 / a
  |} in
  [%expect {| Interpreter error: Division by zero |}]
;;

let%expect_test "interpret multiple strucuture items" =
  let _ =
    test_interpret
      {| 
      let g m = m*m
      let f x = if x > 0 then h x else g x in
      f 5
  |}
  in
  [%expect {| Infer error: Undefined variable "h" |}]
;;
