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
  [%expect {| Interpreter error: Pattern-matching failure |}]
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

(* в OCaml также пишет 'Unbound variable y' *)
let%expect_test "from andrei" =
  let _ = test_interpret {| 
      let f () = y in let y = 42 in f ()
  |} in
  [%expect {| Infer error: Undefined variable "y" |}]
;;

let%expect_test "interpret expr with unary and binary operations" =
  let _ =
    test_interpret
      {| 
    let rez = 
      let x = not true in 
      let y = 13 in if x || (10 >= y) && (5 <= y) && (y <> 6) || (y < 9) && (y > -1000) then +5 :: [] else [10] |}
  in
  [%expect {|
    {
    val rez : int list = [10]
    } |}]
;;

let%expect_test "interpret expr with lists' comparison" =
  let _ =
    test_interpret
      {| 
    let a = Some 4
    let b = (a, [], None)
    let c = [1; 2; 3]
    let d = if (c = [3; 2; 1]) then a else Some 5
    let e = if (c = [1; 2; 3]) then a else Some 6
     |}
  in
  [%expect
    {|
    {
    val a : (int) option = Some 4
    val b : ((int) option * '0 list * ('1) option) = (Some 4, [], None)
    val c : int list = [1; 2; 3]
    val d : (int) option = Some 5
    val e : (int) option = Some 4
    } |}]
;;

let%expect_test "interpret expr with None match" =
  let _ =
    test_interpret
      {| 
    let a = Some 4 
    let _ = match a with
    | Some e -> print_int e
    | None -> print_endline "None" 
     |}
  in
  [%expect {|
    4
    {
    val a : (int) option = Some 4
    } |}]
;;

let%expect_test "interpret expr with None match" =
  let _ =
    test_interpret
      {| 
    let x = function | [] -> 10 | h::m::tl -> 30 | _ -> 20
    let y = x [1; 2; 3]
     |}
  in
  [%expect {|
    {
    val x : '2 list -> int = <fun>
    val y : int = 30
    } |}]
;;
