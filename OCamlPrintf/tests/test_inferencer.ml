(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Parser
open Ocaml_printf_lib.Pprinter
open Ocaml_printf_lib.Inferencer

let run str =
  match parse str with
  | Ok ast ->
    (match run_inferencer ast empty_env with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(Scheme (_, ty)) ->
         Format.printf "val %s : %a\n" key pp_core_type ty)
     | Error e -> Format.printf "Infer error: %a\n" pp_error e)
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "type check undefined variable" =
  run {| b |};
  [%expect {| Infer error: Undefined variable 'b' |}]
;;

let%expect_test "type check definition tuple" =
  run {| let (a,b) = (1,2);; |};
  [%expect {|
    val a : int
    val b : int |}]
;;

let%expect_test "type check definition variable" =
  run {| let a = 5 |};
  [%expect {| val a : int |}]
;;

let%expect_test "type check several definition variable" =
  run {| let f = 1 and r = "qwe";; let q = 2 |};
  [%expect {|
    val f : int
    val q : int
    val r : string
  |}]
;;

let%expect_test "type check several recursive definition" =
  run {| let rec f1 a = a + 1 and f2 b = f1 b;; |};
  [%expect {|
    val f1 : int -> int
    val f2 : int -> int |}]
;;

let%expect_test "type check definition function" =
  run {| let f a b c = if a then b else c |};
  [%expect {|
    val f : bool -> 'c -> 'c -> 'c
  |}]
;;

let%expect_test "type check definition construct" =
  run {|
  let (a :: b :: []) = [ 1; 2 ]
    |};
  [%expect {|
    val a : int
    val b : int
  |}]
;;

let%expect_test "type check recursive let expression" =
  run
    {| 
    let prime n =
      let rec check_zero x d =
        match d with
        | 1 -> true
        | _ -> x + d <> 0 && check_zero x (d - 1)
      in
      match n with
      | 0 -> false
      | 1 -> false
      | _ -> check_zero n (n - 1)
    ;;
    |};
  [%expect {|
    val prime : int -> bool
  |}]
;;

let%expect_test "type check of operators" =
  run {| let f x y z = if x + 1 = 0 && y = 1 || z >= 'w' then 2 else 26;; |};
  [%expect {|
    val f : int -> int -> char -> int
  |}]
;;

let%expect_test "type check pattern matching" =
  run {| let f a b = match a b with 1 -> 'q' | 2 -> 'w' | _ -> 'e' |};
  [%expect {|
    val f : ('b -> int) -> 'b -> char
  |}]
;;

let%expect_test "type check of expression list" =
  run {| let f a = [a; true] |};
  [%expect {|
    val f : bool -> bool list
  |}]
;;

let%expect_test "type check invalid expression list" =
  run {| let f a = [true; a; 2] |};
  [%expect {|
    Infer error: unification failed on bool and int
  |}]
;;

let%expect_test "type check pattern and expression list construct" =
  run
    {|
    let f p =
      let list = 1 :: 2 :: p in
      match list with
      | 1 :: 2 :: [ 3; 4 ] -> true
      | [ 1; 2 ] -> true
      | _ -> false
  |};
  [%expect {| val f : int list -> bool |}]
;;

let%expect_test "type check pattern-matching" =
  run
    {| 
    let fmap f xs =
      match xs with
      | a :: [] -> [ f a ]
      | a :: b :: [] -> [ f a; f b ]
      | a :: b :: c :: [] -> [ f a; f b; f c ]
      | _ -> []
;;
   |};
  [%expect {|
    val fmap : ('d -> 'w) -> 'd list -> 'w list
  |}]
;;

let%expect_test "type check of pattern list" =
  run {| let f a = match a with | [q; 1] -> q | [w; _] -> w |};
  [%expect {|
    val f : int list -> int
  |}]
;;

let%expect_test "type check Some and None" =
  run {| 
  let f a =
    match a with
    | Some _ -> Some 'a'
    | None -> None
;;
  |};
  [%expect {| val f : 'c option -> char option |}]
;;

let%expect_test "type check definition function" =
  run {|
  let f = function
    | Some a -> a
    | None -> false 
    |};
  [%expect {|
    val f : bool option -> bool
  |}]
;;

let%expect_test "type check expression constraint" =
  run {| let f a b = (b a : int) |};
  [%expect {|
    val f : 'a -> ('a -> int) -> int
  |}]
;;

let%expect_test "type check pattern constraint" =
  run {| let f (q : int -> 'a option) (x : int) = q x |};
  [%expect {|
    val f : (int -> 'a option) -> int -> 'a option
  |}]
;;

let%expect_test "type check recursive struct value" =
  run
    {| 
    let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1) 
    and strange_factorial k = if k <= 1 then 1 else k + strange_factorial (k - 1)
    |};
  [%expect {|
    val factorial : int -> int
    val strange_factorial : int -> int
  |}]
;;

let%expect_test "type check polymorphism" =
  run
    {| 
    let rec f1 x = x;;
    let foo1 = f1 1;;
    let foo2 = f1 'a';;
    let foo3 = f1 foo1;;

    let f2 x = x;;
    let foo4 = f2 1;;
    let foo5 = f2 'a';;
    let foo6 = f2 foo5;;
    |};
  [%expect
    {|
    val f1 : 'b -> 'b
    val f2 : 'l -> 'l
    val foo1 : int
    val foo2 : char
    val foo3 : int
    val foo4 : int
    val foo5 : char
    val foo6 : char
  |}]
;;
