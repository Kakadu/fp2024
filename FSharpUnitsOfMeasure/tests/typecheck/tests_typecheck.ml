(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Typecheck.Inference
open Typecheck.Unification
open Typecheck.Types
open Parse.Expressions

let builtins =
  [ "=", Scheme ([ 0 ], TFun (TVar 0, TFun (TVar 0, TBool)))
  ; "+", Scheme ([], TFun (TInt, TFun (TInt, TInt)))
  ; "+.", Scheme ([], TFun (TFloat, TFun (TFloat, TFloat)))
  ; "-", Scheme ([], TFun (TInt, TFun (TInt, TInt)))
  ; "-.", Scheme ([], TFun (TFloat, TFun (TFloat, TFloat)))
  ; "*", Scheme ([], TFun (TInt, TFun (TInt, TInt)))
  ; "*.", Scheme ([], TFun (TFloat, TFun (TFloat, TFloat)))
  ; "/", Scheme ([], TFun (TInt, TFun (TInt, TInt)))
  ; "/.", Scheme ([], TFun (TFloat, TFun (TFloat, TFloat)))
  ; "<", Scheme ([], TFun (TInt, TFun (TInt, TBool)))
  ; "<=", Scheme ([], TFun (TInt, TFun (TInt, TBool)))
  ; ">", Scheme ([], TFun (TInt, TFun (TInt, TBool)))
  ; ">=", Scheme ([], TFun (TInt, TFun (TInt, TBool)))
  ; "<>", Scheme ([], TFun (TInt, TFun (TInt, TBool)))
  ]
;;

let test_infer s =
  let open Format in
  let env = builtins @ initial_env in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All pexpr s with
  | Ok parsed ->
    print_endline (try
    let _, ty = (infer env parsed) in
    string_of_ty ty
    with
  | TypeError s -> s
  | UnificationError s -> s)
  | Error e -> printf "Parse error: %s\n" e
;;

let%expect_test _ =
  test_infer "fun x -> x";
  [%expect {|
    'a0 -> 'a0
  |}]
;;

let%expect_test _ =
  test_infer "fun x -> 1";
  [%expect {|
    'a0 -> int
  |}]
;;

let%expect_test _ =
  test_infer "fun x y -> x" ;
  [%expect {| 'a0 -> 'a1 -> 'a0 |}]

let%expect_test _ =
  test_infer "let rec fact n = if n <= 1 then 1 else n * fact (n-1) in fact";
  [%expect {|
    int -> int
  |}]
;;

let%expect_test _ =
  test_infer "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib";
  [%expect {| int -> int
    |}]
;;

let%expect_test _ =
  test_infer "5.0<cm>";
  [%expect {|
    float<cm>
  |}]
;;

let%expect_test "List literal" =
  test_infer "[1; 2; 3]";
  [%expect {|
    int list
  |}]
;;

let%expect_test _ =
  test_infer "let x = 1 and y = 2 in x";
  [%expect {|
    int
  |}]
;;

let%expect_test _ =
  test_infer "fun (x : int) -> x + 1";
  [%expect {|
  int -> int
|}]
;;

let%expect_test _ =
  test_infer
  {| fun xs ->
       match xs with
       | [] -> 0
       | [x] -> x
       | [x; y] -> x + y |};
  [%expect {|
      int list -> int
    |}]
;;

let%expect_test _ =
  test_infer "let x = 5<cm> in x + 3";
  [%expect{| Cannot unify types: int and int<cm> |}]
;;

let%expect_test _ =
  test_infer "let rec sumList = fun xs -> match xs with [] -> 0 | [x] -> x | [x; y] -> x + y in sumList";
  [%expect {| int list -> int |}]
;;
