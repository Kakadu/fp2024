(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let%expect_test "int type" =
  (match Haskell_lib.Parser.parse_line "a = 42" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "bool type" =
  (match Haskell_lib.Parser.parse_line "a = True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "unit type" =
  (match Haskell_lib.Parser.parse_line "a = ()" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error _ -> print_endline "failure")
   | _ -> ());
  [%expect {|
    [
    a:  ()
     ] |}]
;;

let%expect_test "const with explicit correct single type" =
  (match Haskell_lib.Parser.parse_line "a = 42 :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "const with explicit correct multiple type" =
  (match Haskell_lib.Parser.parse_line "a = (42 :: Int) :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "const with explicit wrong single type" =
  (match Haskell_lib.Parser.parse_line "a = 42 :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "const with explicit wrong multiple type" =
  (match Haskell_lib.Parser.parse_line "a = (42 :: Int) :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "tuple" =
  (match Haskell_lib.Parser.parse_line "a = (42, True)" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (Int, Bool)
     ] |}]
;;

let%expect_test "tuple with extra types" =
  (match Haskell_lib.Parser.parse_line "a = (42, True, ())" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (Int, Bool, ())
     ] |}]
;;

let%expect_test "tuple with explicit correct single type" =
  (match Haskell_lib.Parser.parse_line "a = (42, True, ()) :: (Int, Bool, ())" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (Int, Bool, ())
     ] |}]
;;

let%expect_test "tuple with explicit correct multiple type" =
  (match
     Haskell_lib.Parser.parse_line
       "a = ((42, True, ()) :: (Int, Bool, ())) :: (Int, Bool, ())"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (Int, Bool, ())
     ] |}]
;;

let%expect_test "tuple with explicit wrong single type" =
  (match Haskell_lib.Parser.parse_line "x = (42, True, ()) :: (Bool, Bool, ())" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "tuple with explicit wrong multiple type" =
  (match
     Haskell_lib.Parser.parse_line
       "x = ((42, True, ()) :: (Int, Bool, ())) :: (Bool, Bool, ())"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "maybe type just" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> Just x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  t2 -> Maybe t2
     ] |}]
;;

let%expect_test "maybe type just int" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> Just (x + 1)" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> Maybe Int
     ] |}]
;;

let%expect_test "maybe type just list" =
  (match Haskell_lib.Parser.parse_line "a = \\x y -> Just (y : x)" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [t5] -> t5 -> Maybe [t5]
     ] |}]
;;

let%expect_test "maybe type nothing" =
  (match Haskell_lib.Parser.parse_line "a = Nothing" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Maybe t2
     ] |}]
;;

let%expect_test "correct ariphmetic operation" =
  (match Haskell_lib.Parser.parse_line "a = 5 + 3" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "incorrect ariphmetic operation" =
  (match Haskell_lib.Parser.parse_line "a = 5 + ()" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on () and Int |}]
;;

let%expect_test "ariphmetic operation with explicit correct single type" =
  (match Haskell_lib.Parser.parse_line "a = (5 + 3) :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "ariphmetic operation with explicit correct multiple type" =
  (match Haskell_lib.Parser.parse_line "a = ((5 + 3) :: Int) :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "ariphmetic operation with explicit wrong single type" =
  (match Haskell_lib.Parser.parse_line "a = (5 + 3) :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "ariphmetic operation with explicit wrong multiple type" =
  (match Haskell_lib.Parser.parse_line "a = ((5 + 3) :: Int) :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "correct logical operation" =
  (match Haskell_lib.Parser.parse_line "a = True && False" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "incorrect logical operation" =
  (match Haskell_lib.Parser.parse_line "a = True && 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "logical operation with correct explicit single type" =
  (match Haskell_lib.Parser.parse_line "a = True && False :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "logical operation with correct explicit multiple type" =
  (match Haskell_lib.Parser.parse_line "a = (True && False :: Bool) :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "logical operation with incorrect explicit single type" =
  (match Haskell_lib.Parser.parse_line "a = True && False :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "logical operation with incorrect explicit multiple type" =
  (match Haskell_lib.Parser.parse_line "a = (True && False :: Bool) :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "correct comparison operation with int" =
  (match Haskell_lib.Parser.parse_line "a = 1 <= 2" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "correct comparison operation with bool" =
  (match Haskell_lib.Parser.parse_line "a = False <= True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "incorrect comparison operation with () and int" =
  (match Haskell_lib.Parser.parse_line "a = 1 <= ()" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on () and Int |}]
;;

let%expect_test "incorrect comparison operation with bool and int" =
  (match Haskell_lib.Parser.parse_line "a = 1 <= True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "comparison operation with explicit correct single type" =
  (match Haskell_lib.Parser.parse_line "a = (1 <= 2) :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "comparison operation with explicit correct multiple type" =
  (match Haskell_lib.Parser.parse_line "a = ((1 <= 2) :: Bool) :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "comparison operation with explicit wrong single type" =
  (match Haskell_lib.Parser.parse_line "a = (1 <= 2) :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "comparison operation with explicit wrong multiple type" =
  (match Haskell_lib.Parser.parse_line "a = ((1 <= 2) :: Int) :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "cons correct with int" =
  (match Haskell_lib.Parser.parse_line "a = 1 : []" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "cons correct with bool" =
  (match Haskell_lib.Parser.parse_line "a = True : []" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Bool]
     ] |}]
;;

let%expect_test "cons incorrect with int" =
  (match Haskell_lib.Parser.parse_line "a = 1 : 2" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and [Int] |}]
;;

let%expect_test "cons incorrect with bool" =
  (match Haskell_lib.Parser.parse_line "a = True : False" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and [Bool] |}]
;;

let%expect_test "cons incorrect with int and bool" =
  (match Haskell_lib.Parser.parse_line "a = 1 : [True]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "neg type correct" =
  (match Haskell_lib.Parser.parse_line "a = -42" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "neg type incorrect" =
  (match Haskell_lib.Parser.parse_line "a = -True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "neg type with explicit correct single type" =
  (match Haskell_lib.Parser.parse_line "a = -42 :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "neg type with explicit correct multiple type" =
  (match Haskell_lib.Parser.parse_line "a = (-42 :: Int) :: Int" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "neg type with explicit wrong single type" =
  (match Haskell_lib.Parser.parse_line "a = -42 :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "neg type with explicit wrong multiple type" =
  (match Haskell_lib.Parser.parse_line "a = (-42 :: Int) :: Bool" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "ord polymor" =
  (match
     Haskell_lib.Parser.parse_line "a = (\\f -> let g = (f True) in (f 3)) (\\x -> x)"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "if correct with int return type" =
  (match Haskell_lib.Parser.parse_line "a = if True then 1 else -1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "if correct with tuple return type" =
  (match Haskell_lib.Parser.parse_line "a = if True then (True, 2) else (False, -1)" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (Bool, Int)
     ] |}]
;;

let%expect_test "if incorrect with int condition" =
  (match
     Haskell_lib.Parser.parse_line "a = if (1 + 2) then (True, 2) else (False, -1)"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "if incorrect with tuple condition" =
  (match
     Haskell_lib.Parser.parse_line "a = if (True, ()) then (True, 2) else (False, -1)"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on (Bool, ()) and Bool |}]
;;

let%expect_test "if incorrect with int and bool return types" =
  (match Haskell_lib.Parser.parse_line "a = if True then 1 else False" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "if incorrect with int and tuple return types" =
  (match Haskell_lib.Parser.parse_line "a = if True then 1 else (1, False)" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and (Int, Bool) |}]
;;

let%expect_test "if incorrect with bool and list return types" =
  (match Haskell_lib.Parser.parse_line "a = if True then True else [1, 4]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and [Int] |}]
;;

let%expect_test "lambda ident" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  t2 -> t2
     ] |}]
;;

let%expect_test "lambda int return type" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  t2 -> Int
     ] |}]
;;

let%expect_test "lambda narrowing to int type" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> x + 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> Int
     ] |}]
;;

let%expect_test "lambda tuple return type" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> (x, ())" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  t2 -> (t2, ())
     ] |}]
;;

let%expect_test "lambda multiple arguments" =
  (match Haskell_lib.Parser.parse_line "a = \\x y z -> x + y + z" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> Int -> Int -> Int
     ] |}]
;;

let%expect_test "lambda narrowing to list type" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> 1 : x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int] -> [Int]
     ] |}]
;;

let%expect_test "lambda narrowing to arrow type" =
  (match Haskell_lib.Parser.parse_line "a = \\f -> \\y -> f y" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (t3 -> t4) -> t3 -> t4
     ] |}]
;;

let%expect_test "lambda occurs check" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> x x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| Occurs check failed |}]
;;

let%expect_test "lambda tuple return type" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> x `mod` 2 == 0 && x > 5" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> Bool
     ] |}]
;;

let%expect_test "lambda correct with explicit single type" =
  (match Haskell_lib.Parser.parse_line "a = (\\x -> 1) :: (Int -> Int)" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> Int
     ] |}]
;;

let%expect_test "lambda correct with explicit multiple type" =
  (match
     Haskell_lib.Parser.parse_line "a = ((\\x -> 1) :: (Bool -> Int)) :: (Bool -> Int)"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool -> Int
     ] |}]
;;

let%expect_test "lambda wrong with explicit single type" =
  (match Haskell_lib.Parser.parse_line "a = (\\x -> ()) :: (() -> Bool)" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on () and Bool |}]
;;

let%expect_test "lambda wrong with explicit multiple type" =
  (match
     Haskell_lib.Parser.parse_line "a = ((\\x -> ()) :: (() -> ())) :: (() -> [Int])"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on [Int] and () |}]
;;

let%expect_test "let id" =
  (match Haskell_lib.Parser.parse_line "a = let x = x in x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  t3
     ] |}]
;;

let%expect_test "let narrowing to int" =
  (match Haskell_lib.Parser.parse_line "a = let x = x; y = 1 in x + y" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "let narrowing to [int]" =
  (match Haskell_lib.Parser.parse_line "a = let x = x; y = 1 in y : x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "let narrowing to bool" =
  (match Haskell_lib.Parser.parse_line "a = let x = x; y = True in y && x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "let function" =
  (match Haskell_lib.Parser.parse_line "a = let compose f g x = f (g x) in compose" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (t6 -> t7) -> (t5 -> t6) -> t5 -> t7
     ] |}]
;;

let%expect_test "let recursive fib" =
  (match
     Haskell_lib.Parser.parse_line
       "a = let fib n = if (n == 0) then 0 else if (n==1) then 1 else ((fib (n-1)) + \
        (fib (n-2))) in fib"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> Int
     ] |}]
;;

let%expect_test "let recursive fac" =
  (match
     Haskell_lib.Parser.parse_line
       "a = let factorial = \\n -> if n == 0 then 1 else n * factorial (n - 1) in \
        factorial"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> Int
     ] |}]
;;

let%expect_test "let with explicit correct single type" =
  (match Haskell_lib.Parser.parse_line "a = let (x :: Int) = x in x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "let with explicit correct mutliple type" =
  (match Haskell_lib.Parser.parse_line "a = let ((x :: Int) :: Int) = x in x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "let with explicit wrong single type" =
  (match Haskell_lib.Parser.parse_line "a = let (x :: Bool) = 1 in x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "let with explicit wrong mutliple type" =
  (match Haskell_lib.Parser.parse_line "a = let ((x :: Int) :: Bool) = x in x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "let wrong unification" =
  (match
     Haskell_lib.Parser.parse_line "a = let x = if x <= True then 1 else 0 in x + 1"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "let wrong unification" =
  (match
     Haskell_lib.Parser.parse_line "a = let x = if x <= True then True else False in x"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "let wrong unification" =
  (match
     Haskell_lib.Parser.parse_line "a = let x = if x <= True then True else False in x"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "case correct with int type" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> case x of 1 -> True; 2 -> False" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> Bool
     ] |}]
;;

let%expect_test "case correct with lists" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> case x of (x:xs) -> x; [] -> []" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [[t7]] -> [t7]
     ] |}]
;;

let%expect_test "case correct with int lists and explicit similar types" =
  (match
     Haskell_lib.Parser.parse_line
       "a = \\x -> case x of ((x :: [Int]):(xs :: [[Int]])) -> x; [] -> []"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [[Int]] -> [Int]
     ] |}]
;;

let%expect_test "case incorrect with int lists and explicit different types" =
  (match
     Haskell_lib.Parser.parse_line
       "a = \\x -> case x of ((x :: [Int]):(xs :: [[Bool]])) -> x; [] -> []"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "function apply incorrect" =
  (match Haskell_lib.Parser.parse_line "a = (\\x -> x + 1) True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "function apply list return type" =
  (match Haskell_lib.Parser.parse_line "a = (\\x -> x : []) True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Bool]
     ] |}]
;;

let%expect_test "function apply with correct single type" =
  (match Haskell_lib.Parser.parse_line "a = (\\(x :: Int) -> x <= 2) 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "function apply return correct multiple type" =
  (match Haskell_lib.Parser.parse_line "a = (\\((x :: Int) :: Int) -> x <= 2) 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "function apply return wrong single type" =
  (match Haskell_lib.Parser.parse_line "a = (\\(x :: Bool) -> x <= 2) 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "function apply return wrong multiple type" =
  (match Haskell_lib.Parser.parse_line "a = (\\((x :: Int) :: Bool) -> x <= 2) 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "function apply return correct single type" =
  (match Haskell_lib.Parser.parse_line "a = (\\(x :: Int) -> x : []) 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "list int" =
  (match Haskell_lib.Parser.parse_line "a = [1, 2]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "lazy list int" =
  (match Haskell_lib.Parser.parse_line "a = [1..]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "lazy list wrong type" =
  (match Haskell_lib.Parser.parse_line "a = [(True, 1)..]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Enum t2 and (Bool, Int) |}]
;;

let%expect_test "list of list" =
  (match Haskell_lib.Parser.parse_line "a = [[True]]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [[Bool]]
     ] |}]
;;

let%expect_test "wrong list of different types" =
  (match Haskell_lib.Parser.parse_line "a = [True, (), 3]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and () |}]
;;

let%expect_test "comprehension list with generator" =
  (match Haskell_lib.Parser.parse_line "a = [x * y | x <- [1..10], y <- [1]]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "comprehension list with simple condition" =
  (match Haskell_lib.Parser.parse_line "a = [1 * 2 | True]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "comprehension list with condition" =
  (match Haskell_lib.Parser.parse_line "a = \\x -> [ x | x < 10 ]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> [Int]
     ] |}]
;;

let%expect_test "comprehension list with condition and generator" =
  (match
     Haskell_lib.Parser.parse_line "a = \\y -> [ x * y | x <- [1..10], y <= 10  ]"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  Int -> [Int]
     ] |}]
;;

let%expect_test "wrong comprehension list with generator condition" =
  (match
     Haskell_lib.Parser.parse_line "a = \\x y -> [ x * y | x < 10, y <- [True, False]]"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "several functions" =
  (match Haskell_lib.Parser.parse_line "f x = g x; g y = y" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
  [
  f:  t4 -> t4
  g:  t4 -> t4
   ] |}]
;;

let%expect_test "mutually recursive functions" =
  (match Haskell_lib.Parser.parse_line "f x = g x; g y = f y" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
  [
  f:  t4 -> t5
  g:  t4 -> t5
   ] |}]
;;

let%expect_test "mutually recursive functions with guards" =
  (match
     Haskell_lib.Parser.parse_line
       "isEven n | n == 0 = True | n > 0 = isOdd (n - 1) | True = isOdd (-n); isOdd n | \
        n == 0 = False | n > 0 = isEven (n - 1) | True = isEven (-n)"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
  [
  isEven:  Int -> Bool
  isOdd:  Int -> Bool
   ] |}]
;;

let%expect_test "guards" =
  (match Haskell_lib.Parser.parse_line "f x | x > 0 = x | True = -1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    f:  Int -> Int
     ] |}]
;;

let%expect_test "where single statement" =
  (match Haskell_lib.Parser.parse_line "f x = x + y where y = 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| 
    [
    f:  Int -> Int
     ] |}]
;;

let%expect_test "where single statement with explicit incorrect type" =
  (match Haskell_lib.Parser.parse_line "f x = x + y where (y :: Bool) = 1" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "unification failed on Bool and Int");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "where multiple statements" =
  (match Haskell_lib.Parser.parse_line "f x = x && y || z where y = False; z = True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| 
    [
    f:  Bool -> Bool
     ] |}]
;;

let%expect_test "where single statement incorrect" =
  (match Haskell_lib.Parser.parse_line "f x = x + y where y = True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "where single statement with param shadowing incorrect" =
  (match Haskell_lib.Parser.parse_line "f x y = x + y where y = True" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "where multiple statements incorrect" =
  (match Haskell_lib.Parser.parse_line "f x  = x && y || z where y = False; z = 3" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "where polymorphic argument" =
  (match Haskell_lib.Parser.parse_line "f x = y where y = False" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    f:  t1 -> Bool
     ] |}]
;;

let%expect_test "where list argument" =
  (match Haskell_lib.Parser.parse_line "f (x:xs) = y : xs where y = 2" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    f:  [Int] -> [Int]
     ] |}]
;;

let%expect_test "function with tuple argument" =
  (match Haskell_lib.Parser.parse_line "f (x, y) = (x + 1, y && True)" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    f:  (Int, Bool) -> (Int, Bool)
     ] |}]
;;

let%expect_test "several functions with incorrect type" =
  (match Haskell_lib.Parser.parse_line "f x = x + 1; g = f y where y = False" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "correct arrow declaration" =
  (match Haskell_lib.Parser.parse_line "f :: Int -> Int; f x = x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    f:  Int -> Int
     ] |}]
;;

let%expect_test "incorrect arrow declaration" =
  (match Haskell_lib.Parser.parse_line "f :: Int; f x = x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and t1 -> t1 |}]
;;

let%expect_test "incorrect arrow declaration with different types" =
  (match Haskell_lib.Parser.parse_line "f :: Int -> Bool; f x = x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    unification failed on Bool and Int |}]
;;

let%expect_test "incorrect list declaration with different types" =
  (match Haskell_lib.Parser.parse_line "a :: [Int]; a = [False, True]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    unification failed on Int and Bool |}]
;;

let%expect_test "correct declaration with explicit type" =
  (match Haskell_lib.Parser.parse_line "a :: [Int]; (a :: [Int]) = [1, 2]" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "incorrect declaration with explicit type" =
  (match Haskell_lib.Parser.parse_line "f :: Bool -> Bool; f (x :: Int) = x" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    unification failed on Bool and Int |}]
;;

let%expect_test "correct tuple declaration" =
  (match Haskell_lib.Parser.parse_line "a :: (Int, Bool, ()); a = (1, True, ())" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (Int, Bool, ())
     ] |}]
;;

let%expect_test "incorrect tuple declaration" =
  (match Haskell_lib.Parser.parse_line "a :: (Int, Bool, ()); a = (False, True, ())" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    unification failed on Int and Bool |}]
;;

let%expect_test "failed unification" =
  (match
     Haskell_lib.Parser.parse_line "a = let f = (\\id -> (id 1, id True)) (\\x -> x) in f"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "generalization" =
  (match
     Haskell_lib.Parser.parse_line
       "a = let f = \\x -> let const = \\y -> x in const x in f"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  t8 -> t8
     ] |}]
;;

let%expect_test "incompatible restrictions" =
  (match
     Haskell_lib.Parser.parse_line
       "a = let double f z = f (f z) in (double (\\x -> x+1) 1, double (\\x -> x && x) \
        False)"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "y-combinator" =
  (match Haskell_lib.Parser.parse_line "a = let fix f = f (fix f) in fix" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  (t5 -> t5) -> t5
     ] |}]
;;

let%expect_test "z-combinator without recursion" =
  (match Haskell_lib.Parser.parse_line "a = let fix f eta = f (fix f) eta in fix" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    a:  ((t4 -> t7) -> t4 -> t7) -> t4 -> t7
     ] |}]
;;

let%expect_test "occurs check" =
  (match Haskell_lib.Parser.parse_line "a = let f x = f in f" with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| Occurs check failed |}]
;;

let%expect_test "fail unification" =
  (match
     Haskell_lib.Parser.parse_line "a = let f = (\\x -> x) in let g = (f True) in f 3"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "fail unification" =
  (match
     Haskell_lib.Parser.parse_line "a = (\\f -> let g = (f True) in (f 3)) (\\x -> x)"
   with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "unif with ord, succ" =
  (match Haskell_lib.Parser.parse_line "f x = x > (1,2); g y = y < Just True " with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    [
    f:  (Int, Int) -> Bool
    g:  Maybe Bool -> Bool
     ] |}]
;;

let%expect_test "unif with ord, fail (tuple)" =
  (match Haskell_lib.Parser.parse_line "f x = x > (1, \\ x -> x) " with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    unification failed on Ord t6 and t3 -> t3 |}]
;;

let%expect_test "unif with ord, fail" =
  (match Haskell_lib.Parser.parse_line "f x = x > [\\ x -> x] " with
   | Result.Ok bindings ->
     (match Haskell_lib.Inferencer.w_program bindings with
      | Result.Ok env -> Format.printf "%a" Haskell_lib.Inferencer.pp_typeenv env
      | Result.Error err -> Format.printf "%a" Haskell_lib.Pprint.pp_error err)
   | _ -> prerr_endline "parsing error");
  [%expect {|
    unification failed on Ord t6 and t4 -> t4 |}]
;;
