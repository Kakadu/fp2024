(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let%expect_test "int type" =
  Haskell_lib.Pai.parse_and_infer "a = 42" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "bool type" =
  Haskell_lib.Pai.parse_and_infer "a = True" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "unit type" =
  Haskell_lib.Pai.parse_and_infer "a = ()" false;
  [%expect {|
    [
    a:  ()
     ] |}]
;;

let%expect_test "const with explicit correct single type" =
  Haskell_lib.Pai.parse_and_infer "a = 42 :: Int" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "const with explicit correct multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = (42 :: Int) :: Int" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "const with explicit wrong single type" =
  Haskell_lib.Pai.parse_and_infer "a = 42 :: Bool" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "const with explicit wrong multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = (42 :: Int) :: Bool" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "tuple" =
  Haskell_lib.Pai.parse_and_infer "a = (42, True)" false;
  [%expect {|
    [
    a:  (Int, Bool)
     ] |}]
;;

let%expect_test "tuple with extra types" =
  Haskell_lib.Pai.parse_and_infer "a = (42, True, ())" false;
  [%expect {|
    [
    a:  (Int, Bool, ())
     ] |}]
;;

let%expect_test "tuple with explicit correct single type" =
  Haskell_lib.Pai.parse_and_infer "a = (42, True, ()) :: (Int, Bool, ())" false;
  [%expect {|
    [
    a:  (Int, Bool, ())
     ] |}]
;;

let%expect_test "tuple with explicit correct multiple type" =
  Haskell_lib.Pai.parse_and_infer
    "a = ((42, True, ()) :: (Int, Bool, ())) :: (Int, Bool, ())"
    false;
  [%expect {|
    [
    a:  (Int, Bool, ())
     ] |}]
;;

let%expect_test "tuple with explicit wrong single type" =
  Haskell_lib.Pai.parse_and_infer "x = (42, True, ()) :: (Bool, Bool, ())" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "tuple with explicit wrong multiple type" =
  Haskell_lib.Pai.parse_and_infer
    "x = ((42, True, ()) :: (Int, Bool, ())) :: (Bool, Bool, ())"
    false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "maybe type just" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> Just x" false;
  [%expect {|
    [
    a:  t2 -> Maybe t2
     ] |}]
;;

let%expect_test "maybe type just int" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> Just (x + 1)" false;
  [%expect {|
    [
    a:  Int -> Maybe Int
     ] |}]
;;

let%expect_test "maybe type just list" =
  Haskell_lib.Pai.parse_and_infer "a = \\x y -> Just (y : x)" false;
  [%expect {|
    [
    a:  [t5] -> t5 -> Maybe [t5]
     ] |}]
;;

let%expect_test "maybe type nothing" =
  Haskell_lib.Pai.parse_and_infer "a = Nothing" false;
  [%expect {|
    [
    a:  Maybe t2
     ] |}]
;;

let%expect_test "correct ariphmetic operation" =
  Haskell_lib.Pai.parse_and_infer "a = 5 + 3" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "incorrect ariphmetic operation" =
  Haskell_lib.Pai.parse_and_infer "a = 5 + ()" false;
  [%expect {| unification failed on () and Int |}]
;;

let%expect_test "ariphmetic operation with explicit correct single type" =
  Haskell_lib.Pai.parse_and_infer "a = (5 + 3) :: Int" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "ariphmetic operation with explicit correct multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = ((5 + 3) :: Int) :: Int" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "ariphmetic operation with explicit wrong single type" =
  Haskell_lib.Pai.parse_and_infer "a = (5 + 3) :: Bool" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "ariphmetic operation with explicit wrong multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = ((5 + 3) :: Int) :: Bool" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "correct logical operation" =
  Haskell_lib.Pai.parse_and_infer "a = True && False" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "incorrect logical operation" =
  Haskell_lib.Pai.parse_and_infer "a = True && 1" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "logical operation with correct explicit single type" =
  Haskell_lib.Pai.parse_and_infer "a = True && False :: Bool" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "logical operation with correct explicit multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = (True && False :: Bool) :: Bool" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "logical operation with incorrect explicit single type" =
  Haskell_lib.Pai.parse_and_infer "a = True && False :: Int" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "logical operation with incorrect explicit multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = (True && False :: Bool) :: Int" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "correct comparison operation with int" =
  Haskell_lib.Pai.parse_and_infer "a = 1 <= 2" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "correct comparison operation with bool" =
  Haskell_lib.Pai.parse_and_infer "a = False <= True" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "incorrect comparison operation with () and int" =
  Haskell_lib.Pai.parse_and_infer "a = 1 <= ()" false;
  [%expect {| unification failed on () and Int |}]
;;

let%expect_test "incorrect comparison operation with bool and int" =
  Haskell_lib.Pai.parse_and_infer "a = 1 <= True" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "comparison operation with explicit correct single type" =
  Haskell_lib.Pai.parse_and_infer "a = (1 <= 2) :: Bool" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "comparison operation with explicit correct multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = ((1 <= 2) :: Bool) :: Bool" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "comparison operation with explicit wrong single type" =
  Haskell_lib.Pai.parse_and_infer "a = (1 <= 2) :: Int" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "comparison operation with explicit wrong multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = ((1 <= 2) :: Int) :: Bool" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "cons correct with int" =
  Haskell_lib.Pai.parse_and_infer "a = 1 : []" false;
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "cons correct with bool" =
  Haskell_lib.Pai.parse_and_infer "a = True : []" false;
  [%expect {|
    [
    a:  [Bool]
     ] |}]
;;

let%expect_test "cons incorrect with int" =
  Haskell_lib.Pai.parse_and_infer "a = 1 : 2" false;
  [%expect {| unification failed on Int and [Int] |}]
;;

let%expect_test "cons incorrect with bool" =
  Haskell_lib.Pai.parse_and_infer "a = True : False" false;
  [%expect {| unification failed on Bool and [Bool] |}]
;;

let%expect_test "cons incorrect with int and bool" =
  Haskell_lib.Pai.parse_and_infer "a = 1 : [True]" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "neg type correct" =
  Haskell_lib.Pai.parse_and_infer "a = -42" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "neg type incorrect" =
  Haskell_lib.Pai.parse_and_infer "a = -True" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "neg type with explicit correct single type" =
  Haskell_lib.Pai.parse_and_infer "a = -42 :: Int" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "neg type with explicit correct multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = (-42 :: Int) :: Int" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "neg type with explicit wrong single type" =
  Haskell_lib.Pai.parse_and_infer "a = -42 :: Bool" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "neg type with explicit wrong multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = (-42 :: Int) :: Bool" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "ord polymor" =
  Haskell_lib.Pai.parse_and_infer
    "a = (\\f -> let g = (f True) in (f 3)) (\\x -> x)"
    false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "if correct with int return type" =
  Haskell_lib.Pai.parse_and_infer "a = if True then 1 else -1" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "if correct with tuple return type" =
  Haskell_lib.Pai.parse_and_infer "a = if True then (True, 2) else (False, -1)" false;
  [%expect {|
    [
    a:  (Bool, Int)
     ] |}]
;;

let%expect_test "if incorrect with int condition" =
  Haskell_lib.Pai.parse_and_infer "a = if (1 + 2) then (True, 2) else (False, -1)" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "if incorrect with tuple condition" =
  Haskell_lib.Pai.parse_and_infer
    "a = if (True, ()) then (True, 2) else (False, -1)"
    false;
  [%expect {| unification failed on (Bool, ()) and Bool |}]
;;

let%expect_test "if incorrect with int and bool return types" =
  Haskell_lib.Pai.parse_and_infer "a = if True then 1 else False" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "if incorrect with int and tuple return types" =
  Haskell_lib.Pai.parse_and_infer "a = if True then 1 else (1, False)" false;
  [%expect {| unification failed on Int and (Int, Bool) |}]
;;

let%expect_test "if incorrect with bool and list return types" =
  Haskell_lib.Pai.parse_and_infer "a = if True then True else [1, 4]" false;
  [%expect {| unification failed on Bool and [Int] |}]
;;

let%expect_test "lambda ident" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> x" false;
  [%expect {|
    [
    a:  t2 -> t2
     ] |}]
;;

let%expect_test "lambda int return type" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> 1" false;
  [%expect {|
    [
    a:  t2 -> Int
     ] |}]
;;

let%expect_test "lambda narrowing to int type" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> x + 1" false;
  [%expect {|
    [
    a:  Int -> Int
     ] |}]
;;

let%expect_test "lambda tuple return type" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> (x, ())" false;
  [%expect {|
    [
    a:  t2 -> (t2, ())
     ] |}]
;;

let%expect_test "lambda multiple arguments" =
  Haskell_lib.Pai.parse_and_infer "a = \\x y z -> x + y + z" false;
  [%expect {|
    [
    a:  Int -> Int -> Int -> Int
     ] |}]
;;

let%expect_test "lambda narrowing to list type" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> 1 : x" false;
  [%expect {|
    [
    a:  [Int] -> [Int]
     ] |}]
;;

let%expect_test "lambda narrowing to arrow type" =
  Haskell_lib.Pai.parse_and_infer "a = \\f -> \\y -> f y" false;
  [%expect {|
    [
    a:  (t3 -> t4) -> t3 -> t4
     ] |}]
;;

let%expect_test "lambda occurs check" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> x x" false;
  [%expect {| Occurs check failed |}]
;;

let%expect_test "lambda tuple return type" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> x `mod` 2 == 0 && x > 5" false;
  [%expect {|
    [
    a:  Int -> Bool
     ] |}]
;;

let%expect_test "lambda correct with explicit single type" =
  Haskell_lib.Pai.parse_and_infer "a = (\\x -> 1) :: (Int -> Int)" false;
  [%expect {|
    [
    a:  Int -> Int
     ] |}]
;;

let%expect_test "lambda correct with explicit multiple type" =
  Haskell_lib.Pai.parse_and_infer
    "a = ((\\x -> 1) :: (Bool -> Int)) :: (Bool -> Int)"
    false;
  [%expect {|
    [
    a:  Bool -> Int
     ] |}]
;;

let%expect_test "lambda wrong with explicit single type" =
  Haskell_lib.Pai.parse_and_infer "a = (\\x -> ()) :: (() -> Bool)" false;
  [%expect {| unification failed on () and Bool |}]
;;

let%expect_test "lambda wrong with explicit multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = ((\\x -> ()) :: (() -> ())) :: (() -> [Int])" false;
  [%expect {| unification failed on [Int] and () |}]
;;

let%expect_test "let id" =
  Haskell_lib.Pai.parse_and_infer "a = let x = x in x" false;
  [%expect {|
    [
    a:  t3
     ] |}]
;;

let%expect_test "let narrowing to int" =
  Haskell_lib.Pai.parse_and_infer "a = let x = x; y = 1 in x + y" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "let narrowing to [int]" =
  Haskell_lib.Pai.parse_and_infer "a = let x = x; y = 1 in y : x" false;
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "let narrowing to bool" =
  Haskell_lib.Pai.parse_and_infer "a = let x = x; y = True in y && x" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "let function" =
  Haskell_lib.Pai.parse_and_infer "a = let compose f g x = f (g x) in compose" false;
  [%expect {|
    [
    a:  (t6 -> t7) -> (t5 -> t6) -> t5 -> t7
     ] |}]
;;

let%expect_test "let recursive fib" =
  Haskell_lib.Pai.parse_and_infer
    "a = let fib n = if (n == 0) then 0 else if (n==1) then 1 else ((fib (n-1)) + (fib \
     (n-2))) in fib"
    false;
  [%expect {|
    [
    a:  Int -> Int
     ] |}]
;;

let%expect_test "let recursive fac" =
  Haskell_lib.Pai.parse_and_infer
    "a = let factorial = \\n -> if n == 0 then 1 else n * factorial (n - 1) in factorial"
    false;
  [%expect {|
    [
    a:  Int -> Int
     ] |}]
;;

let%expect_test "let with explicit correct single type" =
  Haskell_lib.Pai.parse_and_infer "a = let (x :: Int) = x in x" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "let with explicit correct mutliple type" =
  Haskell_lib.Pai.parse_and_infer "a = let ((x :: Int) :: Int) = x in x" false;
  [%expect {|
    [
    a:  Int
     ] |}]
;;

let%expect_test "let with explicit wrong single type" =
  Haskell_lib.Pai.parse_and_infer "a = let (x :: Bool) = 1 in x" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "let with explicit wrong mutliple type" =
  Haskell_lib.Pai.parse_and_infer "a = let ((x :: Int) :: Bool) = x in x" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "let wrong unification" =
  Haskell_lib.Pai.parse_and_infer "a = let x = if x <= True then 1 else 0 in x + 1" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "let wrong unification" =
  Haskell_lib.Pai.parse_and_infer
    "a = let x = if x <= True then True else False in x"
    false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "let wrong unification" =
  Haskell_lib.Pai.parse_and_infer
    "a = let x = if x <= True then True else False in x"
    false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "case correct with int type" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> case x of 1 -> True; 2 -> False" false;
  [%expect {|
    [
    a:  Int -> Bool
     ] |}]
;;

let%expect_test "case correct with lists" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> case x of (x:xs) -> x; [] -> []" false;
  [%expect {|
    [
    a:  [[t7]] -> [t7]
     ] |}]
;;

let%expect_test "case correct with int lists and explicit similar types" =
  Haskell_lib.Pai.parse_and_infer
    "a = \\x -> case x of ((x :: [Int]):(xs :: [[Int]])) -> x; [] -> []"
    false;
  [%expect {|
    [
    a:  [[Int]] -> [Int]
     ] |}]
;;

let%expect_test "case incorrect with int lists and explicit different types" =
  Haskell_lib.Pai.parse_and_infer
    "a = \\x -> case x of ((x :: [Int]):(xs :: [[Bool]])) -> x; [] -> []"
    false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "function apply incorrect" =
  Haskell_lib.Pai.parse_and_infer "a = (\\x -> x + 1) True" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "function apply list return type" =
  Haskell_lib.Pai.parse_and_infer "a = (\\x -> x : []) True" false;
  [%expect {|
    [
    a:  [Bool]
     ] |}]
;;

let%expect_test "function apply with correct single type" =
  Haskell_lib.Pai.parse_and_infer "a = (\\(x :: Int) -> x <= 2) 1" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "function apply return correct multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = (\\((x :: Int) :: Int) -> x <= 2) 1" false;
  [%expect {|
    [
    a:  Bool
     ] |}]
;;

let%expect_test "function apply return wrong single type" =
  Haskell_lib.Pai.parse_and_infer "a = (\\(x :: Bool) -> x <= 2) 1" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "function apply return wrong multiple type" =
  Haskell_lib.Pai.parse_and_infer "a = (\\((x :: Int) :: Bool) -> x <= 2) 1" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "function apply return correct single type" =
  Haskell_lib.Pai.parse_and_infer "a = (\\(x :: Int) -> x : []) 1" false;
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "list int" =
  Haskell_lib.Pai.parse_and_infer "a = [1, 2]" false;
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "lazy list int" =
  Haskell_lib.Pai.parse_and_infer "a = [1..]" false;
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "lazy list wrong type" =
  Haskell_lib.Pai.parse_and_infer "a = [(True, 1)..]" false;
  [%expect {| unification failed on Enum t2 and (Bool, Int) |}]
;;

let%expect_test "list of list" =
  Haskell_lib.Pai.parse_and_infer "a = [[True]]" false;
  [%expect {|
    [
    a:  [[Bool]]
     ] |}]
;;

let%expect_test "wrong list of different types" =
  Haskell_lib.Pai.parse_and_infer "a = [True, (), 3]" false;
  [%expect {| unification failed on Bool and () |}]
;;

let%expect_test "comprehension list with generator" =
  Haskell_lib.Pai.parse_and_infer "a = [x * y | x <- [1..10], y <- [1]]" false;
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "comprehension list with simple condition" =
  Haskell_lib.Pai.parse_and_infer "a = [1 * 2 | True]" false;
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "comprehension list with condition" =
  Haskell_lib.Pai.parse_and_infer "a = \\x -> [ x | x < 10 ]" false;
  [%expect {|
    [
    a:  Int -> [Int]
     ] |}]
;;

let%expect_test "comprehension list with condition and generator" =
  Haskell_lib.Pai.parse_and_infer "a = \\y -> [ x * y | x <- [1..10], y <= 10  ]" false;
  [%expect {|
    [
    a:  Int -> [Int]
     ] |}]
;;

let%expect_test "wrong comprehension list with generator condition" =
  Haskell_lib.Pai.parse_and_infer
    "a = \\x y -> [ x * y | x < 10, y <- [True, False]]"
    false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "several functions" =
  Haskell_lib.Pai.parse_and_infer "f x = g x; g y = y" false;
  [%expect {|
  [
  f:  t4 -> t4
  g:  t4 -> t4
   ] |}]
;;

let%expect_test "mutually recursive functions" =
  Haskell_lib.Pai.parse_and_infer "f x = g x; g y = f y" false;
  [%expect {|
  [
  f:  t4 -> t5
  g:  t4 -> t5
   ] |}]
;;

let%expect_test "mutually recursive functions with guards" =
  Haskell_lib.Pai.parse_and_infer
    "isEven n | n == 0 = True | n > 0 = isOdd (n - 1) | True = isOdd (-n); isOdd n | n \
     == 0 = False | n > 0 = isEven (n - 1) | True = isEven (-n)"
    false;
  [%expect {|
  [
  isEven:  Int -> Bool
  isOdd:  Int -> Bool
   ] |}]
;;

let%expect_test "guards" =
  Haskell_lib.Pai.parse_and_infer "f x | x > 0 = x | True = -1" false;
  [%expect {|
    [
    f:  Int -> Int
     ] |}]
;;

let%expect_test "where single statement" =
  Haskell_lib.Pai.parse_and_infer "f x = x + y where y = 1" false;
  [%expect {| 
    [
    f:  Int -> Int
     ] |}]
;;

let%expect_test "where single statement with explicit incorrect type" =
  Haskell_lib.Pai.parse_and_infer "f x = x + y where (y :: Bool) = 1" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "where multiple statements" =
  Haskell_lib.Pai.parse_and_infer "f x = x && y || z where y = False; z = True" false;
  [%expect {| 
    [
    f:  Bool -> Bool
     ] |}]
;;

let%expect_test "where single statement incorrect" =
  Haskell_lib.Pai.parse_and_infer "f x = x + y where y = True" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "where single statement with param shadowing incorrect" =
  Haskell_lib.Pai.parse_and_infer "f x y = x + y where y = True" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "where multiple statements incorrect" =
  Haskell_lib.Pai.parse_and_infer "f x  = x && y || z where y = False; z = 3" false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "where polymorphic argument" =
  Haskell_lib.Pai.parse_and_infer "f x = y where y = False" false;
  [%expect {|
    [
    f:  t1 -> Bool
     ] |}]
;;

let%expect_test "where list argument" =
  Haskell_lib.Pai.parse_and_infer "f (x:xs) = y : xs where y = 2" false;
  [%expect {|
    [
    f:  [Int] -> [Int]
     ] |}]
;;

let%expect_test "function with tuple argument" =
  Haskell_lib.Pai.parse_and_infer "f (x, y) = (x + 1, y && True)" false;
  [%expect {|
    [
    f:  (Int, Bool) -> (Int, Bool)
     ] |}]
;;

let%expect_test "several functions with incorrect type" =
  Haskell_lib.Pai.parse_and_infer "f x = x + 1; g = f y where y = False" false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "correct arrow declaration" =
  Haskell_lib.Pai.parse_and_infer "f :: Int -> Int; f x = x" false;
  [%expect {|
    [
    f:  Int -> Int
     ] |}]
;;

let%expect_test "incorrect arrow declaration" =
  Haskell_lib.Pai.parse_and_infer "f :: Int; f x = x" false;
  [%expect {| unification failed on Int and t1 -> t1 |}]
;;

let%expect_test "incorrect arrow declaration with different types" =
  Haskell_lib.Pai.parse_and_infer "f :: Int -> Bool; f x = x" false;
  [%expect {|
    unification failed on Bool and Int |}]
;;

let%expect_test "incorrect list declaration with different types" =
  Haskell_lib.Pai.parse_and_infer "a :: [Int]; a = [False, True]" false;
  [%expect {|
    unification failed on Int and Bool |}]
;;

let%expect_test "correct declaration with explicit type" =
  Haskell_lib.Pai.parse_and_infer "a :: [Int]; (a :: [Int]) = [1, 2]" false;
  [%expect {|
    [
    a:  [Int]
     ] |}]
;;

let%expect_test "incorrect declaration with explicit type" =
  Haskell_lib.Pai.parse_and_infer "f :: Bool -> Bool; f (x :: Int) = x" false;
  [%expect {|
    unification failed on Bool and Int |}]
;;

let%expect_test "correct tuple declaration" =
  Haskell_lib.Pai.parse_and_infer "a :: (Int, Bool, ()); a = (1, True, ())" false;
  [%expect {|
    [
    a:  (Int, Bool, ())
     ] |}]
;;

let%expect_test "incorrect tuple declaration" =
  Haskell_lib.Pai.parse_and_infer "a :: (Int, Bool, ()); a = (False, True, ())" false;
  [%expect {|
    unification failed on Int and Bool |}]
;;

let%expect_test "failed unification" =
  Haskell_lib.Pai.parse_and_infer
    "a = let f = (\\id -> (id 1, id True)) (\\x -> x) in f"
    false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "generalization" =
  Haskell_lib.Pai.parse_and_infer
    "a = let f = \\x -> let const = \\y -> x in const x in f"
    false;
  [%expect {|
    [
    a:  t8 -> t8
     ] |}]
;;

let%expect_test "incompatible restrictions" =
  Haskell_lib.Pai.parse_and_infer
    "a = let double f z = f (f z) in (double (\\x -> x+1) 1, double (\\x -> x && x) \
     False)"
    false;
  [%expect {| unification failed on Int and Bool |}]
;;

let%expect_test "y-combinator" =
  Haskell_lib.Pai.parse_and_infer "a = let fix f = f (fix f) in fix" false;
  [%expect {|
    [
    a:  (t5 -> t5) -> t5
     ] |}]
;;

let%expect_test "z-combinator without recursion" =
  Haskell_lib.Pai.parse_and_infer "a = let fix f eta = f (fix f) eta in fix" false;
  [%expect {|
    [
    a:  ((t4 -> t7) -> t4 -> t7) -> t4 -> t7
     ] |}]
;;

let%expect_test "occurs check" =
  Haskell_lib.Pai.parse_and_infer "a = let f x = f in f" false;
  [%expect {| Occurs check failed |}]
;;

let%expect_test "fail unification" =
  Haskell_lib.Pai.parse_and_infer
    "a = let f = (\\x -> x) in let g = (f True) in f 3"
    false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "fail unification" =
  Haskell_lib.Pai.parse_and_infer
    "a = (\\f -> let g = (f True) in (f 3)) (\\x -> x)"
    false;
  [%expect {| unification failed on Bool and Int |}]
;;

let%expect_test "unif with ord, succ" =
  Haskell_lib.Pai.parse_and_infer "f x = x > (1,2); g y = y < Just True " false;
  [%expect {|
    [
    f:  (Int, Int) -> Bool
    g:  Maybe Bool -> Bool
     ] |}]
;;

let%expect_test "unif with ord, fail (tuple)" =
  Haskell_lib.Pai.parse_and_infer "f x = x > (1, \\ x -> x) " false;
  [%expect {|
    unification failed on Ord t6 and t3 -> t3 |}]
;;

let%expect_test "unif with ord, fail" =
  Haskell_lib.Pai.parse_and_infer "f x = x > [\\ x -> x] " false;
  [%expect {|
    unification failed on Ord t6 and t4 -> t4 |}]
;;
