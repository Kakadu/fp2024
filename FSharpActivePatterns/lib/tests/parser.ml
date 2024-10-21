(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.Parser
open FSharpActivePatterns.PrintAst

let%expect_test "binary subtract" =
  let input = {| a - 3|} in
  print_p_res (parse input);
  [%expect
    {|
    | Binary expr(
    | Binary Subtract
    --| Variable(a)
    --| Const(Int: 3) |}]
;;

let%expect_test "function apply of letIn" =
  let input = {| f let x = false in true || x|} in
  print_p_res (parse input);
  [%expect
    {|
    | Function Call:
      FUNCTION
    --| Variable(f)
      ARGS
    -- | LetIn  x =
        ARGS
        BODY
    ----| Const(Bool: false)
        INNER EXPRESSION
    ----| Binary expr(
    ----| Logical Or
    ------| Const(Bool: true)
    ------| Variable(x) |}]
;;

let%expect_test "arithmetic with unary operations and variables" =
  let input = {| - a - - b + 4|} in
  print_p_res (parse input);
  [%expect
    {|
    | Binary expr(
    | Binary Add
    --| Binary expr(
    --| Binary Subtract
    ----| Unary expr(
    ----| Unary minus
    ------| Variable(a)
    ----| Unary expr(
    ----| Unary minus
    ------| Variable(b)
    --| Const(Int: 4) |}]
;;

let%expect_test "sum of function applying" =
  let input = {| f 4 + g 3|} in
  print_p_res (parse input);
  [%expect
    {|
    | Binary expr(
    | Binary Add
    --| Function Call:
        FUNCTION
    ----| Variable(f)
        ARGS
    ----| Const(Int: 4)
    --| Function Call:
        FUNCTION
    ----| Variable(g)
        ARGS
    ----| Const(Int: 3) |}]
;;

let%expect_test "order of logical expressions and function applying" =
  let input = {| let x = true in not x || true && f 12|} in
  print_p_res (parse input);
  [%expect
    {|
     | LetIn  x =
      ARGS
      BODY
    --| Const(Bool: true)
      INNER EXPRESSION
    --| Binary expr(
    --| Logical Or
    ----| Unary expr(
    ----| Unary negative
    ------| Variable(x)
    ----| Binary expr(
    ----| Logical And
    ------| Const(Bool: true)
    ------| Function Call:
            FUNCTION
    --------| Variable(f)
            ARGS
    --------| Const(Int: 12) |}]
;;

let%expect_test "parse logical expression" =
  let input = {| (3 + 5) >= 8 || true && (5 <> 4) |} in
  print_p_res (parse input);
  [%expect
    {|
    | Binary expr(
    | Logical Or
    --| Binary expr(
    --| Binary Greater Or Equal
    ----| Binary expr(
    ----| Binary Add
    ------| Const(Int: 3)
    ------| Const(Int: 5)
    ----| Const(Int: 8)
    --| Binary expr(
    --| Logical And
    ----| Const(Bool: true)
    ----| Binary expr(
    ----| Binary Unequal
    ------| Const(Int: 5)
    ------| Const(Int: 4) |}]
;;

let%expect_test "parse integer expression" =
  let input = " (3 + 5) - (12 / 7)" in
  print_p_res (parse input);
  [%expect
    {|
    | Binary expr(
    | Binary Subtract
    --| Binary expr(
    --| Binary Add
    ----| Const(Int: 3)
    ----| Const(Int: 5)
    --| Binary expr(
    --| Binary Divide
    ----| Const(Int: 12)
    ----| Const(Int: 7) |}]
;;

let%expect_test "parse_unary_chain" =
  let input = "not not ( not true && false || 3 > 5)" in
  let result = parse input in
  print_p_res result;
  [%expect
    {|
    | Unary expr(
    | Unary negative
    --| Unary expr(
    --| Unary negative
    ----| Binary expr(
    ----| Logical Or
    ------| Binary expr(
    ------| Logical And
    --------| Unary expr(
    --------| Unary negative
    ----------| Const(Bool: true)
    --------| Const(Bool: false)
    ------| Binary expr(
    ------| Binary Greater
    --------| Const(Int: 3)
    --------| Const(Int: 5) |}]
;;

let%expect_test "parse if with comparison" =
  let input = "if 3 > 2 && false then 5 + 7 else 12" in
  print_p_res (parse input);
  [%expect
    {|
    | If Then Else(
      CONDITION
    --| Binary expr(
    --| Logical And
    ----| Binary expr(
    ----| Binary Greater
    ------| Const(Int: 3)
    ------| Const(Int: 2)
    ----| Const(Bool: false)
      THEN BRANCH
    ----| Binary expr(
    ----| Binary Add
    ------| Const(Int: 5)
    ------| Const(Int: 7)
      ELSE BRANCH
    ----| Const(Int: 12) |}]
;;

let%expect_test "sum with if" =
  let input = "a + if 3 > 2 then 2 else 1" in
  print_p_res (parse input);
  [%expect
    {|
    | Binary expr(
    | Binary Add
    --| Variable(a)
    --| If Then Else(
        CONDITION
    ----| Binary expr(
    ----| Binary Greater
    ------| Const(Int: 3)
    ------| Const(Int: 2)
        THEN BRANCH
    ------| Const(Int: 2)
        ELSE BRANCH
    ------| Const(Int: 1) |}]
;;

let%expect_test "inner expressions with LetIn and If" =
  let input =
    "if let x = true in let y = false in x || y then 3 else if 5 > 3 then 2 else 1"
  in
  print_p_res (parse input);
  [%expect
    {|
    | If Then Else(
      CONDITION
    -- | LetIn  x =
        ARGS
        BODY
    ----| Const(Bool: true)
        INNER EXPRESSION
    ---- | LetIn  y =
          ARGS
          BODY
    ------| Const(Bool: false)
          INNER EXPRESSION
    ------| Binary expr(
    ------| Logical Or
    --------| Variable(x)
    --------| Variable(y)
      THEN BRANCH
    ----| Const(Int: 3)
      ELSE BRANCH
    ----| If Then Else(
          CONDITION
    ------| Binary expr(
    ------| Binary Greater
    --------| Const(Int: 5)
    --------| Const(Int: 3)
          THEN BRANCH
    --------| Const(Int: 2)
          ELSE BRANCH
    --------| Const(Int: 1) |}]
;;

let%expect_test "factorial" =
  let input = "let factorial n = if n = 0 then 1 else factorial (n - 1) in factorial b" in
  print_p_res (parse input);
  [%expect
    {|
     | LetIn  factorial =
      ARGS
    --| Variable(n)
      BODY
    --| If Then Else(
        CONDITION
    ----| Binary expr(
    ----| Binary Equal
    ------| Variable(n)
    ------| Const(Int: 0)
        THEN BRANCH
    ------| Const(Int: 1)
        ELSE BRANCH
    ------| Function Call:
            FUNCTION
    --------| Variable(factorial)
            ARGS
    --------| Binary expr(
    --------| Binary Subtract
    ----------| Variable(n)
    ----------| Const(Int: 1)
      INNER EXPRESSION
    --| Function Call:
        FUNCTION
    ----| Variable(factorial)
        ARGS
    ----| Variable(b) |}]
;;

let%expect_test "fail in ITE with incorrect else expression" =
  let input = "if true then 1 else 2c" in
  print_p_res (parse input);
  [%expect {| Error occured |}]
;;

let%expect_test "call if with parentheses" =
  let input = "(if(false)then(a) else(b))c" in
  print_p_res (parse input);
  [%expect
    {|
    | Function Call:
      FUNCTION
    --| If Then Else(
        CONDITION
    ----| Const(Bool: false)
        THEN BRANCH
    ------| Variable(a)
        ELSE BRANCH
    ------| Variable(b)
      ARGS
    --| Variable(c) |}]
;;
