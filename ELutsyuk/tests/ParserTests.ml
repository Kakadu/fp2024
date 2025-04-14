(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast
open Parser

let test_parse str =
  let open Stdlib.Format in
  match parse str with
  | Ok program -> printf "%s\n" (show_program program)
  | Error err -> printf "%s\n" err
;;

let%expect_test "parse_arithmetic" =
  test_parse "1 + 2 * 3";
  [%expect
    {|
    [(Eval
        (BinOp (Add, (Const (Int 1)),
           (BinOp (Mul, (Const (Int 2)), (Const (Int 3)))))))
      ] |}]
;;

let%expect_test "parse_application" =
  test_parse "fact (n - 1)";
  [%expect
    {|
    [(Eval (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (Int 1)))))))] |}]
;;

let%expect_test "parse_application" =
  test_parse "foo -1";
  [%expect {|
    [(Eval (BinOp (Sub, (Var "foo"), (Const (Int 1)))))] |}]
;;

let%expect_test "parse_multiple_bindings" =
  test_parse "let x = 10\n   let y = x + 5";
  [%expect
    {|
[(Value (NonRec, (Binding ((PatVar "x"), (Const (Int 10)))), []));
  (Value (NonRec,
     (Binding ((PatVar "y"), (BinOp (Add, (Var "x"), (Const (Int 5)))))),
     []))
  ] |}]
;;

let%expect_test "parse_brackets" =
  test_parse "(1 + 2) * 3";
  [%expect
    {|
    [(Eval
        (BinOp (Mul, (BinOp (Add, (Const (Int 1)), (Const (Int 2)))),
           (Const (Int 3)))))
      ] |}]
;;

let%expect_test "complex_tuple" =
  test_parse "((1, 1, 1), (2, 2, 2), {|meow|})";
  [%expect
    {|
    [(Eval
        (Tup ((Tup ((Const (Int 1)), (Const (Int 1)), [(Const (Int 1))])),
           (Tup ((Const (Int 2)), (Const (Int 2)), [(Const (Int 2))])),
           [(Const (Str "meow"))])))
      ] |}]
;;

let%expect_test "parse_tuple" =
  test_parse "(1, 2, 3)";
  [%expect {|
    [(Eval (Tup ((Const (Int 1)), (Const (Int 2)), [(Const (Int 3))])))] |}]
;;

let%expect_test "parse_two_func" =
  test_parse
    "let rec fac n = if n<=1 then 1 else n * fac (n-1)\n\
     let main =\n\
    \  let () = print_int (fac 4) in\n\
    \  0\n\
     ;;";
  [%expect
    {|
    [(Value (Rec,
        (Binding ((PatVar "fac"),
           (Fun ((PatVar "n"), [],
              (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))), (Const (Int 1)),
                 (BinOp (Mul, (Var "n"),
                    (App ((Var "fac"), (BinOp (Sub, (Var "n"), (Const (Int 1))))
                       ))
                    ))
                 ))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "main"),
            (Let (NonRec,
               (Binding ((PatConst Unit),
                  (App ((Var "print_int"), (App ((Var "fac"), (Const (Int 4))))))
                  )),
               [], (Const (Int 0))))
            )),
         []))
      ] |}]
;;

let%expect_test "parse_sub_without_ws" =
  test_parse "a-1";
  [%expect {|
    [(Eval (BinOp (Sub, (Var "a"), (Const (Int 1)))))] |}]
;;

let%expect_test "parse_sub_with_ws" =
  test_parse "a - 1";
  [%expect {|
    [(Eval (BinOp (Sub, (Var "a"), (Const (Int 1)))))] |}]
;;

let%expect_test "parse_unary_minus" =
  test_parse "a -1";
  [%expect {|
    [(Eval (BinOp (Sub, (Var "a"), (Const (Int 1)))))] |}]
;;

let%expect_test "parse_unary_plus" =
  test_parse "a +1";
  [%expect {| [(Eval (BinOp (Add, (Var "a"), (Const (Int 1)))))] |}]
;;
