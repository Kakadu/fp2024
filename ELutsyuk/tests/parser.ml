(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Parse

let parse_program input =
  match parse input with
  | Ok program -> Stdlib.Format.printf "%s\n" (show_program program)
  | Error err -> Stdlib.Format.printf "%s\n" err
;;

let%expect_test "parse_factorial" =
  parse_program "let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;";
  [%expect
    {|
    [(Binding
        { is_rec = Rec; pat = (PatVar "fact");
          expr =
          (Fun ((PatVar "n"),
             (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))), (Const (Int 1)),
                (BinOp (Mul, (Var "n"),
                   (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (Int 1))))
                      ))
                   ))
                ))
             ))
          })
      ] |}]
;;

let%expect_test "parse_arithmetic" =
  parse_program "1 + 2 * 3;;";
  [%expect
    {|
    [(EvalExpr
        (BinOp (Add, (Const (Int 1)),
           (BinOp (Mul, (Const (Int 2)), (Const (Int 3)))))))
      ] |}]
;;

let%expect_test "parse_application" =
  parse_program "fact (n - 1);;";
  [%expect
    {|
    [(EvalExpr (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (Int 1)))))))] |}]
;;

let%expect_test "parse_multiple_evaluations" =
  parse_program "1 + 1;; 2 + 2;;";
  [%expect
    {|
    [(EvalExpr (BinOp (Add, (Const (Int 1)), (Const (Int 1)))));
      (EvalExpr (BinOp (Add, (Const (Int 2)), (Const (Int 2)))))] |}]
;;

let%expect_test "parse_multiple_bindings" =
  parse_program "let x = 10;; let y = x + 5;;";
  [%expect
    {|
    [(Binding { is_rec = NonRec; pat = (PatVar "x"); expr = (Const (Int 10)) });
      (Binding
         { is_rec = NonRec; pat = (PatVar "y");
           expr = (BinOp (Add, (Var "x"), (Const (Int 5)))) })
      ] |}]
;;

let%expect_test "parse_brackets" =
  parse_program "(1 + 2) * 3;;";
  [%expect
    {|
    [(EvalExpr
        (BinOp (Mul, (BinOp (Add, (Const (Int 1)), (Const (Int 2)))),
           (Const (Int 3)))))
      ] |}]
;;

let%expect_test "parse_nested_function" =
  parse_program "let rec add x y = x + y;; add 5 3;;";
  [%expect
    {|
    [(Binding
        { is_rec = Rec; pat = (PatVar "add");
          expr =
          (Fun ((PatVar "x"),
             (Fun ((PatVar "y"), (BinOp (Add, (Var "x"), (Var "y")))))))
          });
      (EvalExpr (App ((App ((Var "add"), (Const (Int 5)))), (Const (Int 3)))))] |}]
;;

let%expect_test "parse_match_expression" =
  parse_program "match x with | 1 -> {|one|} | 2 -> {|two|};;";
  [%expect
    {|
    [(EvalExpr
        (Match ((Var "x"),
           { match_pat = (PatConst (Int 1)); match_expr = (Const (Str "one")) },
           [{ match_pat = (PatConst (Int 2)); match_expr = (Const (Str "two")) }]
           )))
      ] |}]
;;

let%expect_test "parse_tuple" =
  parse_program "(1, 2, 3);;";
  [%expect
    {|
    [(EvalExpr (Tup ((Const (Int 1)), (Const (Int 2)), [(Const (Int 3))])))] |}]
;;

let%expect_test "parse_eval_without_;;" =
  parse_program "1 + 1";
  [%expect {|
    : not enough input |}]
;;

let%expect_test "parse_sub_without_ws" =
  parse_program "n-1;;";
  [%expect {|
    [(EvalExpr (BinOp (Sub, (Var "n"), (Const (Int 1)))))]|}]
;;

let%expect_test "parse_two_func" =
  parse_program
    "let rec fac n = if n<=1 then 1 else n * fac (n-1);;\n\n\
     let main =\n\
    \  let () = print_int (fac 4) in\n\
    \  0\n\
     ;;";
  [%expect
    {|
    [(Binding
        { is_rec = Rec; pat = (PatVar "fac");
          expr =
          (Fun ((PatVar "n"),
             (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))), (Const (Int 1)),
                (BinOp (Mul, (Var "n"),
                   (App ((Var "fac"), (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                   ))
                ))
             ))
          });
      (Binding
         { is_rec = NonRec; pat = (PatVar "main");
           expr =
           (Let (
              { is_rec = NonRec; pat = (PatConst Unit);
                expr =
                (App ((Var "print_int"), (App ((Var "fac"), (Const (Int 4)))))) },
              (Const (Int 0))))
           })
      ] |}]
;;
