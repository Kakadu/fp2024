(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Parse

let parse_program input =
  match parse input with
  | Ok program -> Stdlib.Format.printf "%s\n" (show_program program)
  | Error err -> Stdlib.Format.printf "%s\n" err
;;

let%expect_test "parse_arithmetic" =
  parse_program "1 + 2 * 3";
  [%expect
    {|
    [(EvalExpr
        (BinOp (Add, (Const (Int 1)),
           (BinOp (Mul, (Const (Int 2)), (Const (Int 3)))))))
      ] |}]
;;

let%expect_test "parse_application" =
  parse_program "fact (n - 1)";
  [%expect
    {|
    [(EvalExpr (App ((Var "fact"), (BinOp (Sub, (Var "n"), (Const (Int 1)))))))] |}]
;;

let%expect_test "parse_application" =
  parse_program "foo -1";
  [%expect
    {|
    [(EvalExpr
        (App ((Var "foo"), (BinOp (Sub, (Const (Int 0)), (Const (Int 1)))))))
      ] |}]
;;

let%expect_test "parse_multiple_evaluations" =
  parse_program "1 + 1;;   2 + 2";
  [%expect
    {|
  [(EvalExpr (BinOp (Add, (Const (Int 1)), (Const (Int 1)))));
    (EvalExpr (BinOp (Add, (Const (Int 2)), (Const (Int 2)))))] |}]
;;

let%expect_test "parse_multiple_bindings" =
  parse_program "let x = 10\n   let y = x + 5";
  [%expect
    {|
[(Binding (NonRec, { pat = (PatVar "x"); expr = (Const (Int 10)) }, []));
  (Binding (NonRec,
     { pat = (PatVar "y"); expr = (BinOp (Add, (Var "x"), (Const (Int 5)))) },
     []))
  ] |}]
;;

let%expect_test "parse_brackets" =
  parse_program "(1 + 2) * 3";
  [%expect
    {|
    [(EvalExpr
        (BinOp (Mul, (BinOp (Add, (Const (Int 1)), (Const (Int 2)))),
           (Const (Int 3)))))
      ] |}]
;;

let%expect_test "complex_tuple" =
  parse_program "((1, 1, 1), (2, 2, 2), {|meow|})";
  [%expect
    {|
    [(EvalExpr
        (Tup ((Tup ((Const (Int 1)), (Const (Int 1)), [(Const (Int 1))])),
           (Tup ((Const (Int 2)), (Const (Int 2)), [(Const (Int 2))])),
           [(Const (Str "meow"))])))
      ] |}]
;;

let%expect_test "parse_nested_function" =
  parse_program "let rec add x y = x + y;;   add 5 3";
  [%expect
    {|
    [(Binding (Rec,
        { pat = (PatVar "add");
          expr =
          (Fun ((PatVar "x"),
             (Fun ((PatVar "y"), (BinOp (Add, (Var "x"), (Var "y")))))))
          },
        []));
      (EvalExpr (App ((App ((Var "add"), (Const (Int 5)))), (Const (Int 3)))))] |}]
;;

(* let%expect_test "parse_match_expression" =
  parse_program "match x with | 1 -> {|one|} | 2 -> {|two|}";
  [%expect
    {|
    [(EvalExpr
        (Match ((Var "x"),
           { match_pat = (PatConst (Int 1)); match_expr = (Const (Str "one")) },
           [{ match_pat = (PatConst (Int 2)); match_expr = (Const (Str "two")) }]
           )))
      ] |}]
;; *)

let%expect_test "parse_tuple" =
  parse_program "(1, 2, 3)";
  [%expect
    {|
    [(EvalExpr (Tup ((Const (Int 1)), (Const (Int 2)), [(Const (Int 3))])))] |}]
;;

let%expect_test "parse_two_func" =
  parse_program
    "let rec fac n = if n<=1 then 1 else n * fac (n-1)\n\
     let main =\n\
    \  let () = print_int (fac 4) in\n\
    \  0\n\
     ;;";
  [%expect
    {|
    [(Binding (Rec,
        { pat = (PatVar "fac");
          expr =
          (Fun ((PatVar "n"),
             (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))), (Const (Int 1)),
                (BinOp (Mul, (Var "n"),
                   (App ((Var "fac"), (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                   ))
                ))
             ))
          },
        []));
      (Binding (NonRec,
         { pat = (PatVar "main");
           expr =
           (Let (NonRec,
              { pat = (PatConst Unit);
                expr =
                (App ((Var "print_int"), (App ((Var "fac"), (Const (Int 4)))))) },
              [], (Const (Int 0))))
           },
         []))
      ] |}]
;;

let%expect_test "parse_sub_without_ws" =
  parse_program "a-1";
  [%expect {|
    [(EvalExpr (BinOp (Sub, (Var "a"), (Const (Int 1)))))] |}]
;;

let%expect_test "parse_sub_with_ws" =
  parse_program "a - 1";
  [%expect {|
    [(EvalExpr (BinOp (Sub, (Var "a"), (Const (Int 1)))))] |}]
;;

let%expect_test "parse_unary_minus" =
  parse_program "a -1";
  [%expect
    {|
    [(EvalExpr (App ((Var "a"), (BinOp (Sub, (Const (Int 0)), (Const (Int 1)))))))
      ] |}]
;;

let%expect_test "parse_unary_plus" =
  parse_program "a +1";
  [%expect {| [(EvalExpr (App ((Var "a"), (Const (Int 1)))))] |}]
;;

let%expect_test "parse_001" =
  parse_program "let rec fac n = if n<=1 then 1 else n * fac (n-1)";
  [%expect
    {|
    [(Binding (Rec,
        { pat = (PatVar "fac");
          expr =
          (Fun ((PatVar "n"),
             (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))), (Const (Int 1)),
                (BinOp (Mul, (Var "n"),
                   (App ((Var "fac"), (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                   ))
                ))
             ))
          },
        []))
      ] |}]
;;

let%expect_test "parse_002if" =
  parse_program "let main = if true then 1 else false";
  [%expect
    {|
    [(Binding (NonRec,
        { pat = (PatVar "main");
          expr =
          (Branch ((Const (Bool true)), (Const (Int 1)), (Const (Bool false)))) },
        []))
      ] |}]
;;

let%expect_test "parse_004let_poly" =
  parse_program "let _1 =\n  (fun f -> (f 1, f true)) (fun x -> x)";
  [%expect
    {|
    [(Binding (NonRec,
        { pat = (PatVar "_1");
          expr =
          (App (
             (Fun ((PatVar "f"),
                (Tup ((App ((Var "f"), (Const (Int 1)))),
                   (App ((Var "f"), (Const (Bool true)))), []))
                )),
             (Fun ((PatVar "x"), (Var "x")))))
          },
        []))
      ]|}]
;;

let%expect_test "parse_015tuples" =
  parse_program "let rec (a,b) = (a,b)";
  [%expect
    {|
    [(Binding (Rec,
        { pat = (PatTup ((PatVar "a"), (PatVar "b"), []));
          expr = (Tup ((Var "a"), (Var "b"), [])) },
        []))
      ] |}]
;;

let%expect_test "parse_016tuples_mismatches" =
  parse_program "let a, b = 1, 2, 3";
  [%expect
    {|
    [(Binding (NonRec,
        { pat = (PatTup ((PatVar "a"), (PatVar "b"), []));
          expr = (Tup ((Const (Int 1)), (Const (Int 2)), [(Const (Int 3))])) },
        []))
      ] |}]
;;

let%expect_test "parse_097fun_vs_list" =
  parse_program "let [a] = (fun x -> x)";
  [%expect
    {|
    [(Binding (NonRec,
        { pat = (PatList [(PatVar "a")]); expr = (Fun ((PatVar "x"), (Var "x")))
          },
        []))
      ] |}]
;;

let%expect_test "parse_098rec_int" =
  parse_program "let rec x = x + 1";
  [%expect
    {|
    [(Binding (Rec,
        { pat = (PatVar "x"); expr = (BinOp (Add, (Var "x"), (Const (Int 1)))) },
        []))
      ] |}]
;;

let%expect_test "parse_099" =
  parse_program "let rec x::[] = [1]";
  [%expect
    {|
    [(Binding (Rec,
        { pat = (PatListCons ((PatVar "x"), (PatList [])));
          expr = (List [(Const (Int 1))]) },
        []))
      ] |}]
;;
