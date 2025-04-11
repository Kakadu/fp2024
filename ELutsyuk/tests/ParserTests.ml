(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast
open Parser

let test_parse str =
  match parse_program str with
  | Ok program -> Stdlib.Format.printf "%s\n" (show_program program)
  | Error err -> Stdlib.Format.printf "%s\n" err
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

let%expect_test "parse_001" =
  test_parse "let rec fac n = if n<=1 then 1 else n * fac (n-1)";
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
        []))
      ] |}]
;;

let%expect_test "parse_002if" =
  test_parse "let main = if true then 1 else false";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "main"),
           (Branch ((Const (Bool true)), (Const (Int 1)), (Const (Bool false))))
           )),
        []))
      ] |}]
;;

let%expect_test "parse_003occurs" =
  test_parse "let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f))";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "fix"),
           (Fun ((PatVar "f"), [],
              (App (
                 (Fun ((PatVar "x"), [],
                    (App ((Var "f"),
                       (Fun ((PatVar "f"), [],
                          (App ((App ((Var "x"), (Var "x"))), (Var "f")))))
                       ))
                    )),
                 (Fun ((PatVar "x"), [],
                    (App ((Var "f"),
                       (Fun ((PatVar "f"), [],
                          (App ((App ((Var "x"), (Var "x"))), (Var "f")))))
                       ))
                    ))
                 ))
              ))
           )),
        []))
      ] |}]
;;

let%expect_test "parse_x_x" =
  test_parse "x x";
  [%expect {| [(Eval (App ((Var "x"), (Var "x"))))] |}]
;;

let%expect_test "parse_004let_poly" =
  test_parse "let _1 =\n  (fun f -> (f 1, f true)) (fun x -> x)";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "_1"),
           (App (
              (Fun ((PatVar "f"), [],
                 (Tup ((App ((Var "f"), (Const (Int 1)))),
                    (App ((Var "f"), (Const (Bool true)))), []))
                 )),
              (Fun ((PatVar "x"), [], (Var "x")))))
           )),
        []))
      ]|}]
;;

let%expect_test "parse_015tuples" =
  test_parse "let rec (a,b) = (a,b)";
  [%expect
    {|
    [(Value (Rec,
        (Binding ((PatTup ((PatVar "a"), (PatVar "b"), [])),
           (Tup ((Var "a"), (Var "b"), [])))),
        []))
      ] |}]
;;

let%expect_test "parse_016tuples_mismatches" =
  test_parse "let a, b = 1, 2, 3";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatTup ((PatVar "a"), (PatVar "b"), [])),
           (Tup ((Const (Int 1)), (Const (Int 2)), [(Const (Int 3))])))),
        []))
      ] |}]
;;

let%expect_test "parse_097fun_vs_list" =
  test_parse "let [a] = (fun x -> x)";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatList [(PatVar "a")]), (Fun ((PatVar "x"), [], (Var "x"))))),
        []))
      ] |}]
;;

let%expect_test "parse_098rec_int" =
  test_parse "let rec x = x + 1";
  [%expect
    {|
    [(Value (Rec,
        (Binding ((PatVar "x"), (BinOp (Add, (Var "x"), (Const (Int 1)))))),
        []))
      ] |}]
;;

let%expect_test "parse_099" =
  test_parse "let rec x::[] = [1]";
  [%expect
    {|
    [(Value (Rec,
        (Binding ((PatListCons ((PatVar "x"), (PatList []))),
           (List [(Const (Int 1))]))),
        []))
      ] |}]
;;

let%expect_test "typed_0" =
  test_parse "let (a : int list) = 5";
  [%expect
    {|
[(Value (NonRec,
    (Binding ((PatType ((PatVar "a"), int list)), (Const (Int 5)))),
    []))
  ]
|}]
;;

let%expect_test "typed_001fac" =
  test_parse
    "let rec fac n = if n<=1 then 1 else n * fac (n-1)\n\n\
     let main =\n\
    \  let () = print_int (fac 4) in\n\
    \  0\n";
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

let%expect_test "typed_002fac" =
  test_parse
    "let rec fac_cps n k =\n\
    \  if n=1 then k 1 else\n\
    \  fac_cps (n-1) (fun p -> k (p*n))\n\n\
     let main =\n\
    \  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in\n\
    \  0\n";
  [%expect
    {|
    [(Value (Rec,
        (Binding ((PatVar "fac_cps"),
           (Fun ((PatVar "n"), [(PatVar "k")],
              (Branch ((BinOp (Eq, (Var "n"), (Const (Int 1)))),
                 (App ((Var "k"), (Const (Int 1)))),
                 (App (
                    (App ((Var "fac_cps"),
                       (BinOp (Sub, (Var "n"), (Const (Int 1)))))),
                    (Fun ((PatVar "p"), [],
                       (App ((Var "k"), (BinOp (Mul, (Var "p"), (Var "n")))))))
                    ))
                 ))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "main"),
            (Let (NonRec,
               (Binding ((PatConst Unit),
                  (App ((Var "print_int"),
                     (App ((App ((Var "fac_cps"), (Const (Int 4)))),
                        (Fun ((PatVar "print_int"), [], (Var "print_int")))))
                     ))
                  )),
               [], (Const (Int 0))))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_003fib" =
  test_parse
    "let rec fib_acc a b n =\n\
    \  if n=1 then b\n\
    \  else\n\
    \    let n1 = n-1 in\n\
    \    let ab = a+b in\n\
    \    fib_acc b ab n1\n\n\
     let rec fib n =\n\
    \  if n<2\n\
    \  then n\n\
    \  else fib (n - 1) + fib (n - 2) \n\n\
     let main =\n\
    \  let () = print_int (fib_acc 0 1 4) in\n\
    \  let () = print_int (fib 4) in\n\
    \  0\n";
  [%expect
    {|
    [(Value (Rec,
        (Binding ((PatVar "fib_acc"),
           (Fun ((PatVar "a"), [(PatVar "b"); (PatVar "n")],
              (Branch ((BinOp (Eq, (Var "n"), (Const (Int 1)))), (Var "b"),
                 (Let (NonRec,
                    (Binding ((PatVar "n1"),
                       (BinOp (Sub, (Var "n"), (Const (Int 1)))))),
                    [],
                    (Let (NonRec,
                       (Binding ((PatVar "ab"),
                          (BinOp (Add, (Var "a"), (Var "b"))))),
                       [],
                       (App (
                          (App ((App ((Var "fib_acc"), (Var "b"))), (Var "ab"))),
                          (Var "n1")))
                       ))
                    ))
                 ))
              ))
           )),
        []));
      (Value (Rec,
         (Binding ((PatVar "fib"),
            (Fun ((PatVar "n"), [],
               (Branch ((BinOp (Lt, (Var "n"), (Const (Int 2)))), (Var "n"),
                  (BinOp (Add,
                     (App ((Var "fib"), (BinOp (Sub, (Var "n"), (Const (Int 1))))
                        )),
                     (App ((Var "fib"), (BinOp (Sub, (Var "n"), (Const (Int 2))))
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
                  (App ((Var "print_int"),
                     (App (
                        (App ((App ((Var "fib_acc"), (Const (Int 0)))),
                           (Const (Int 1)))),
                        (Const (Int 4))))
                     ))
                  )),
               [],
               (Let (NonRec,
                  (Binding ((PatConst Unit),
                     (App ((Var "print_int"),
                        (App ((Var "fib"), (Const (Int 4))))))
                     )),
                  [], (Const (Int 0))))
               ))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_004manyargs" =
  test_parse
    "let wrap f = if 1 = 1 then f else f\n\n\
     let test3 a b c =\n\
    \  let a = print_int a in\n\
    \  let b = print_int b in\n\
    \  let c = print_int c in\n\
    \  0\n\n\
     let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j\n\n\
     let main =\n\
    \  let rez =\n\
    \      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000\n\
    \         1000000000)\n\
    \  in\n\
    \  let () = print_int rez in\n\
    \  let temp2 = wrap test3 1 10 100 in\n\
    \  0\n";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "wrap"),
           (Fun ((PatVar "f"), [],
              (Branch ((BinOp (Eq, (Const (Int 1)), (Const (Int 1)))), (Var "f"),
                 (Var "f")))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "test3"),
            (Fun ((PatVar "a"), [(PatVar "b"); (PatVar "c")],
               (Let (NonRec,
                  (Binding ((PatVar "a"), (App ((Var "print_int"), (Var "a"))))),
                  [],
                  (Let (NonRec,
                     (Binding ((PatVar "b"), (App ((Var "print_int"), (Var "b")))
                        )),
                     [],
                     (Let (NonRec,
                        (Binding ((PatVar "c"),
                           (App ((Var "print_int"), (Var "c"))))),
                        [], (Const (Int 0))))
                     ))
                  ))
               ))
            )),
         []));
      (Value (NonRec,
         (Binding ((PatVar "test10"),
            (Fun ((PatVar "a"),
               [(PatVar "b"); (PatVar "c"); (PatVar "d"); (PatVar "e");
                 (PatVar "f"); (PatVar "g"); (PatVar "h"); (PatVar "i");
                 (PatVar "j")],
               (BinOp (Add,
                  (BinOp (Add,
                     (BinOp (Add,
                        (BinOp (Add,
                           (BinOp (Add,
                              (BinOp (Add,
                                 (BinOp (Add,
                                    (BinOp (Add,
                                       (BinOp (Add, (Var "a"), (Var "b"))),
                                       (Var "c"))),
                                    (Var "d"))),
                                 (Var "e"))),
                              (Var "f"))),
                           (Var "g"))),
                        (Var "h"))),
                     (Var "i"))),
                  (Var "j")))
               ))
            )),
         []));
      (Value (NonRec,
         (Binding ((PatVar "main"),
            (Let (NonRec,
               (Binding ((PatVar "rez"),
                  (App (
                     (App (
                        (App (
                           (App (
                              (App (
                                 (App (
                                    (App (
                                       (App (
                                          (App (
                                             (App (
                                                (App ((Var "wrap"),
                                                   (Var "test10"))),
                                                (Const (Int 1)))),
                                             (Const (Int 10)))),
                                          (Const (Int 100)))),
                                       (Const (Int 1000)))),
                                    (Const (Int 10000)))),
                                 (Const (Int 100000)))),
                              (Const (Int 1000000)))),
                           (Const (Int 10000000)))),
                        (Const (Int 100000000)))),
                     (Const (Int 1000000000))))
                  )),
               [],
               (Let (NonRec,
                  (Binding ((PatConst Unit),
                     (App ((Var "print_int"), (Var "rez"))))),
                  [],
                  (Let (NonRec,
                     (Binding ((PatVar "temp2"),
                        (App (
                           (App (
                              (App ((App ((Var "wrap"), (Var "test3"))),
                                 (Const (Int 1)))),
                              (Const (Int 10)))),
                           (Const (Int 100))))
                        )),
                     [], (Const (Int 0))))
                  ))
               ))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_005fix" =
  test_parse
    "let rec fix f x = f (fix f) x\n\n\
     let fac self n = if n<=1 then 1 else n * self (n-1)\n\n\
     let main =\n\
    \  let () = print_int (fix fac 6) in\n\
    \  0\n";
  [%expect
    {|
    [(Value (Rec,
        (Binding ((PatVar "fix"),
           (Fun ((PatVar "f"), [(PatVar "x")],
              (App ((App ((Var "f"), (App ((Var "fix"), (Var "f"))))), (Var "x")
                 ))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "fac"),
            (Fun ((PatVar "self"), [(PatVar "n")],
               (Branch ((BinOp (Le, (Var "n"), (Const (Int 1)))),
                  (Const (Int 1)),
                  (BinOp (Mul, (Var "n"),
                     (App ((Var "self"),
                        (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                     ))
                  ))
               ))
            )),
         []));
      (Value (NonRec,
         (Binding ((PatVar "main"),
            (Let (NonRec,
               (Binding ((PatConst Unit),
                  (App ((Var "print_int"),
                     (App ((App ((Var "fix"), (Var "fac"))), (Const (Int 6))))))
                  )),
               [], (Const (Int 0))))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_006partial" =
  test_parse
    "let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)\n\n\
     let foo x = foo true (foo false (foo true (foo false x)))\n\
     let main =\n\
    \  let () = print_int (foo 11) in\n\
    \  0";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "foo"),
           (Fun ((PatVar "b"), [],
              (Branch ((Var "b"),
                 (Fun ((PatVar "foo"), [],
                    (BinOp (Add, (Var "foo"), (Const (Int 2)))))),
                 (Fun ((PatVar "foo"), [],
                    (BinOp (Mul, (Var "foo"), (Const (Int 10))))))
                 ))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "foo"),
            (Fun ((PatVar "x"), [],
               (App ((App ((Var "foo"), (Const (Bool true)))),
                  (App ((App ((Var "foo"), (Const (Bool false)))),
                     (App ((App ((Var "foo"), (Const (Bool true)))),
                        (App ((App ((Var "foo"), (Const (Bool false)))),
                           (Var "x")))
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
                  (App ((Var "print_int"), (App ((Var "foo"), (Const (Int 11))))
                     ))
                  )),
               [], (Const (Int 0))))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_006partial2" =
  test_parse
    "let foo a b c =\n\
    \  let () = print_int a in\n\
    \  let () = print_int b in\n\
    \  let () = print_int c in\n\
    \  a + b * c\n\n\
     let main =\n\
    \  let foo = foo 1 in\n\
    \  let foo = foo 2 in\n\
    \  let foo = foo 3 in\n\
    \  let () = print_int foo in\n\
    \  0";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "foo"),
           (Fun ((PatVar "a"), [(PatVar "b"); (PatVar "c")],
              (Let (NonRec,
                 (Binding ((PatConst Unit), (App ((Var "print_int"), (Var "a")))
                    )),
                 [],
                 (Let (NonRec,
                    (Binding ((PatConst Unit),
                       (App ((Var "print_int"), (Var "b"))))),
                    [],
                    (Let (NonRec,
                       (Binding ((PatConst Unit),
                          (App ((Var "print_int"), (Var "c"))))),
                       [],
                       (BinOp (Add, (Var "a"),
                          (BinOp (Mul, (Var "b"), (Var "c")))))
                       ))
                    ))
                 ))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "main"),
            (Let (NonRec,
               (Binding ((PatVar "foo"), (App ((Var "foo"), (Const (Int 1)))))),
               [],
               (Let (NonRec,
                  (Binding ((PatVar "foo"), (App ((Var "foo"), (Const (Int 2))))
                     )),
                  [],
                  (Let (NonRec,
                     (Binding ((PatVar "foo"),
                        (App ((Var "foo"), (Const (Int 3)))))),
                     [],
                     (Let (NonRec,
                        (Binding ((PatConst Unit),
                           (App ((Var "print_int"), (Var "foo"))))),
                        [], (Const (Int 0))))
                     ))
                  ))
               ))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_006partial3" =
  test_parse
    "let foo a =\n\
    \  let () = print_int a in fun b ->\n\
    \  let () = print_int b in fun c ->\n\
    \  print_int c\n\n\
     let main =\n\
    \  let () = foo 4 8 9 in\n\
    \  0";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "foo"),
           (Fun ((PatVar "a"), [],
              (Let (NonRec,
                 (Binding ((PatConst Unit), (App ((Var "print_int"), (Var "a")))
                    )),
                 [],
                 (Fun ((PatVar "b"), [],
                    (Let (NonRec,
                       (Binding ((PatConst Unit),
                          (App ((Var "print_int"), (Var "b"))))),
                       [],
                       (Fun ((PatVar "c"), [],
                          (App ((Var "print_int"), (Var "c")))))
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
                  (App (
                     (App ((App ((Var "foo"), (Const (Int 4)))), (Const (Int 8))
                        )),
                     (Const (Int 9))))
                  )),
               [], (Const (Int 0))))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_007order" =
  test_parse
    "let _start () () a () b _c () d __ =\n\
    \  let () = print_int (a+b) in\n\
    \  let () = print_int __ in\n\
    \  a*b / _c + d\n\n\n\
     let main =\n\
    \  print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int \
     (-1)) 10000 (-555555))";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "_start"),
           (Fun ((PatConst Unit),
              [(PatConst Unit); (PatVar "a"); (PatConst Unit); (PatVar "b");
                (PatVar "_c"); (PatConst Unit); (PatVar "d"); PatAny; PatAny],
              (Let (NonRec,
                 (Binding ((PatConst Unit),
                    (App ((Var "print_int"), (BinOp (Add, (Var "a"), (Var "b")))
                       ))
                    )),
                 [],
                 (Let (NonRec,
                    (Binding ((PatConst Unit),
                       (App ((Var "print_int"), (Var "__"))))),
                    [],
                    (BinOp (Add,
                       (BinOp (Div, (BinOp (Mul, (Var "a"), (Var "b"))),
                          (Var "_c"))),
                       (Var "d")))
                    ))
                 ))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "main"),
            (App ((Var "print_int"),
               (App (
                  (App (
                     (App (
                        (App (
                           (App (
                              (App (
                                 (App (
                                    (App (
                                       (App ((Var "_start"),
                                          (App ((Var "print_int"),
                                             (Const (Int 1))))
                                          )),
                                       (App ((Var "print_int"), (Const (Int 2))))
                                       )),
                                    (Const (Int 3)))),
                                 (App ((Var "print_int"), (Const (Int 4)))))),
                              (Const (Int 100)))),
                           (Const (Int 1000)))),
                        (App ((Var "print_int"), (Unary (Minus, (Const (Int 1))))
                           ))
                        )),
                     (Const (Int 10000)))),
                  (Unary (Minus, (Const (Int 555555))))))
               ))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_008ascription" =
  test_parse
    "let addi = fun f g x -> (f x (g x: bool) : int)\n\n\
     let main =\n\
    \  let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> \
     _start/2 = 0) 4) in\n\
    \  0";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "addi"),
           (Fun ((PatVar "f"), [(PatVar "g"); (PatVar "x")],
              (Type (
                 (App ((App ((Var "f"), (Var "x"))),
                    (Type ((App ((Var "g"), (Var "x"))), bool)))),
                 int))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "main"),
            (Let (NonRec,
               (Binding ((PatConst Unit),
                  (App ((Var "print_int"),
                     (App (
                        (App (
                           (App ((Var "addi"),
                              (Fun ((PatVar "x"), [(PatVar "b")],
                                 (Branch ((Var "b"),
                                    (BinOp (Add, (Var "x"), (Const (Int 1)))),
                                    (BinOp (Mul, (Var "x"), (Const (Int 2))))))
                                 ))
                              )),
                           (Fun ((PatVar "_start"), [],
                              (BinOp (Eq,
                                 (BinOp (Div, (Var "_start"), (Const (Int 2)))),
                                 (Const (Int 0))))
                              ))
                           )),
                        (Const (Int 4))))
                     ))
                  )),
               [], (Const (Int 0))))
            )),
         []))
      ] |}]
;;

let%expect_test "typed_009let_poly" =
  test_parse "let temp =\n  let f = fun x -> x in\n  (f 1, f true)";
  [%expect
    {|
    [(Value (NonRec,
        (Binding ((PatVar "temp"),
           (Let (NonRec,
              (Binding ((PatVar "f"), (Fun ((PatVar "x"), [], (Var "x"))))),
              [],
              (Tup ((App ((Var "f"), (Const (Int 1)))),
                 (App ((Var "f"), (Const (Bool true)))), []))
              ))
           )),
        []))
      ] |}]
;;

let%expect_test "typed_015tuples" =
  test_parse
    "let rec fix f x = f (fix f) x\n\
     let map f p = let (a,b) = p in (f a, f b)\n\
     let fixpoly l =\n\
    \  fix (fun self l -> map (fun li x -> li (self l) x) l) l\n\
     let feven p n =\n\
    \  let (e, o) = p in\n\
    \  if n = 0 then 1 else o (n - 1)\n\
     let fodd p n =\n\
    \  let (e, o) = p in\n\
    \  if n = 0 then 0 else e (n - 1)\n\
     let tie = fixpoly (feven, fodd)\n\n\
     let rec meven n = if n = 0 then 1 else modd (n - 1)\n\
     and modd n = if n = 0 then 1 else meven (n - 1)\n\
     let main =\n\
    \  let () = print_int (modd 1) in\n\
    \  let () = print_int (meven 2) in\n\
    \  let (even,odd) = tie in\n\
    \  let () = print_int (odd 3) in\n\
    \  let () = print_int (even 4) in\n\
    \  0\n";
  [%expect
    {|
    [(Value (Rec,
        (Binding ((PatVar "fix"),
           (Fun ((PatVar "f"), [(PatVar "x")],
              (App ((App ((Var "f"), (App ((Var "fix"), (Var "f"))))), (Var "x")
                 ))
              ))
           )),
        []));
      (Value (NonRec,
         (Binding ((PatVar "map"),
            (Fun ((PatVar "f"), [(PatVar "p")],
               (Let (NonRec,
                  (Binding ((PatTup ((PatVar "a"), (PatVar "b"), [])), (Var "p")
                     )),
                  [],
                  (Tup ((App ((Var "f"), (Var "a"))),
                     (App ((Var "f"), (Var "b"))), []))
                  ))
               ))
            )),
         []));
      (Value (NonRec,
         (Binding ((PatVar "fixpoly"),
            (Fun ((PatVar "l"), [],
               (App (
                  (App ((Var "fix"),
                     (Fun ((PatVar "self"), [(PatVar "l")],
                        (App (
                           (App ((Var "map"),
                              (Fun ((PatVar "li"), [(PatVar "x")],
                                 (App (
                                    (App ((Var "li"),
                                       (App ((Var "self"), (Var "l"))))),
                                    (Var "x")))
                                 ))
                              )),
                           (Var "l")))
                        ))
                     )),
                  (Var "l")))
               ))
            )),
         []));
      (Value (NonRec,
         (Binding ((PatVar "feven"),
            (Fun ((PatVar "p"), [(PatVar "n")],
               (Let (NonRec,
                  (Binding ((PatTup ((PatVar "e"), (PatVar "o"), [])), (Var "p")
                     )),
                  [],
                  (Branch ((BinOp (Eq, (Var "n"), (Const (Int 0)))),
                     (Const (Int 1)),
                     (App ((Var "o"), (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                     ))
                  ))
               ))
            )),
         []));
      (Value (NonRec,
         (Binding ((PatVar "fodd"),
            (Fun ((PatVar "p"), [(PatVar "n")],
               (Let (NonRec,
                  (Binding ((PatTup ((PatVar "e"), (PatVar "o"), [])), (Var "p")
                     )),
                  [],
                  (Branch ((BinOp (Eq, (Var "n"), (Const (Int 0)))),
                     (Const (Int 0)),
                     (App ((Var "e"), (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                     ))
                  ))
               ))
            )),
         []));
      (Value (NonRec,
         (Binding ((PatVar "tie"),
            (App ((Var "fixpoly"), (Tup ((Var "feven"), (Var "fodd"), [])))))),
         []));
      (Value (Rec,
         (Binding ((PatVar "meven"),
            (Fun ((PatVar "n"), [],
               (Branch ((BinOp (Eq, (Var "n"), (Const (Int 0)))),
                  (Const (Int 1)),
                  (App ((Var "modd"), (BinOp (Sub, (Var "n"), (Const (Int 1))))))
                  ))
               ))
            )),
         [(Binding ((PatVar "modd"),
             (Fun ((PatVar "n"), [],
                (Branch ((BinOp (Eq, (Var "n"), (Const (Int 0)))),
                   (Const (Int 1)),
                   (App ((Var "meven"), (BinOp (Sub, (Var "n"), (Const (Int 1))))
                      ))
                   ))
                ))
             ))
           ]
         ));
      (Value (NonRec,
         (Binding ((PatVar "main"),
            (Let (NonRec,
               (Binding ((PatConst Unit),
                  (App ((Var "print_int"), (App ((Var "modd"), (Const (Int 1))))
                     ))
                  )),
               [],
               (Let (NonRec,
                  (Binding ((PatConst Unit),
                     (App ((Var "print_int"),
                        (App ((Var "meven"), (Const (Int 2))))))
                     )),
                  [],
                  (Let (NonRec,
                     (Binding ((PatTup ((PatVar "even"), (PatVar "odd"), [])),
                        (Var "tie"))),
                     [],
                     (Let (NonRec,
                        (Binding ((PatConst Unit),
                           (App ((Var "print_int"),
                              (App ((Var "odd"), (Const (Int 3))))))
                           )),
                        [],
                        (Let (NonRec,
                           (Binding ((PatConst Unit),
                              (App ((Var "print_int"),
                                 (App ((Var "even"), (Const (Int 4))))))
                              )),
                           [], (Const (Int 0))))
                        ))
                     ))
                  ))
               ))
            )),
         []))
      ] |}]
;;
