(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ETenyaeva_lib.Parser
open ETenyaeva_lib.Ast

let run input =
  match parse input with
  | Ok structure -> Stdlib.Format.printf "%s\n" (show_structure structure)
  | Error err -> Stdlib.Format.printf "%s\n" err
;;

let%expect_test "parse_multiple_bindings" =
  run "let x = 10;; let y = x + 5;;";
  [%expect
    {|
  [(Binding (NonRec, { pat = (PatVar "x"); expr = (ExpConst (Int 10)) }, []));
    (Binding (NonRec,
       { pat = (PatVar "y");
         expr = (ExpBinOper (Add, (ExpVar "x"), (ExpConst (Int 5)))) },
       []))
    ]|}]
;;

let%expect_test "parse_list_construct_case" =
  run "a::b::c::[];;";
  [%expect
    {|
  [(EvalExp
      (ExpListConstructor
         [(ExpVar "a"); (ExpVar "b"); (ExpVar "c"); (ExpList [])]))
    ]
|}]
;;

let%expect_test "parse_application" =
  run "fact (n - 1);;";
  [%expect
    {|
    [(EvalExp 
        (ExpApp ((ExpVar "fact"),
           (ExpBinOper (Sub, (ExpVar "n"), (ExpConst (Int 1)))))))
      ] |}]
;;

let%expect_test "parse_multiple_evaluations" =
  run "1 + 1;; 2 + 2;;";
  [%expect
    {|
    [(EvalExp (ExpBinOper (Add, (ExpConst (Int 1)), (ExpConst (Int 1)))));
      (EvalExp (ExpBinOper (Add, (ExpConst (Int 2)), (ExpConst (Int 2)))))] |}]
;;

let%expect_test "parse_case_expression" =
  run "match x with | 1 -> \"one\" | 2 -> \"two\";;";
  [%expect
    {|
    [(EvalExp
        (ExpMatch ((ExpVar "x"),
           { case_pat = (PatConst (Int 1)); case_expr = (ExpConst (String "one"))
             },
           [{ case_pat = (PatConst (Int 2));
              case_expr = (ExpConst (String "two")) }
             ]
           )))
      ] |}]
;;

let%expect_test "parse_tuple" =
  run "(1, 2, 3);;";
  [%expect
    {|
    [(EvalExp
        (ExpTup ((ExpConst (Int 1)), (ExpConst (Int 2)), [(ExpConst (Int 3))])))
      ] |}]
;;

let%expect_test "parse_eval;;" =
  run "1 + 1";
  [%expect {|
    [(EvalExp (ExpBinOper (Add, (ExpConst (Int 1)), (ExpConst (Int 1)))))] |}]
;;

let%expect_test "parse_sub_without_ws" =
  run "n-1;;";
  [%expect {|
    [(EvalExp (ExpBinOper (Sub, (ExpVar "n"), (ExpConst (Int 1)))))]|}]
;;

let%expect_test "parse_brackets" =
  run "(((1 + 2) * 3));;";
  [%expect
    {|
    [(EvalExp
        (ExpBinOper (Mult,
           (ExpBinOper (Add, (ExpConst (Int 1)), (ExpConst (Int 2)))),
           (ExpConst (Int 3)))))
      ] |}]
;;

let%expect_test "parse_factorial" =
  run "let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;";
  [%expect
    {|
    [(Binding (Rec,
        { pat = (PatVar "fact");
          expr =
          (ExpFun ((PatVar "n"),
             (ExpIfThenElse (
                (ExpBinOper (LessEquals, (ExpVar "n"), (ExpConst (Int 1)))),
                (ExpConst (Int 1)),
                (Some (ExpBinOper (Mult, (ExpVar "n"),
                         (ExpApp ((ExpVar "fact"),
                            (ExpBinOper (Sub, (ExpVar "n"), (ExpConst (Int 1))))
                            ))
                         )))
                ))
             ))
          },
        []))
      ] |}]
;;

let%expect_test "parse_arithmetic" =
  run "1 + 2 * 3;;";
  [%expect
    {|
    [(EvalExp
        (ExpBinOper (Add, (ExpConst (Int 1)),
           (ExpBinOper (Mult, (ExpConst (Int 2)), (ExpConst (Int 3)))))))
      ] |}]
;;

let%expect_test "parse_ifthen" =
  run "let ifthen n = if n > 0 then 1";
  [%expect
    {|
  [(Binding (NonRec,
      { pat = (PatVar "ifthen");
        expr =
        (ExpFun ((PatVar "n"),
           (ExpIfThenElse (
              (ExpBinOper (GreaterThan, (ExpVar "n"), (ExpConst (Int 0)))),
              (ExpConst (Int 1)), None))
           ))
        },
      []))
    ] |}]
;; 

let%expect_test "parse_ifthen_without_else" =
  run "if x > 0 then x + 4 ";
  [%expect
    {|
    [(EvalExp
        (ExpIfThenElse (
           (ExpBinOper (GreaterThan, (ExpVar "x"), (ExpConst (Int 0)))),
           (ExpBinOper (Add, (ExpVar "x"), (ExpConst (Int 4)))), None)))
      ] |}]
;;

let%expect_test "parse_list" =
  run "let lst = [1; 2; 3]";
  [%expect
    {|
  [(Binding (NonRec,
      { pat = (PatVar "lst");
        expr =
        (ExpList [(ExpConst (Int 1)); (ExpConst (Int 2)); (ExpConst (Int 3))])
        },
      []))
    ] |}]
;;

let%expect_test "parse_with_type" =
  run "let (x : int) = 5;;";
  [%expect
    {|
  [(Binding (NonRec,
      { pat = (PatWithTyp (TypInt, (PatVar "x"))); expr = (ExpConst (Int 5)) },
      []))
    ] |}]
;;

let%expect_test "parse_with_type2" =
  run "let add (a : int) (b : int) =
  a + b;;";
  [%expect
    {|
  [(Binding (NonRec,
      { pat = (PatVar "add");
        expr =
        (ExpFun ((PatWithTyp (TypInt, (PatVar "a"))),
           (ExpFun ((PatWithTyp (TypInt, (PatVar "b"))),
              (ExpBinOper (Add, (ExpVar "a"), (ExpVar "b")))))
           ))
        },
      []))
    ] |}]
;;

let%expect_test "fibonacci" =
  run "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2) ;;";
  [%expect
    {|
  [(Binding (Rec,
      { pat = (PatVar "fibo");
        expr =
        (ExpFun ((PatVar "n"),
           (ExpIfThenElse (
              (ExpBinOper (LessThan, (ExpVar "n"), (ExpConst (Int 2)))),
              (ExpConst (Int 1)),
              (Some (ExpBinOper (Add,
                       (ExpApp ((ExpVar "fibo"),
                          (ExpBinOper (Sub, (ExpVar "n"), (ExpConst (Int 1))))
                          )),
                       (ExpApp ((ExpVar "fibo"),
                          (ExpBinOper (Sub, (ExpVar "n"), (ExpConst (Int 2))))
                          ))
                       )))
              ))
           ))
        },
      []))
    ]
|}]
;;

let%expect_test "lambda_test" =
  run "let add x = fun y -> x + y;;";
  [%expect
    {|
  [(Binding (NonRec,
      { pat = (PatVar "add");
        expr =
        (ExpFun ((PatVar "x"),
           (ExpFun ((PatVar "y"),
              (ExpBinOper (Add, (ExpVar "x"), (ExpVar "y")))))
           ))
        },
      []))
    ]
|}]
;;

let%expect_test "lambda_test_2" =
  run "let add x = fun y -> y b;;";
  [%expect
    {|
  [(Binding (NonRec,
      { pat = (PatVar "add");
        expr =
        (ExpFun ((PatVar "x"),
           (ExpFun ((PatVar "y"), (ExpApp ((ExpVar "y"), (ExpVar "b")))))))
        },
      []))
    ]
|}]
;;

let%expect_test "lambda_test_3" =
  run "let foo a =
  let () = print_int a in fun b ->
  let () = print_int b in fun c ->
  print_int c
";
  [%expect
    {|
  [(Binding (NonRec,
      { pat = (PatVar "foo");
        expr =
        (ExpFun ((PatVar "a"),
           (ExpLet (NonRec,
              { pat = (PatConst Unit);
                expr = (ExpApp ((ExpVar "print_int"), (ExpVar "a"))) },
              [],
              (ExpFun ((PatVar "b"),
                 (ExpLet (NonRec,
                    { pat = (PatConst Unit);
                      expr = (ExpApp ((ExpVar "print_int"), (ExpVar "b"))) },
                    [],
                    (ExpFun ((PatVar "c"),
                       (ExpApp ((ExpVar "print_int"), (ExpVar "c")))))
                    ))
                 ))
              ))
           ))
        },
      []))
    ]
|}]
;;

let%expect_test "test_tuple" =
  run "let x = (1, 2, true);;";
  [%expect
    {|
  [(Binding (NonRec,
      { pat = (PatVar "x");
        expr =
        (ExpTup ((ExpConst (Int 1)), (ExpConst (Int 2)),
           [(ExpConst (Bool true))]))
        },
      []))
    ]
|}]
;;

let%expect_test "test_annotate_type" =
  run "let (a : int list) = [] ";
  [%expect
    {|
[(Binding (NonRec,
    { pat = (PatWithTyp (TypInt, (PatVar "a"))); expr = (ExpList []) },
    []))
  ]
|}]
;;

let%expect_test "test_arithmetic2" =
  run "-1 -2 - (-1) -(3)";
  [%expect
    {|
[(EvalExp
    (ExpUnOper (Neg,
       (ExpBinOper (Sub,
          (ExpBinOper (Sub,
             (ExpBinOper (Sub, (ExpConst (Int 1)), (ExpConst (Int 2)))),
             (ExpUnOper (Neg, (ExpConst (Int 1)))))),
          (ExpConst (Int 3))))
       )))
  ]
 |}]
;;

let%expect_test "test_let-match" =
  run "let _5 =
    let id x = x in
    match Some id with
      | Some f -> let _ = f \"42\" in f 42
      | None -> 0";
  [%expect
    {|
[(Binding (NonRec,
    { pat = (PatVar "_5");
      expr =
      (ExpLet (NonRec,
         { pat = (PatVar "id"); expr = (ExpFun ((PatVar "x"), (ExpVar "x")))
           },
         [],
         (ExpMatch ((ExpOption (Some (ExpVar "id"))),
            { case_pat = (PatOption (Some (PatVar "f")));
              case_expr =
              (ExpLet (NonRec,
                 { pat = PatAny;
                   expr = (ExpApp ((ExpVar "f"), (ExpConst (String "42")))) },
                 [], (ExpApp ((ExpVar "f"), (ExpConst (Int 42))))))
              },
            [{ case_pat = (PatOption None); case_expr = (ExpConst (Int 0)) }]
            ))
         ))
      },
    []))
  ]
 |}]
;;

let%expect_test "test_fib" =
  run "let rec fib n =
  if n<2
  then n
  else fib (n - 1) + fib (n - 2)";
  [%expect
    {|
[(Binding (Rec,
    { pat = (PatVar "fib");
      expr =
      (ExpFun ((PatVar "n"),
         (ExpIfThenElse (
            (ExpBinOper (LessThan, (ExpVar "n"), (ExpConst (Int 2)))),
            (ExpVar "n"),
            (Some (ExpBinOper (Add,
                     (ExpApp ((ExpVar "fib"),
                        (ExpBinOper (Sub, (ExpVar "n"), (ExpConst (Int 1))))
                        )),
                     (ExpApp ((ExpVar "fib"),
                        (ExpBinOper (Sub, (ExpVar "n"), (ExpConst (Int 2))))
                        ))
                     )))
            ))
         ))
      },
    []))
  ]
 |}]
;;

let%expect_test "test_partial" =
  run "let foo a b c =
  let () = print_int a in
  let () = print_int b in
  let () = print_int c in
  a + b * c";
  [%expect
    {|
[(Binding (NonRec,
    { pat = (PatVar "foo");
      expr =
      (ExpFun ((PatVar "a"),
         (ExpFun ((PatVar "b"),
            (ExpFun ((PatVar "c"),
               (ExpLet (NonRec,
                  { pat = (PatConst Unit);
                    expr = (ExpApp ((ExpVar "print_int"), (ExpVar "a"))) },
                  [],
                  (ExpLet (NonRec,
                     { pat = (PatConst Unit);
                       expr = (ExpApp ((ExpVar "print_int"), (ExpVar "b"))) },
                     [],
                     (ExpLet (NonRec,
                        { pat = (PatConst Unit);
                          expr =
                          (ExpApp ((ExpVar "print_int"), (ExpVar "c"))) },
                        [],  
                        (ExpBinOper (Add, (ExpVar "a"),
                           (ExpBinOper (Mult, (ExpVar "b"), (ExpVar "c")))))
                        ))
                     ))
                  ))
               ))
            ))
         ))
      },
    []))
  ]
 |}]
;;

let%expect_test "parse_let_and" =
  run {|
  let x = 10 and y = 3 + 5 and z = (1, true, Some [x;y], [1; 2; 3], ("katya", "nastya"));;
  |};
  [%expect
    {|
    [(Binding (NonRec, { pat = (PatVar "x"); expr = (ExpConst (Int 10)) },
        [{ pat = (PatVar "y");
           expr = (ExpBinOper (Add, (ExpConst (Int 3)), (ExpConst (Int 5)))) };
          { pat = (PatVar "z");
            expr =
            (ExpTup ((ExpConst (Int 1)), (ExpConst (Bool true)),
               [(ExpOption (Some (ExpVar "x")));
                 (ExpList
                    [(ExpConst (Int 1)); (ExpConst (Int 2)); (ExpConst (Int 3))]);
                 (ExpTup ((ExpConst (String "katya")),
                    (ExpConst (String "nastya")), []))
                 ]
               ))
            }
          ]
        ))
      ] |}]
;;
