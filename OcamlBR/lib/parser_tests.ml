(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Parser
open Ast

let parse str =
  match parse_expr str with
  | Ok ast -> Stdlib.print_endline (show_structure ast)
  | _ -> Stdlib.print_endline "Parsing failed"
;;

let%expect_test "parse factorial" =
  parse "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  [%expect
    {|
 [(SEval
     (Elet (Recursive,
        (Evalue_binding ((PVar (Id "factorial")),
           (Efun ((PVar (Id "n")), [],
              (Eif_then_else (
                 (Ebin_op (Eq, (Evar (Id "n")), (Econst (Int 0)))),
                 (Econst (Int 1)),
                 (Some (Ebin_op (Mult, (Evar (Id "n")),
                          (Efun_application ((Evar (Id "factorial")),
                             (Ebin_op (Sub, (Evar (Id "n")), (Econst (Int 1))
                                ))
                             ))
                          )))
                 ))
              ))
           )),
        [], (Efun_application ((Evar (Id "factorial")), (Econst (Int 5)))))))
   ]
 |}]
;;

let%expect_test "parse calculation sequence" =
  parse "1234 + 676 - 9002 * (52 / 2)";
  [%expect
    {|
  [(SEval
      (Ebin_op (Sub, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 676)))),
         (Ebin_op (Mult, (Econst (Int 9002)),
            (Ebin_op (Div, (Econst (Int 52)), (Econst (Int 2))))))
         )))
    ]
  |}]
;;

let%expect_test "parse complex if-then-else" =
  parse "if 1234 + 1 = 1235 then let x = 4 in x * 2";
  [%expect
    {|
    [(SEval
        (Eif_then_else (
           (Ebin_op (Eq, (Ebin_op (Add, (Econst (Int 1234)), (Econst (Int 1)))),
              (Econst (Int 1235)))),
           (Elet (Non_recursive,
              (Evalue_binding ((PVar (Id "x")), (Econst (Int 4)))), [],
              (Ebin_op (Mult, (Evar (Id "x")), (Econst (Int 2)))))),
           None)))
      ]
  |}]
;;

let%expect_test "parse unallowable range for the int type" =
  parse "39482309482390842309482438208 + 2";
  [%expect {| 
  Parsing failed
  |}]
;;

let%expect_test "parse nested let-in" =
  parse "let x = 5 in let y = 3 in x + y;; if 13 > 12 then let a = 2 in a - 4";
  [%expect
    {|
  [(SEval
      (Elet (Non_recursive,
         (Evalue_binding ((PVar (Id "x")), (Econst (Int 5)))), [],
         (Elet (Non_recursive,
            (Evalue_binding ((PVar (Id "y")), (Econst (Int 3)))), [],
            (Ebin_op (Add, (Evar (Id "x")), (Evar (Id "y"))))))
         )));
    (SEval
       (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
          (Elet (Non_recursive,
             (Evalue_binding ((PVar (Id "a")), (Econst (Int 2)))), [],
             (Ebin_op (Sub, (Evar (Id "a")), (Econst (Int 4)))))),
          None)))
    ] |}]
;;

let%expect_test "parse multiple structure items" =
  parse "let x = 5 ;; if 13 > 12 then let a = 2 in a + x";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding ((PVar (Id "x")), (Econst (Int 5)))), []));
    (SEval
       (Eif_then_else ((Ebin_op (Gt, (Econst (Int 13)), (Econst (Int 12)))),
          (Elet (Non_recursive,
             (Evalue_binding ((PVar (Id "a")), (Econst (Int 2)))), [],
             (Ebin_op (Add, (Evar (Id "a")), (Evar (Id "x")))))),
          None)))
    ] |}]
;;

let%expect_test "parse incorrect pattern-matching" =
  parse "let rec factorial n = match n with 5 0 -> 1 5 1 -> 1 5 _ -> n * factorial(n - 1)";
  [%expect {|
  Parsing failed
  |}]
;;

let%expect_test "parse correct pattern-matching" =
  parse "let x = match 3 with | 1 -> -10 | 2 -> +20 | _ -> 30 ;;";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding ((PVar (Id "x")),
         (Ematch ((Econst (Int 3)),
            (Ecase ((PConst (Int 1)), (Eun_op (Negative, (Econst (Int 10)))))),
            [(Ecase ((PConst (Int 2)), (Eun_op (Positive, (Econst (Int 20))))));
              (Ecase (PAny, (Econst (Int 30))))]
            ))
         )),
      []))
    ]
  |}]
;;

let%expect_test "parse parenthesised expression" =
  parse "(5 + 6) * 4";
  [%expect
    {|
  [(SEval
      (Ebin_op (Mult, (Ebin_op (Add, (Econst (Int 5)), (Econst (Int 6)))),
         (Econst (Int 4)))))
    ]
  |}]
;;

let%expect_test "parse prefix operators" =
  parse "let (|?) a b = a/b + b*a in (|?) 3 ((|?) 5 6)";
  [%expect
    {|
  [(SEval
      (Elet (Non_recursive,
         (Evalue_binding ((PVar (Id "|?")),
            (Efun ((PVar (Id "a")), [(PVar (Id "b"))],
               (Ebin_op (Add,
                  (Ebin_op (Div, (Evar (Id "a")), (Evar (Id "b")))),
                  (Ebin_op (Mult, (Evar (Id "b")), (Evar (Id "a"))))))
               ))
            )),
         [],
         (Efun_application (
            (Efun_application ((Evar (Id "|?")), (Econst (Int 3)))),
            (Efun_application (
               (Efun_application ((Evar (Id "|?")), (Econst (Int 5)))),
               (Econst (Int 6))))
            ))
         )))
    ]
  |}]
;;

let%expect_test "parse match with function keyword" =
  parse "let x = function | [] -> 10 | h::tl -> 20 | h::m::tl -> 30 ;;";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding ((PVar (Id "x")),
         (Efunction ((Ecase ((PList []), (Econst (Int 10)))),
            [(Ecase ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                (Econst (Int 20))));
              (Ecase (
                 (PCons ((PVar (Id "h")),
                    (PCons ((PVar (Id "m")), (PVar (Id "tl")))))),
                 (Econst (Int 30))))
              ]
            ))
         )),
      []))
    ]
  |}]
;;

let%expect_test "parse pattern with arguments" =
  parse "let (w : int) (Some c) (2::v) (a, b, d)  = c";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding ((PConstraint ((PVar (Id "w")), int)),
         (Efun ((POption (Some (PVar (Id "c")))),
            [(PCons ((PConst (Int 2)), (PVar (Id "v"))));
              (PTuple ((PVar (Id "a")), (PVar (Id "b")), [(PVar (Id "d"))]))],
            (Evar (Id "c"))))
         )),
      []))
    ]
  |}]
;;

let%expect_test "parse expr with unary and binary operations" =
  parse
    "let x = not true in let y = 13 in if x || (10 >= y) && (5 <= y) && (y <> 6) || (y < \
     9) && (y > -1000) then +5 :: [] else [10] ;;";
  [%expect
    {|
  [(SEval
      (Elet (Non_recursive,
         (Evalue_binding ((PVar (Id "x")), (Eun_op (Not, (Econst (Bool true))))
            )),
         [],
         (Elet (Non_recursive,
            (Evalue_binding ((PVar (Id "y")), (Econst (Int 13)))), [],
            (Eif_then_else (
               (Ebin_op (And,
                  (Ebin_op (Or,
                     (Ebin_op (And,
                        (Ebin_op (And,
                           (Ebin_op (Or, (Evar (Id "x")),
                              (Ebin_op (Gte, (Econst (Int 10)), (Evar (Id "y"))
                                 ))
                              )),
                           (Ebin_op (Lte, (Econst (Int 5)), (Evar (Id "y")))))),
                        (Ebin_op (Neq, (Evar (Id "y")), (Econst (Int 6)))))),
                     (Ebin_op (Lt, (Evar (Id "y")), (Econst (Int 9)))))),
                  (Ebin_op (Gt, (Evar (Id "y")),
                     (Eun_op (Negative, (Econst (Int 1000))))))
                  )),
               (Ebin_op (Cons, (Eun_op (Positive, (Econst (Int 5)))),
                  (Elist []))),
               (Some (Elist [(Econst (Int 10))]))))
            ))
         )))
    ]
  |}]
;;

let%expect_test "parse multiple patterns" =
  parse "let a = Some 4 in let b = (c, [], not true) in c :: [a]";
  [%expect
    {|
  [(SEval
      (Elet (Non_recursive,
         (Evalue_binding ((PVar (Id "a")), (Eoption (Some (Econst (Int 4)))))),
         [],
         (Elet (Non_recursive,
            (Evalue_binding ((PVar (Id "b")),
               (Etuple ((Evar (Id "c")), (Elist []),
                  [(Eun_op (Not, (Econst (Bool true))))]))
               )),
            [], (Ebin_op (Cons, (Evar (Id "c")), (Elist [(Evar (Id "a"))])))))
         )))
    ]
  |}]
;;

let%expect_test "parse expr with constraint" =
  parse "let addi = fun f g x -> (f x (g x: bool) : int) ";
  [%expect
    {|
  [(SValue (Non_recursive,
      (Evalue_binding ((PVar (Id "addi")),
         (Efun ((PVar (Id "f")), [(PVar (Id "g")); (PVar (Id "x"))],
            (Econstraint (
               (Efun_application (
                  (Efun_application ((Evar (Id "f")), (Evar (Id "x")))),
                  (Econstraint (
                     (Efun_application ((Evar (Id "g")), (Evar (Id "x")))),
                     bool))
                  )),
               int))
            ))
         )),
      []))
    ]
     |}]
;;
