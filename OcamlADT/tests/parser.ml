(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Parser
open Ocamladt_lib.Ast

(* open Angstrom *)
let test_programm str = print_endline (show_program (parse_str str))

let%expect_test "negative int constant" =
  test_programm {|-1;;|};
  [%expect
    {|
    [(Str_eval (Exp_apply ((Exp_ident "-"), (Exp_constant (Const_integer 1)))))] |}]
;;

(*good*)
let%expect_test "positive int constant" =
  test_programm {|+1;;|};
  [%expect
    {|
    [(Str_eval (Exp_apply ((Exp_ident "+"), (Exp_constant (Const_integer 1)))))] |}]
;;

(*good*)
let%expect_test " nt constant" =
  test_programm {|1;;|};
  [%expect {| [(Str_eval (Exp_constant (Const_integer 1)))] |}]
;;

(*good*)
let%expect_test "whitespace befor int constant" =
  test_programm {|     1;;|};
  [%expect {| [(Str_eval (Exp_constant (Const_integer 1)))] |}]
;;

(*good*)
let%expect_test "negative zero" =
  test_programm {|-0;;|};
  [%expect
    {|
    [(Str_eval (Exp_apply ((Exp_ident "-"), (Exp_constant (Const_integer 0)))))] |}]
;;

(*good*)
let%expect_test "positive zero" =
  test_programm {|+0;;|};
  [%expect
    {|
    [(Str_eval (Exp_apply ((Exp_ident "+"), (Exp_constant (Const_integer 0)))))] |}]
;;

(*good*)
let%expect_test "char" =
  test_programm {|''';;|};
  [%expect {| [(Str_eval (Exp_constant (Const_char '\'')))] |}]
;;

(*good*)
let%expect_test "zero" =
  test_programm {|0;;|};
  [%expect {| [(Str_eval (Exp_constant (Const_integer 0)))] |}]
;;

(*good*)
let%expect_test "substraction" =
  test_programm {|5-11;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)),
               (Exp_constant (Const_integer 11)), []))
           )))
      ] |}]
;;

(*good*)
let%expect_test "strange move" =
  test_programm {|5=5;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "="),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)),
               (Exp_constant (Const_integer 5)), []))
           )))
      ] |}]
;;

(*good*)
let%expect_test "(assignment)" =
  test_programm {|x = 52;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "="),
           (Exp_tuple ((Exp_ident "x"), (Exp_constant (Const_integer 52)), [])))))
      ] |}]
;;

(*good*)
let%expect_test "multiplication" =
  test_programm {|5*5;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "*"),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)),
               (Exp_constant (Const_integer 5)), []))
           )))
      ] |}]
;;

(*good*)
let%expect_test "operators with different priorities" =
  test_programm {|5-5*1;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)),
               (Exp_apply ((Exp_ident "*"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 5)),
                      (Exp_constant (Const_integer 1)), []))
                  )),
               []))
           )))
      ] |}]
;;

(*good*)
let%expect_test "operators with different priorities" =
  test_programm {|5*5-1;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple
              ((Exp_apply ((Exp_ident "*"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 5)),
                      (Exp_constant (Const_integer 5)), []))
                  )),
               (Exp_constant (Const_integer 1)), []))
           )))
      ] |}]
;;

(*good*)

let%expect_test "parenthesis with operators with different priorities" =
  test_programm {|5*(5-1);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "*"),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)),
               (Exp_apply ((Exp_ident "-"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 5)),
                      (Exp_constant (Const_integer 1)), []))
                  )),
               []))
           )))
      ] |}]
;;

let%expect_test "parenthesis3" =
  test_programm {|(5);;|};
  [%expect {| [(Str_eval (Exp_constant (Const_integer 5)))] |}]
;;

let%expect_test "parenthesis1" =
  test_programm {|(5*(5-1));;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "*"),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)),
               (Exp_apply ((Exp_ident "-"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 5)),
                      (Exp_constant (Const_integer 1)), []))
                  )),
               []))
           )))
      ] |}]
;;

let%expect_test "parenthesis2" =
  test_programm {|105 * 64 / 27 - 2 * (5*(5-1)) + 47 / 64 - (56 * (57 *4) - 5);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple
              ((Exp_apply ((Exp_ident "+"),
                  (Exp_tuple
                     ((Exp_apply ((Exp_ident "-"),
                         (Exp_tuple
                            ((Exp_apply ((Exp_ident "/"),
                                (Exp_tuple
                                   ((Exp_apply ((Exp_ident "*"),
                                       (Exp_tuple
                                          ((Exp_constant (Const_integer 105)),
                                           (Exp_constant (Const_integer 64)),
                                           []))
                                       )),
                                    (Exp_constant (Const_integer 27)), []))
                                )),
                             (Exp_apply ((Exp_ident "*"),
                                (Exp_tuple
                                   ((Exp_constant (Const_integer 2)),
                                    (Exp_apply ((Exp_ident "*"),
                                       (Exp_tuple
                                          ((Exp_constant (Const_integer 5)),
                                           (Exp_apply ((Exp_ident "-"),
                                              (Exp_tuple
                                                 ((Exp_constant (Const_integer 5)),
                                                  (Exp_constant (Const_integer 1)),
                                                  []))
                                              )),
                                           []))
                                       )),
                                    []))
                                )),
                             []))
                         )),
                      (Exp_apply ((Exp_ident "/"),
                         (Exp_tuple
                            ((Exp_constant (Const_integer 47)),
                             (Exp_constant (Const_integer 64)), []))
                         )),
                      []))
                  )),
               (Exp_apply ((Exp_ident "-"),
                  (Exp_tuple
                     ((Exp_apply ((Exp_ident "*"),
                         (Exp_tuple
                            ((Exp_constant (Const_integer 56)),
                             (Exp_apply ((Exp_ident "*"),
                                (Exp_tuple
                                   ((Exp_constant (Const_integer 57)),
                                    (Exp_constant (Const_integer 4)), []))
                                )),
                             []))
                         )),
                      (Exp_constant (Const_integer 5)), []))
                  )),
               []))
           )))
      ] |}]
;;

let%expect_test "parenthesis3" =
  test_programm {|1 + (2 + 3);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "+"),
           (Exp_tuple
              ((Exp_constant (Const_integer 1)),
               (Exp_apply ((Exp_ident "+"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 2)),
                      (Exp_constant (Const_integer 3)), []))
                  )),
               []))
           )))
      ] |}]
;;

let%expect_test "logical ops + parenthesis" =
  test_programm
    {|
    ((3 * (9 - 12 / 4) < 7 && 1) || 1 && 5 < 6) || 20 - 100 / (4 + 16) && 10 < 12 ;; 
|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "&&"),
           (Exp_tuple
              ((Exp_apply ((Exp_ident "||"),
                  (Exp_tuple
                     ((Exp_apply ((Exp_ident "&&"),
                         (Exp_tuple
                            ((Exp_apply ((Exp_ident "||"),
                                (Exp_tuple
                                   ((Exp_apply ((Exp_ident "&&"),
                                       (Exp_tuple
                                          ((Exp_apply ((Exp_ident "<"),
                                              (Exp_tuple
                                                 ((Exp_apply ((Exp_ident "*"),
                                                     (Exp_tuple
                                                        ((Exp_constant
                                                            (Const_integer 3)),
                                                         (Exp_apply (
                                                            (Exp_ident "-"),
                                                            (Exp_tuple
                                                               ((Exp_constant
                                                                   (Const_integer
                                                                      9)),
                                                                (Exp_apply (
                                                                   (Exp_ident "/"),
                                                                   (Exp_tuple
                                                                      ((Exp_constant
                                                                        (Const_integer
                                                                        12)),
                                                                       (Exp_constant
                                                                        (Const_integer
                                                                        4)),
                                                                       []))
                                                                   )),
                                                                []))
                                                            )),
                                                         []))
                                                     )),
                                                  (Exp_constant (Const_integer 7)),
                                                  []))
                                              )),
                                           (Exp_constant (Const_integer 1)),
                                           []))
                                       )),
                                    (Exp_constant (Const_integer 1)), []))
                                )),
                             (Exp_apply ((Exp_ident "<"),
                                (Exp_tuple
                                   ((Exp_constant (Const_integer 5)),
                                    (Exp_constant (Const_integer 6)), []))
                                )),
                             []))
                         )),
                      (Exp_apply ((Exp_ident "-"),
                         (Exp_tuple
                            ((Exp_constant (Const_integer 20)),
                             (Exp_apply ((Exp_ident "/"),
                                (Exp_tuple
                                   ((Exp_constant (Const_integer 100)),
                                    (Exp_apply ((Exp_ident "+"),
                                       (Exp_tuple
                                          ((Exp_constant (Const_integer 4)),
                                           (Exp_constant (Const_integer 16)),
                                           []))
                                       )),
                                    []))
                                )),
                             []))
                         )),
                      []))
                  )),
               (Exp_apply ((Exp_ident "<"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 10)),
                      (Exp_constant (Const_integer 12)), []))
                  )),
               []))
           )))
      ] |}]
;;

let%expect_test "parenthesis4" =
  test_programm {|((5-1)*5);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "*"),
           (Exp_tuple
              ((Exp_apply ((Exp_ident "-"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 5)),
                      (Exp_constant (Const_integer 1)), []))
                  )),
               (Exp_constant (Const_integer 5)), []))
           )))
      ] |}]
;;

let%expect_test "parenthesis5" =
  test_programm {|(5*5-1);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple
              ((Exp_apply ((Exp_ident "*"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 5)),
                      (Exp_constant (Const_integer 5)), []))
                  )),
               (Exp_constant (Const_integer 1)), []))
           )))
      ] |}]
;;

let%expect_test "parenthesis5" =
  test_programm {|(1-5*5);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple
              ((Exp_constant (Const_integer 1)),
               (Exp_apply ((Exp_ident "*"),
                  (Exp_tuple
                     ((Exp_constant (Const_integer 5)),
                      (Exp_constant (Const_integer 5)), []))
                  )),
               []))
           )))
      ] |}]
;;

(* +(+(1, 2), 3) *)

(*bad*)
let%expect_test "parenthesis2" =
  test_programm {|( 5-1 );;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)),
               (Exp_constant (Const_integer 1)), []))
           )))
      ] |}]
;;

(* good fr *)
let%expect_test "tuple" =
  test_programm {|(5,1,2,5);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_tuple
           ((Exp_constant (Const_integer 5)), (Exp_constant (Const_integer 1)),
            [(Exp_constant (Const_integer 2)); (Exp_constant (Const_integer 5))])))
      ] |}]
;;

(* good fr *)
let%expect_test "int + a" =
  test_programm {|5+'a';;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "+"),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)), (Exp_constant (Const_char 'a')),
               []))
           )))
      ] |}]
;;

let%expect_test "let assignment" =
  test_programm {|let x = 5 in 6;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }, []),
           (Exp_constant (Const_integer 6)))))
      ] |}]
;;

let%expect_test "let assignment" =
  test_programm {|let reca = 1;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "reca"); expr = (Exp_constant (Const_integer 1)) }, [])
        ))
      ] |}]
;;

(* TODO *)

let%expect_test "let assignment" =
  test_programm {|let Some None = Some 1;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_construct ("Some", (Some (Pat_construct ("None", None)))));
           expr =
           (Exp_construct ("Some", (Some (Exp_constant (Const_integer 1))))) },
         [])
        ))
      ] |}]
;;

let%expect_test "let assignment none" =
  test_programm {|let Some Some Some Some Some None = 1;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_construct ("Some",
              (Some (Pat_construct ("Some",
                       (Some (Pat_construct ("Some",
                                (Some (Pat_construct ("Some",
                                         (Some (Pat_construct ("Some",
                                                  (Some (Pat_construct ("None",
                                                           None)))
                                                  )))
                                         )))
                                )))
                       )))
              ));
           expr = (Exp_constant (Const_integer 1)) },
         [])
        ))
      ] |}]
;;

let%expect_test "let assignment none" =
  test_programm {|let Some Some Some Some Some None = 1;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_construct ("Some",
              (Some (Pat_construct ("Some",
                       (Some (Pat_construct ("Some",
                                (Some (Pat_construct ("Some",
                                         (Some (Pat_construct ("Some",
                                                  (Some (Pat_construct ("None",
                                                           None)))
                                                  )))
                                         )))
                                )))
                       )))
              ));
           expr = (Exp_constant (Const_integer 1)) },
         [])
        ))
      ] |}]
;;

let%expect_test "let assignment with recursion" =
  test_programm {|let rec x = 5 in 6;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Recursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }, []),
           (Exp_constant (Const_integer 6)))))
      ] |}]
;;

(*bad*)
let%expect_test "let assignment with recursion" =
  test_programm {|let rec x = 5 in 7;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Recursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }, []),
           (Exp_constant (Const_integer 7)))))
      ] |}]
;;

let%expect_test "apply" =
  test_programm {|x;;|};
  [%expect {| [(Str_eval (Exp_ident "x"))] |}]
;;

let%expect_test "apply without space" =
  test_programm {|f(x);;|};
  [%expect {| [(Str_eval (Exp_apply ((Exp_ident "f"), (Exp_ident "x"))))] |}]
;;

let%expect_test "apply num to ident" =
  test_programm {|f (x-1);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "f"),
           (Exp_apply ((Exp_ident "-"),
              (Exp_tuple ((Exp_ident "x"), (Exp_constant (Const_integer 1)), []))
              ))
           )))
      ] |}]
;;

let%expect_test "simple fun" =
  test_programm {|fun x -> y;;|};
  [%expect {| [(Str_eval (Exp_fun (((Pat_var "x"), []), (Exp_ident "y"))))] |}]
;;

let%expect_test "multi pattern fun" =
  test_programm {|fun x z -> y;;|};
  [%expect
    {| [(Str_eval (Exp_fun (((Pat_var "x"), [(Pat_var "z")]), (Exp_ident "y"))))] |}]
;;

let%expect_test "multi fun" =
  test_programm {|fun p -> fun x -> z;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_fun (((Pat_var "p"), []),
           (Exp_fun (((Pat_var "x"), []), (Exp_ident "z"))))))
      ] |}]
;;

let%expect_test "apply and subtraction" =
  test_programm {|f (x-1);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "f"),
           (Exp_apply ((Exp_ident "-"),
              (Exp_tuple ((Exp_ident "x"), (Exp_constant (Const_integer 1)), []))
              ))
           )))
      ] |}]
;;

let%expect_test "exprlet and" =
  test_programm {|let rec x x x x x x x = y and x = 20 in 5;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Recursive,
           ({ pat = (Pat_var "x");
              expr =
              (Exp_fun (
                 ((Pat_var "x"),
                  [(Pat_var "x"); (Pat_var "x"); (Pat_var "x"); (Pat_var "x");
                    (Pat_var "x")]),
                 (Exp_ident "y")))
              },
            [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 20)) }]),
           (Exp_constant (Const_integer 5)))))
      ] |}]
;;

let%expect_test "let and tuple" =
  test_programm {|let (a,b) = (a,b);;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_tuple ((Pat_var "a"), (Pat_var "b"), []));
           expr = (Exp_tuple ((Exp_ident "a"), (Exp_ident "b"), [])) },
         [])
        ))
      ] |}]
;;

let%expect_test "let and" =
  test_programm {|let rec x x x x x x x = y and x = 20;;|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "x");
           expr =
           (Exp_fun (
              ((Pat_var "x"),
               [(Pat_var "x"); (Pat_var "x"); (Pat_var "x"); (Pat_var "x");
                 (Pat_var "x")]),
              (Exp_ident "y")))
           },
         [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 20)) }])
        ))
      ] |}]
;;

let%expect_test "multiplication and apply" =
  test_programm {|x * f x;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "*"),
           (Exp_tuple
              ((Exp_ident "x"), (Exp_apply ((Exp_ident "f"), (Exp_ident "x"))),
               []))
           )))
      ] |}]
;;

let%expect_test "let and apply" =
  test_programm {|let f x = x;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "f");
           expr = (Exp_fun (((Pat_var "x"), []), (Exp_ident "x"))) },
         [])
        ))
      ] |}]
;;

let%expect_test "pattern constraint" =
  test_programm {|let (x : int * int) = (x: int);;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_constraint ((Pat_var "x"),
              (Type_tuple ((Type_var "int"), (Type_var "int"), []))));
           expr = (Exp_constraint ((Exp_ident "x"), (Type_var "int"))) },
         [])
        ))
      ] |}]
;;

let%expect_test "pattern constraint" =
  test_programm {|let (x : int*int) = (x: int*int);;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_constraint ((Pat_var "x"),
              (Type_tuple ((Type_var "int"), (Type_var "int"), []))));
           expr =
           (Exp_constraint ((Exp_ident "x"),
              (Type_tuple ((Type_var "int"), (Type_var "int"), []))))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "pattern constraint" =
  test_programm {|let (x : int->int) = (x: int->int);;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_constraint ((Pat_var "x"),
              (Type_arrow ((Type_var "int"), (Type_var "int")))));
           expr =
           (Exp_constraint ((Exp_ident "x"),
              (Type_arrow ((Type_var "int"), (Type_var "int")))))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "let and apply" =
  test_programm {|let f x = g a b c;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "f");
           expr =
           (Exp_fun (((Pat_var "x"), []),
              (Exp_apply (
                 (Exp_apply ((Exp_apply ((Exp_ident "g"), (Exp_ident "a"))),
                    (Exp_ident "b"))),
                 (Exp_ident "c")))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "let and apply v2" =
  test_programm {|let fact x = fact(x-1);;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "fact");
           expr =
           (Exp_fun (((Pat_var "x"), []),
              (Exp_apply ((Exp_ident "fact"),
                 (Exp_apply ((Exp_ident "-"),
                    (Exp_tuple
                       ((Exp_ident "x"), (Exp_constant (Const_integer 1)), []))
                    ))
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "if then" =
  test_programm {|if 5 then 6;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_if ((Exp_constant (Const_integer 5)),
           (Exp_constant (Const_integer 6)), None)))
      ] |}]
;;

let%expect_test "if statement. condition from fact" =
  test_programm {|if n = 0 then 1 else 7;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_if (
           (Exp_apply ((Exp_ident "="),
              (Exp_tuple ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
              )),
           (Exp_constant (Const_integer 1)),
           (Some (Exp_constant (Const_integer 7))))))
      ] |}]
;;

let%expect_test "let and if" =
  test_programm {|let x = if n = 0 then 6 else 7 in 6;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "x");
              expr =
              (Exp_if (
                 (Exp_apply ((Exp_ident "="),
                    (Exp_tuple
                       ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                    )),
                 (Exp_constant (Const_integer 6)),
                 (Some (Exp_constant (Const_integer 7)))))
              },
            []),
           (Exp_constant (Const_integer 6)))))
      ] |}]
;;

let%expect_test "factorial" =
  test_programm {|let rec fact n = if n = 0 then 1 else n * fact(n-1);;|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "fact");
           expr =
           (Exp_fun (((Pat_var "n"), []),
              (Exp_if (
                 (Exp_apply ((Exp_ident "="),
                    (Exp_tuple
                       ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                    )),
                 (Exp_constant (Const_integer 1)),
                 (Some (Exp_apply ((Exp_ident "*"),
                          (Exp_tuple
                             ((Exp_ident "n"),
                              (Exp_apply ((Exp_ident "fact"),
                                 (Exp_apply ((Exp_ident "-"),
                                    (Exp_tuple
                                       ((Exp_ident "n"),
                                        (Exp_constant (Const_integer 1)),
                                        []))
                                    ))
                                 )),
                              []))
                          )))
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "factorial" =
  test_programm {|let (x: int->char->string -> x *x* x) = 1;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_constraint ((Pat_var "x"),
              (Type_arrow (
                 (Type_arrow ((Type_arrow ((Type_var "int"), (Type_var "char"))),
                    (Type_var "string"))),
                 (Type_tuple ((Type_var "x"), (Type_var "x"), [(Type_var "x")]))
                 ))
              ));
           expr = (Exp_constant (Const_integer 1)) },
         [])
        ))
      ] |}]
;;

let%expect_test "factorial" =
  test_programm {|let rec a = 1;;|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "a"); expr = (Exp_constant (Const_integer 1)) }, [])))
      ] |}]
;;

let%expect_test "factorial" =
  test_programm {|let rec a = 1;;|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "a"); expr = (Exp_constant (Const_integer 1)) }, [])))
      ] |}]
;;

let%expect_test "factorial" =
  test_programm {|let reca = 1 in 5;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "reca"); expr = (Exp_constant (Const_integer 1)) },
            []),
           (Exp_constant (Const_integer 5)))))
      ] |}]
;;

let%expect_test "factorial" =
  test_programm {|let reca = 1;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "reca"); expr = (Exp_constant (Const_integer 1)) }, [])
        ))
      ] |}]
;;

let%expect_test "_" =
  test_programm {|let recgP6Tz_9 = zdghovr and _ = n_4p;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "recgP6Tz_9"); expr = (Exp_ident "zdghovr") },
         [{ pat = Pat_any; expr = (Exp_ident "n_4p") }])
        ))
      ] |}]
;;

let%expect_test "_" =
  test_programm {|(f : (Int -> int -> int));;|};
  [%expect
    {|
    [(Str_eval
        (Exp_constraint ((Exp_ident "f"),
           (Type_arrow ((Type_arrow ((Type_var "Int"), (Type_var "int"))),
              (Type_var "int")))
           )))
      ] |}]
;;

let%expect_test "_" =
  test_programm {|let (f:(x)) = 5;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_constraint ((Pat_var "f"), (Type_var "x")));
           expr = (Exp_constant (Const_integer 5)) },
         [])
        ))
      ] |}]
;;

let%expect_test "_" =
  test_programm {|function l -> "" | ;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_function
           ({ first = (Pat_var "l"); second = (Exp_constant (Const_string "")) },
            [])))
      ] |}]
;;

let%expect_test "_" =
  test_programm {| function
| "" -> 'a'
| "" -> "izvkvwcet" ;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_function
           ({ first = (Pat_constant (Const_string ""));
              second = (Exp_constant (Const_char 'a')) },
            [{ first = (Pat_constant (Const_string ""));
               second = (Exp_constant (Const_string "izvkvwcet")) }
              ])))
      ] |}]
;;

let%expect_test "_" =
  test_programm
    {| (_kR__E2RrhRf_Ln_n_KbLPf97J__gCp1G5T3rOo_7_S5__yRY1377LAU3U_9m_u_yl_wYFdTj5_q_S_9k_JC : EE7___Oe_) ;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_constraint (
           (Exp_ident
              "_kR__E2RrhRf_Ln_n_KbLPf97J__gCp1G5T3rOo_7_S5__yRY1377LAU3U_9m_u_yl_wYFdTj5_q_S_9k_JC"),
           (Type_var "EE7___Oe_"))))
      ] |}]
;;

let%expect_test "_" =
  test_programm
    {|('v' : (SqEcf8boz* L58r6D_P_bX___yy_93GPH__04_r___d9Zc_1U2__c8XmN1n_F_WBqxl68h_8_TCGqp3B_5w_Y_53a6_d_6_H9845__c5__09s* Sh__7ud_43* E_KKm_z3r5__jHMLw_qd1760R_G__nI6_J040__AB_6s0__D__d__e32Te6H_4__Ec_V_E__f_* o0_a_W_* f__LcPREH13__mY_CezffoI5_8_u_zU__ZncOnf_v4_L8_44Y72_3_A5_B758TViP_u_vyFU9_1* QD0* g4wp33A_W* E1V_gi_6y* x_Sv_PZ)) ;; |};
  [%expect
    {|
    [(Str_eval
        (Exp_constraint ((Exp_constant (Const_char 'v')),
           (Type_tuple
              ((Type_var "SqEcf8boz"),
               (Type_var
                  "L58r6D_P_bX___yy_93GPH__04_r___d9Zc_1U2__c8XmN1n_F_WBqxl68h_8_TCGqp3B_5w_Y_53a6_d_6_H9845__c5__09s"),
               [(Type_var "Sh__7ud_43");
                 (Type_var
                    "E_KKm_z3r5__jHMLw_qd1760R_G__nI6_J040__AB_6s0__D__d__e32Te6H_4__Ec_V_E__f_");
                 (Type_var "o0_a_W_");
                 (Type_var
                    "f__LcPREH13__mY_CezffoI5_8_u_zU__ZncOnf_v4_L8_44Y72_3_A5_B758TViP_u_vyFU9_1");
                 (Type_var "QD0"); (Type_var "g4wp33A_W");
                 (Type_var "E1V_gi_6y"); (Type_var "x_Sv_PZ")]))
           )))
      ] |}]
;;

let%expect_test "_" =
  test_programm {|(p_Ui7_1fX : pT9pj -> O9_dnUo);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_constraint ((Exp_ident "p_Ui7_1fX"),
           (Type_arrow ((Type_var "pT9pj"), (Type_var "O9_dnUo"))))))
      ] |}]
;;

let%expect_test "keyword" =
  test_programm {|(Kakadu_52) (fun x -> x);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_construct ("Kakadu_52", None)),
           (Exp_fun (((Pat_var "x"), []), (Exp_ident "x"))))))
      ] |}]
;;
