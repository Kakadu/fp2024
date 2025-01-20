(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Parser
open Ocamladt_lib.Ast

(* open Angstrom *)
let test_program str = print_endline (show_program (parse_str str))

let%expect_test "negative int constant" =
  test_program {|-1;;|};
  [%expect
    {|
    [(Str_eval (Exp_apply ((Exp_ident "-"), (Exp_constant (Const_integer 1)))))] |}]
;;

(*good*)
let%expect_test "positive int constant" =
  test_program {|+1;;|};
  [%expect
    {|
    [(Str_eval (Exp_apply ((Exp_ident "+"), (Exp_constant (Const_integer 1)))))] |}]
;;

(*good*)
let%expect_test " nt constant" =
  test_program {|1;;|};
  [%expect {| [(Str_eval (Exp_constant (Const_integer 1)))] |}]
;;

(*good*)
let%expect_test "whitespace befor int constant" =
  test_program {|     1;;|};
  [%expect {| [(Str_eval (Exp_constant (Const_integer 1)))] |}]
;;

(*good*)
let%expect_test "negative zero" =
  test_program {|-0;;|};
  [%expect
    {|
    [(Str_eval (Exp_apply ((Exp_ident "-"), (Exp_constant (Const_integer 0)))))] |}]
;;

(*good*)
let%expect_test "positive zero" =
  test_program {|+0;;|};
  [%expect
    {|
    [(Str_eval (Exp_apply ((Exp_ident "+"), (Exp_constant (Const_integer 0)))))] |}]
;;

(*good*)
let%expect_test "char" =
  test_program {|''';;|};
  [%expect {| [(Str_eval (Exp_constant (Const_char '\'')))] |}]
;;

(*good*)
let%expect_test "zero" =
  test_program {|0;;|};
  [%expect {| [(Str_eval (Exp_constant (Const_integer 0)))] |}]
;;

(*good*)
let%expect_test "substraction" =
  test_program {|5-11;;|};
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
  test_program {|5=5;;|};
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
  test_program {|x = 52;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "="),
           (Exp_tuple ((Exp_ident "x"), (Exp_constant (Const_integer 52)), [])))))
      ] |}]
;;

(*good*)
let%expect_test "multiplication" =
  test_program {|5*5;;|};
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
  test_program {|5-5*1;;|};
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
  test_program {|5*5-1;;|};
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
  test_program {|5*(5-1);;|};
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
  test_program {|(5);;|};
  [%expect {| [(Str_eval (Exp_constant (Const_integer 5)))] |}]
;;

let%expect_test "parenthesis1" =
  test_program {|(5*(5-1));;|};
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
  test_program {|105 * 64 / 27 - 2 * (5*(5-1)) + 47 / 64 - (56 * (57 *4) - 5);;|};
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
  test_program {|1 + (2 + 3);;|};
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
  test_program
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
  test_program {|((5-1)*5);;|};
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
  test_program {|(5*5-1);;|};
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
  test_program {|(1-5*5);;|};
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
  test_program {|( 5-1 );;|};
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
  test_program {|(5,1,2,5);;|};
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
  test_program {|5+'a';;|};
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
  test_program {|let x = 5 in 6;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }, []),
           (Exp_constant (Const_integer 6)))))
      ] |}]
;;

let%expect_test "let assignment" =
  test_program {|let reca = 1;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "reca"); expr = (Exp_constant (Const_integer 1)) }, [])
        ))
      ] |}]
;;

(* TODO *)

let%expect_test "let assignment" =
  test_program {|let Some None = Some 1;;|};
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
  test_program {|let Some Some Some Some Some None = 1;;|};
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
  test_program {|let Some Some Some Some Some None = 1;;|};
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
  test_program {|let rec x = 5 in 6;;|};
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
  test_program {|let rec x = 5 in 7;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Recursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }, []),
           (Exp_constant (Const_integer 7)))))
      ] |}]
;;

let%expect_test "apply" =
  test_program {|x;;|};
  [%expect {| [(Str_eval (Exp_ident "x"))] |}]
;;

let%expect_test "apply without space" =
  test_program {|f(x);;|};
  [%expect {| [(Str_eval (Exp_apply ((Exp_ident "f"), (Exp_ident "x"))))] |}]
;;

let%expect_test "apply num to ident" =
  test_program {|f (x-1);;|};
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
  test_program {|fun x -> y;;|};
  [%expect {| [(Str_eval (Exp_fun (((Pat_var "x"), []), (Exp_ident "y"))))] |}]
;;

let%expect_test "multi pattern fun" =
  test_program {|fun x z -> y;;|};
  [%expect
    {| [(Str_eval (Exp_fun (((Pat_var "x"), [(Pat_var "z")]), (Exp_ident "y"))))] |}]
;;

let%expect_test "multi fun" =
  test_program {|fun p -> fun x -> z;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_fun (((Pat_var "p"), []),
           (Exp_fun (((Pat_var "x"), []), (Exp_ident "z"))))))
      ] |}]
;;

let%expect_test "apply and subtraction" =
  test_program {|f (x-1);;|};
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
  test_program {|let rec x x x x x x x = y and x = 20 in 5;;|};
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
  test_program {|let (a,b) = (a,b);;|};
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
  test_program {|let rec x x x x x x x = y and x = 20;;|};
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
  test_program {|x * f x;;|};
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
  test_program {|let f x = x;;|};
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
  test_program {|let (x : int * int) = (x: int);;|};
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
  test_program {|let (x : int*int) = (x: int*int);;|};
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
  test_program {|let (x : int->int) = (x: int->int);;|};
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
  test_program {|let f x = g a b c;;|};
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
  test_program {|let fact x = fact(x-1);;|};
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
  test_program {|if 5 then 6;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_if ((Exp_constant (Const_integer 5)),
           (Exp_constant (Const_integer 6)), None)))
      ] |}]
;;

let%expect_test "if statement. condition from fact" =
  test_program {|if n = 0 then 1 else 7;;|};
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
  test_program {|let x = if n = 0 then 6 else 7 in 6;;|};
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
  test_program {|let rec fact n = if n = 0 then 1 else n * fact(n-1);;|};
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
  test_program {|let (x: int->char->string -> x *x* x) = 1;;|};
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
  test_program {|let rec a = 1;;|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "a"); expr = (Exp_constant (Const_integer 1)) }, [])))
      ] |}]
;;

let%expect_test "factorial" =
  test_program {|let rec a = 1;;|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "a"); expr = (Exp_constant (Const_integer 1)) }, [])))
      ] |}]
;;

let%expect_test "factorial" =
  test_program {|let reca = 1 in 5;;|};
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
  test_program {|let reca = 1;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "reca"); expr = (Exp_constant (Const_integer 1)) }, [])
        ))
      ] |}]
;;

let%expect_test "_" =
  test_program {|let recgP6Tz_9 = zdghovr and _ = n_4p;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "recgP6Tz_9"); expr = (Exp_ident "zdghovr") },
         [{ pat = Pat_any; expr = (Exp_ident "n_4p") }])
        ))
      ] |}]
;;

let%expect_test "_" =
  test_program {|(f : (Int -> int -> int));;|};
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
  test_program {|let (f:(x)) = 5;;|};
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
  test_program {|function l -> "" | ;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_function
           ({ first = (Pat_var "l"); second = (Exp_constant (Const_string "")) },
            [])))
      ] |}]
;;

let%expect_test "_" =
  test_program {| function
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
  test_program
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
  test_program
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
  test_program {|(p_Ui7_1fX : pT9pj -> O9_dnUo);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_constraint ((Exp_ident "p_Ui7_1fX"),
           (Type_arrow ((Type_var "pT9pj"), (Type_var "O9_dnUo"))))))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|(Kakadu_52) (fun x -> x);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_construct ("Kakadu_52", None)),
           (Exp_fun (((Pat_var "x"), []), (Exp_ident "x"))))))
      ] |}]
;;

let%expect_test "print_endline as an arg" =
  test_program {|let f = print_endline in 
f "Hello";;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "f"); expr = (Exp_ident "print_endline") }, []),
           (Exp_apply ((Exp_ident "f"), (Exp_constant (Const_string "Hello")))))))
      ] |}]
;;

let%expect_test "just let (char)" =
  test_program {|let x = '5';;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_char '5')) }, [])))
      ] |}]
;;

let%expect_test "string print_endline" =
  test_program {|let x = "51" in 
print_endline x;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_string "51")) },
            []),
           (Exp_apply ((Exp_ident "print_endline"), (Exp_ident "x"))))))
      ] |}]
;;

let%expect_test "string print_endline" =
  test_program {|x = "51";;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident "="),
           (Exp_tuple ((Exp_ident "x"), (Exp_constant (Const_string "51")), []))
           )))
      ] |}]
;;

let%expect_test "match case" =
  test_program
    {|let classify n = 
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | _ -> "other"
;;|};
  [%expect
    {|
  [(Str_value (Nonrecursive,
      ({ pat = (Pat_var "classify");
         expr =
         (Exp_fun (((Pat_var "n"), []),
            (Exp_match ((Exp_ident "n"),
               ({ first = (Pat_constant (Const_integer 0));
                  second = (Exp_constant (Const_string "zero")) },
                [{ first = (Pat_constant (Const_integer 1));
                   second = (Exp_constant (Const_string "one")) };
                  { first = Pat_any;
                    second = (Exp_constant (Const_string "other")) }
                  ])
               ))
            ))
         },
       [])
      ))
    ] |}]
;;

let%expect_test "if then case" =
  test_program
    {| let x = 10 in
if x > 5 then print_endline "> 5"
else print_endline "<= 5";;
;; |};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_tests__Parser.test_program in file "tests/parser.ml", line 9, characters 51-66
  Called from Ocamladt_tests__Parser.(fun) in file "tests/parser.ml", line 1123, characters 2-103
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "if then case" =
  test_program
    {| let check_number n =
  if n = 0 then
    print_endline "Zero"
  else if n = 1 then
    print_endline "One"
  else
    print_endline "Other"
in 
check_number 5
;; |};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "check_number");
              expr =
              (Exp_fun (((Pat_var "n"), []),
                 (Exp_if (
                    (Exp_apply ((Exp_ident "="),
                       (Exp_tuple
                          ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                       )),
                    (Exp_apply ((Exp_ident "print_endline"),
                       (Exp_constant (Const_string "Zero")))),
                    (Some (Exp_if (
                             (Exp_apply ((Exp_ident "="),
                                (Exp_tuple
                                   ((Exp_ident "n"),
                                    (Exp_constant (Const_integer 1)), []))
                                )),
                             (Exp_apply ((Exp_ident "print_endline"),
                                (Exp_constant (Const_string "One")))),
                             (Some (Exp_apply ((Exp_ident "print_endline"),
                                      (Exp_constant (Const_string "Other")))))
                             )))
                    ))
                 ))
              },
            []),
           (Exp_apply ((Exp_ident "check_number"),
              (Exp_constant (Const_integer 5))))
           )))
      ] |}]
;;
