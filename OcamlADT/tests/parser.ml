(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Parser
open Ocamladt_lib.Ast

(* open Angstrom *)
let test_program str = print_endline (show_program (parse_str str))

let%expect_test "negative int constant" =
  test_program {|-1;;|};
  [%expect
    {| [(Str_eval (Exp_apply ((Exp_ident "-"), (Exp_constant (Const_integer 1)))))] |}]
;;

(*good*)
let%expect_test "positive int constant" =
  test_program {|+1;;|};
  [%expect
    {| [(Str_eval (Exp_apply ((Exp_ident "+"), (Exp_constant (Const_integer 1)))))] |}]
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
    {| [(Str_eval (Exp_apply ((Exp_ident "-"), (Exp_constant (Const_integer 0)))))] |}]
;;

(*good*)
let%expect_test "positive zero" =
  test_program {|+0;;|};
  [%expect
    {| [(Str_eval (Exp_apply ((Exp_ident "+"), (Exp_constant (Const_integer 0)))))] |}]
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

let%expect_test "whitespace befor int constant" =
  test_program
    {|    let x = 10 in
if x > 5 then print_endline "> 5"
else print_endline "<= 5";;
 5+5;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 10)) }, []),
           (Exp_if (
              (Exp_apply ((Exp_ident ">"),
                 (Exp_tuple
                    ((Exp_ident "x"), (Exp_constant (Const_integer 5)), []))
                 )),
              (Exp_apply ((Exp_ident "print_endline"),
                 (Exp_constant (Const_string "> 5")))),
              (Some (Exp_apply ((Exp_ident "print_endline"),
                       (Exp_constant (Const_string "<= 5")))))
              ))
           )));
      (Str_eval
         (Exp_apply ((Exp_ident "+"),
            (Exp_tuple
               ((Exp_constant (Const_integer 5)),
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
  test_program {|fun x -> y;;|};
  [%expect {| [(Str_eval (Exp_fun (((Pat_var "x"), []), (Exp_ident "y"))))] |}]
;;

let%expect_test "multi pattern fun" =
  test_program {|5>5;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_ident ">"),
           (Exp_tuple
              ((Exp_constant (Const_integer 5)),
               (Exp_constant (Const_integer 5)), []))
           )))
      ] |}]
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
  test_program {|let x = 5 and y = 10;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) },
         [{ pat = (Pat_var "y"); expr = (Exp_constant (Const_integer 10)) }])
        ))
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
  test_program {|let (a,b) = (b,a);;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_tuple ((Pat_var "a"), (Pat_var "b"), []));
           expr = (Exp_tuple ((Exp_ident "b"), (Exp_ident "a"), [])) },
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
              (Type_tuple
                 ((Type_construct ("int", [])), (Type_construct ("int", [])), []))
              ));
           expr =
           (Exp_constraint ((Exp_ident "x"), (Type_construct ("int", [])))) },
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
              (Type_tuple
                 ((Type_construct ("int", [])), (Type_construct ("int", [])), []))
              ));
           expr =
           (Exp_constraint ((Exp_ident "x"),
              (Type_tuple
                 ((Type_construct ("int", [])), (Type_construct ("int", [])), []))
              ))
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
              (Type_arrow ((Type_construct ("int", [])),
                 (Type_construct ("int", []))))
              ));
           expr =
           (Exp_constraint ((Exp_ident "x"),
              (Type_arrow ((Type_construct ("int", [])),
                 (Type_construct ("int", []))))
              ))
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
                 (Type_arrow (
                    (Type_arrow ((Type_construct ("int", [])),
                       (Type_construct ("char", [])))),
                    (Type_construct ("string", [])))),
                 (Type_tuple
                    ((Type_construct ("x", [])), (Type_construct ("x", [])),
                     [(Type_construct ("x", []))]))
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

(*good*)
let%expect_test "_" =
  test_program {|(f : (int -> int -> int));;|};
  [%expect
    {|
    [(Str_eval
        (Exp_constraint ((Exp_ident "f"),
           (Type_arrow (
              (Type_arrow ((Type_construct ("int", [])),
                 (Type_construct ("int", [])))),
              (Type_construct ("int", []))))
           )))
      ] |}]
;;

let%expect_test "_" =
  test_program {|let (f:(x)) = 5;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_constraint ((Pat_var "f"), (Type_construct ("x", []))));
           expr = (Exp_constant (Const_integer 5)) },
         [])
        ))
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

(*good*)
let%expect_test "_" =
  test_program
    {|('v' : (sqEcf8boz* s58r6D_P_bX___yy_93GPH__04_r___d9Zc_1U2__c8XmN1n_F_WBqxl68h_8_TCGqp3B_5w_Y_53a6_d_6_H9845__c5__09s* sh__7ud_43* s_KKm_z3r5__jHMLw_qd1760R_G__nI6_J040__AB_6s0__D__d__e32Te6H_4__Ec_V_E__f_* o0_a_W_* f__LcPREH13__mY_CezffoI5_8_u_zU__ZncOnf_v4_L8_44Y72_3_A5_B758TViP_u_vyFU9_1* qD0* g4wp33A_W* e1V_gi_6y* x_Sv_PZ)) ;; |};
  [%expect
    {|
    [(Str_eval
        (Exp_constraint ((Exp_constant (Const_char 'v')),
           (Type_tuple
              ((Type_construct ("sqEcf8boz", [])),
               (Type_construct (
                  "s58r6D_P_bX___yy_93GPH__04_r___d9Zc_1U2__c8XmN1n_F_WBqxl68h_8_TCGqp3B_5w_Y_53a6_d_6_H9845__c5__09s",
                  [])),
               [(Type_construct ("sh__7ud_43", []));
                 (Type_construct (
                    "s_KKm_z3r5__jHMLw_qd1760R_G__nI6_J040__AB_6s0__D__d__e32Te6H_4__Ec_V_E__f_",
                    []));
                 (Type_construct ("o0_a_W_", []));
                 (Type_construct (
                    "f__LcPREH13__mY_CezffoI5_8_u_zU__ZncOnf_v4_L8_44Y72_3_A5_B758TViP_u_vyFU9_1",
                    []));
                 (Type_construct ("qD0", []));
                 (Type_construct ("g4wp33A_W", []));
                 (Type_construct ("e1V_gi_6y", []));
                 (Type_construct ("x_Sv_PZ", []))]))
           )))
      ] |}]
;;

let%expect_test "not keyword" =
  test_program {|(Kakadu_52) (fun x -> x);;|};
  [%expect
    {|
    [(Str_eval
        (Exp_apply ((Exp_construct ("Kakadu_52", None)),
           (Exp_fun (((Pat_var "x"), []), (Exp_ident "x"))))))
      ] |}]
;;

let%expect_test "adt v0" =
  test_program {|type shape = Circle;;|};
  [%expect {| [(Str_adt ([], "shape", (("Circle", None), [])))] |}]
;;

let%expect_test "adt v1" =
  test_program {|type shape = Circle | Square of int;;|};
  [%expect
    {|
    [(Str_adt ([], "shape",
        (("Circle", None), [("Square", (Some (Type_construct ("int", []))))])))
      ] |}]
;;

let%expect_test "adt v2" =
  test_program {|type shape = Circle | Square;;|};
  [%expect {| [(Str_adt ([], "shape", (("Circle", None), [("Square", None)])))] |}]
;;

let%expect_test "adt v3" =
  test_program {|type shape = Circle | Square of int * int;;|};
  [%expect
    {|
    [(Str_adt ([], "shape",
        (("Circle", None),
         [("Square",
           (Some (Type_tuple
                    ((Type_construct ("int", [])), (Type_construct ("int", [])),
                     []))))
           ])
        ))
      ] |}]
;;

let%expect_test "adt with poly" =
  test_program {|type 'a shape = Circle | Square of 'a * 'a ;;|};
  [%expect
    {|
    [(Str_adt (["a"], "shape",
        (("Circle", None),
         [("Square", (Some (Type_tuple ((Type_var "a"), (Type_var "a"), []))))])
        ))
      ] |}]
;;

let%expect_test "bad adt with poly (wrong types)" =
  test_program {|type 'a shape = Circle | Square of 'b;;|};
  [%expect
    {|
    [(Str_adt (["a"], "shape",
        (("Circle", None), [("Square", (Some (Type_var "b")))])))
      ] |}]
;;

let%expect_test "adt with poly (not poly in variant)" =
  test_program {|type 'a shape = Circle | Square of int;;|};
  [%expect
    {|
    [(Str_adt (["a"], "shape",
        (("Circle", None), [("Square", (Some (Type_construct ("int", []))))])))
      ] |}]
;;

let%expect_test "adt with poly v.easy" =
  test_program {|type 'a shape = Circle;;|};
  [%expect {| [(Str_adt (["a"], "shape", (("Circle", None), [])))] |}]
;;

let%expect_test "adt with multiple poly v1" =
  test_program {|type ('a, 'b) shape = Circle | Square of 'a;;|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "shape",
        (("Circle", None), [("Square", (Some (Type_var "a")))])))
      ] |}]
;;

let%expect_test "adt with multiple poly v2" =
  test_program {|type ('a, 'b) shape = Circle | Square of ('a,'b) shape;;|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "shape",
        (("Circle", None),
         [("Square",
           (Some (Type_construct ("shape", [(Type_var "a"); (Type_var "b")]))))])
        ))
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

let%expect_test "adt with tuple in variant" =
  test_program {|type shape = Circle | Square of int * int ;;|};
  [%expect
    {|
    [(Str_adt ([], "shape",
        (("Circle", None),
         [("Square",
           (Some (Type_tuple
                    ((Type_construct ("int", [])), (Type_construct ("int", [])),
                     []))))
           ])
        ))
      ] |}]
;;

let%expect_test "adt with recursive poly variant" =
  test_program {|type ('a, 'b) shape = Circle | Square of 'a shape;;|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "shape",
        (("Circle", None),
         [("Square", (Some (Type_construct ("shape", [(Type_var "a")]))))])
        ))
      ] |}]
;;

let%expect_test "adt list" =
  test_program {|
type 'a my_list = Nil | Cons of 'a * 'a my_list;;
|};
  [%expect
    {|
    [(Str_adt (["a"], "my_list",
        (("Nil", None),
         [("Cons",
           (Some (Type_tuple
                    ((Type_var "a"),
                     (Type_construct ("my_list", [(Type_var "a")])), []))))
           ])
        ))
      ] |}]
;;

let%expect_test "adt nested type_construct in args" =
  test_program
    {|
type 'a nested_list = Nil 
| Cons of 'a * 'a nested_list 
| List of 'a nested_list;;

|};
  [%expect
    {|
    [(Str_adt (["a"], "nested_list",
        (("Nil", None),
         [("Cons",
           (Some (Type_tuple
                    ((Type_var "a"),
                     (Type_construct ("nested_list", [(Type_var "a")])),
                     []))));
           ("List", (Some (Type_construct ("nested_list", [(Type_var "a")]))))])
        ))
      ] |}]
;;

let%expect_test "adt nested type_construct in args" =
  test_program
    {|
type 'a nested_list = Nil 
| Cons of 'a * 'a nested_list 
| List of 'a nested_list nested_list;;
|};
  [%expect
    {|
    [(Str_adt (["a"], "nested_list",
        (("Nil", None),
         [("Cons",
           (Some (Type_tuple
                    ((Type_var "a"),
                     (Type_construct ("nested_list", [(Type_var "a")])),
                     []))));
           ("List",
            (Some (Type_construct ("nested_list",
                     [(Type_var "a"); (Type_construct ("nested_list", []))]))))
           ])
        ))
      ] |}]
;;

let%expect_test "poly adt (tree)" =
  test_program {|
type 'a tree = Leaf
  | Node of 'a * 'a tree * 'a tree
;;
  |};
  [%expect
    {|
    [(Str_adt (["a"], "tree",
        (("Leaf", None),
         [("Node",
           (Some (Type_tuple
                    ((Type_var "a"), (Type_construct ("tree", [(Type_var "a")])),
                     [(Type_construct ("tree", [(Type_var "a")]))]))))
           ])
        ))
      ] |}]
;;

let%expect_test "adt list with pair" =
  test_program
    {| type ('a, 'b) pair_list = Nil 
    | Cons of ('a * 'b) * ('a, 'b) pair_list;;
|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "pair_list",
        (("Nil", None),
         [("Cons",
           (Some (Type_tuple
                    ((Type_tuple ((Type_var "a"), (Type_var "b"), [])),
                     (Type_construct ("pair_list",
                        [(Type_var "a"); (Type_var "b")])),
                     []))))
           ])
        ))
      ] |}]
;;

let%expect_test "adt list with 2 el in node" =
  test_program
    {| type ('a, 'b) pair_list = Nil 
    | Cons of 'a * 'b * ('a, 'b) pair_list;;
|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "pair_list",
        (("Nil", None),
         [("Cons",
           (Some (Type_tuple
                    ((Type_var "a"), (Type_var "b"),
                     [(Type_construct ("pair_list",
                         [(Type_var "a"); (Type_var "b")]))
                       ]))))
           ])
        ))
      ] |}]
;;

let%expect_test "adt" =
  test_program
    {|
type shape = Point of int 
  | Circle of int * int 
  | Rect of int * int * int 
;;
|};
  [%expect
    {|
    [(Str_adt ([], "shape",
        (("Point", (Some (Type_construct ("int", [])))),
         [("Circle",
           (Some (Type_tuple
                    ((Type_construct ("int", [])), (Type_construct ("int", [])),
                     []))));
           ("Rect",
            (Some (Type_tuple
                     ((Type_construct ("int", [])), (Type_construct ("int", [])),
                      [(Type_construct ("int", []))]))))
           ])
        ))
      ] |}]
;;

let%expect_test "simple adt with pattern matching function (else case) + printing" =
  test_program
    {|
type shape = Circle of int
  | Rectangle of (int*int) * int  
  | Square of int
;;
let area s = 
    match s with
    | Square c -> 0
    | Circle c -> 3 
    | Rectangle c -> 10
;;
let x = Square 5 in
let y = area x in
print_int y
;;

  |};
  [%expect
    {|
    [(Str_adt ([], "shape",
        (("Circle", (Some (Type_construct ("int", [])))),
         [("Rectangle",
           (Some (Type_tuple
                    ((Type_tuple
                        ((Type_construct ("int", [])),
                         (Type_construct ("int", [])), [])),
                     (Type_construct ("int", [])), []))));
           ("Square", (Some (Type_construct ("int", []))))])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "area");
            expr =
            (Exp_fun (((Pat_var "s"), []),
               (Exp_match ((Exp_ident "s"),
                  ({ first = (Pat_construct ("Square", (Some (Pat_var "c"))));
                     second = (Exp_constant (Const_integer 0)) },
                   [{ first = (Pat_construct ("Circle", (Some (Pat_var "c"))));
                      second = (Exp_constant (Const_integer 3)) };
                     { first =
                       (Pat_construct ("Rectangle", (Some (Pat_var "c"))));
                       second = (Exp_constant (Const_integer 10)) }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_eval
         (Exp_let (Nonrecursive,
            ({ pat = (Pat_var "x");
               expr =
               (Exp_construct ("Square", (Some (Exp_constant (Const_integer 5)))
                  ))
               },
             []),
            (Exp_let (Nonrecursive,
               ({ pat = (Pat_var "y");
                  expr = (Exp_apply ((Exp_ident "area"), (Exp_ident "x"))) },
                []),
               (Exp_apply ((Exp_ident "print_int"), (Exp_ident "y")))))
            )))
      ] |}]
;;

let%expect_test "rec fun (pow)" =
  test_program
    {|
let rec pow x y = if y = 0 then 1 else x * pow x (y - 1) in print_int (pow 5 6)
;; |};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Recursive,
           ({ pat = (Pat_var "pow");
              expr =
              (Exp_fun (((Pat_var "x"), [(Pat_var "y")]),
                 (Exp_if (
                    (Exp_apply ((Exp_ident "="),
                       (Exp_tuple
                          ((Exp_ident "y"), (Exp_constant (Const_integer 0)), []))
                       )),
                    (Exp_constant (Const_integer 1)),
                    (Some (Exp_apply ((Exp_ident "*"),
                             (Exp_tuple
                                ((Exp_ident "x"),
                                 (Exp_apply (
                                    (Exp_apply ((Exp_ident "pow"),
                                       (Exp_ident "x"))),
                                    (Exp_apply ((Exp_ident "-"),
                                       (Exp_tuple
                                          ((Exp_ident "y"),
                                           (Exp_constant (Const_integer 1)),
                                           []))
                                       ))
                                    )),
                                 []))
                             )))
                    ))
                 ))
              },
            []),
           (Exp_apply ((Exp_ident "print_int"),
              (Exp_apply (
                 (Exp_apply ((Exp_ident "pow"), (Exp_constant (Const_integer 5))
                    )),
                 (Exp_constant (Const_integer 6))))
              ))
           )))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|let x = 5 and (z,v,c) = (5,6,7);;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) },
         [{ pat = (Pat_tuple ((Pat_var "z"), (Pat_var "v"), [(Pat_var "c")]));
            expr =
            (Exp_tuple
               ((Exp_constant (Const_integer 5)),
                (Exp_constant (Const_integer 6)),
                [(Exp_constant (Const_integer 7))]))
            }
           ])
        ))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|fun x -> x+x;;|};
  [%expect
    {|
    [(Str_eval
        (Exp_fun (((Pat_var "x"), []),
           (Exp_apply ((Exp_ident "+"),
              (Exp_tuple ((Exp_ident "x"), (Exp_ident "x"), []))))
           )))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|let main = 
   let () = print_int (fib 4) in
  0;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "main");
           expr =
           (Exp_let (Nonrecursive,
              ({ pat = (Pat_construct ("()", None));
                 expr =
                 (Exp_apply ((Exp_ident "print_int"),
                    (Exp_apply ((Exp_ident "fib"),
                       (Exp_constant (Const_integer 4))))
                    ))
                 },
               []),
              (Exp_constant (Const_integer 0))))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|let (x:char) = 20;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_constraint ((Pat_var "x"), (Type_construct ("char", []))));
           expr = (Exp_constant (Const_integer 20)) },
         [])
        ))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|let (x:(char*char)) = 20;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_constraint ((Pat_var "x"),
              (Type_tuple
                 ((Type_construct ("char", [])), (Type_construct ("char", [])),
                  []))
              ));
           expr = (Exp_constant (Const_integer 20)) },
         [])
        ))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|let (x: int option) = 20;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_constraint ((Pat_var "x"),
              (Type_construct ("option", [(Type_construct ("int", []))]))));
           expr = (Exp_constant (Const_integer 20)) },
         [])
        ))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {||};
  [%expect {| [] |}]
;;

let%expect_test "keyword" =
  test_program {|let () =  print_int 5;;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_construct ("()", None));
           expr =
           (Exp_apply ((Exp_ident "print_int"), (Exp_constant (Const_integer 5))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|let addi = fun f g x -> (f x (g x: bool) : int);;|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "addi");
           expr =
           (Exp_fun (((Pat_var "f"), [(Pat_var "g"); (Pat_var "x")]),
              (Exp_constraint (
                 (Exp_apply ((Exp_apply ((Exp_ident "f"), (Exp_ident "x"))),
                    (Exp_constraint (
                       (Exp_apply ((Exp_ident "g"), (Exp_ident "x"))),
                       (Type_construct ("bool", []))))
                    )),
                 (Type_construct ("int", []))))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "simple adt with pattern matching function + printing v3" =
  test_program
    {|
type 'a shape = Circle of int
  | Rectangle of int * int
  | Square of int 
;;
let area s = 
    match s with
    | Circle c -> 3 
    | Square c -> 0
    | Rectangle (c1, c2) -> c1 * c2
;;
let x = Rectangle (5, 10) in
let y = area x in
print_int y
;;
  |};
  [%expect
    {|
    [(Str_adt (["a"], "shape",
        (("Circle", (Some (Type_construct ("int", [])))),
         [("Rectangle",
           (Some (Type_tuple
                    ((Type_construct ("int", [])), (Type_construct ("int", [])),
                     []))));
           ("Square", (Some (Type_construct ("int", []))))])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "area");
            expr =
            (Exp_fun (((Pat_var "s"), []),
               (Exp_match ((Exp_ident "s"),
                  ({ first = (Pat_construct ("Circle", (Some (Pat_var "c"))));
                     second = (Exp_constant (Const_integer 3)) },
                   [{ first = (Pat_construct ("Square", (Some (Pat_var "c"))));
                      second = (Exp_constant (Const_integer 0)) };
                     { first =
                       (Pat_construct ("Rectangle",
                          (Some (Pat_tuple ((Pat_var "c1"), (Pat_var "c2"), [])))
                          ));
                       second =
                       (Exp_apply ((Exp_ident "*"),
                          (Exp_tuple ((Exp_ident "c1"), (Exp_ident "c2"), []))))
                       }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_eval
         (Exp_let (Nonrecursive,
            ({ pat = (Pat_var "x");
               expr =
               (Exp_construct ("Rectangle",
                  (Some (Exp_tuple
                           ((Exp_constant (Const_integer 5)),
                            (Exp_constant (Const_integer 10)), [])))
                  ))
               },
             []),
            (Exp_let (Nonrecursive,
               ({ pat = (Pat_var "y");
                  expr = (Exp_apply ((Exp_ident "area"), (Exp_ident "x"))) },
                []),
               (Exp_apply ((Exp_ident "print_int"), (Exp_ident "y")))))
            )))
      ] |}]
;;

let%expect_test "simple adt with pattern matching function + printing v3" =
  test_program
    {|
type ('a,'b) shape = Circle of int
  | Rectangle of int * int
  | Square of 'a * 'b
;;
  |};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "shape",
        (("Circle", (Some (Type_construct ("int", [])))),
         [("Rectangle",
           (Some (Type_tuple
                    ((Type_construct ("int", [])), (Type_construct ("int", [])),
                     []))));
           ("Square", (Some (Type_tuple ((Type_var "a"), (Type_var "b"), []))))])
        ))
      ] |}]
;;

let%expect_test "function assignment with bool operators" =
  test_program {| let id = fun (x, y) -> x && y in print_bool (id true false) ;; |};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "id");
              expr =
              (Exp_fun (((Pat_tuple ((Pat_var "x"), (Pat_var "y"), [])), []),
                 (Exp_apply ((Exp_ident "&&"),
                    (Exp_tuple ((Exp_ident "x"), (Exp_ident "y"), []))))
                 ))
              },
            []),
           (Exp_apply ((Exp_ident "print_bool"),
              (Exp_apply (
                 (Exp_apply ((Exp_ident "id"), (Exp_construct ("true", None)))),
                 (Exp_construct ("false", None))))
              ))
           )))
      ] |}]
;;

let%expect_test "function" =
  test_program
    {|
     let f = function
        | Some x -> x
        | None -> 0
      in
      f None, f (Some 42)
  |};
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "f");
              expr =
              (Exp_function
                 ({ first = (Pat_construct ("Some", (Some (Pat_var "x"))));
                    second = (Exp_ident "x") },
                  [{ first = (Pat_construct ("None", None));
                     second = (Exp_constant (Const_integer 0)) }
                    ]))
              },
            []),
           (Exp_tuple
              ((Exp_apply ((Exp_ident "f"), (Exp_construct ("None", None)))),
               (Exp_apply ((Exp_ident "f"),
                  (Exp_construct ("Some",
                     (Some (Exp_constant (Const_integer 42)))))
                  )),
               []))
           )))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|
let _6 = fun arg -> match arg with Some x -> let y = x in y;;
  |};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "_6");
           expr =
           (Exp_fun (((Pat_var "arg"), []),
              (Exp_match ((Exp_ident "arg"),
                 ({ first = (Pat_construct ("Some", (Some (Pat_var "x"))));
                    second =
                    (Exp_let (Nonrecursive,
                       ({ pat = (Pat_var "y"); expr = (Exp_ident "x") }, []),
                       (Exp_ident "y")))
                    },
                  [])
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "lists v1" =
  test_program {|
let x = [];;
  |};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "x"); expr = (Exp_construct ("[]", None)) }, [])))
      ] |}]
;;

let%expect_test "keyword" =
  test_program
    {|let rec fix f x = f (fix f) x;;
let map f p = let (a,b) = p in (f a, f b);;
let fixpoly l =
  fix (fun self l -> map (fun li x -> li (self l) x) l) l;;
let feven p n =
  let (e, o) = p in
  if n = 0 then 1 else o (n - 1);;
let fodd p n =
  let (e, o) = p in
  if n = 0 then 0 else e (n - 1);;
  let tie = fixpoly (feven, fodd);; |};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "fix");
           expr =
           (Exp_fun (((Pat_var "f"), [(Pat_var "x")]),
              (Exp_apply (
                 (Exp_apply ((Exp_ident "f"),
                    (Exp_apply ((Exp_ident "fix"), (Exp_ident "f"))))),
                 (Exp_ident "x")))
              ))
           },
         [])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "map");
            expr =
            (Exp_fun (((Pat_var "f"), [(Pat_var "p")]),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_tuple ((Pat_var "a"), (Pat_var "b"), []));
                     expr = (Exp_ident "p") },
                   []),
                  (Exp_tuple
                     ((Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                      (Exp_apply ((Exp_ident "f"), (Exp_ident "b"))), []))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "fixpoly");
            expr =
            (Exp_fun (((Pat_var "l"), []),
               (Exp_apply (
                  (Exp_apply ((Exp_ident "fix"),
                     (Exp_fun (((Pat_var "self"), [(Pat_var "l")]),
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "map"),
                              (Exp_fun (((Pat_var "li"), [(Pat_var "x")]),
                                 (Exp_apply (
                                    (Exp_apply ((Exp_ident "li"),
                                       (Exp_apply ((Exp_ident "self"),
                                          (Exp_ident "l")))
                                       )),
                                    (Exp_ident "x")))
                                 ))
                              )),
                           (Exp_ident "l")))
                        ))
                     )),
                  (Exp_ident "l")))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "feven");
            expr =
            (Exp_fun (((Pat_var "p"), [(Pat_var "n")]),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_tuple ((Pat_var "e"), (Pat_var "o"), []));
                     expr = (Exp_ident "p") },
                   []),
                  (Exp_if (
                     (Exp_apply ((Exp_ident "="),
                        (Exp_tuple
                           ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                        )),
                     (Exp_constant (Const_integer 1)),
                     (Some (Exp_apply ((Exp_ident "o"),
                              (Exp_apply ((Exp_ident "-"),
                                 (Exp_tuple
                                    ((Exp_ident "n"),
                                     (Exp_constant (Const_integer 1)), []))
                                 ))
                              )))
                     ))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "fodd");
            expr =
            (Exp_fun (((Pat_var "p"), [(Pat_var "n")]),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_tuple ((Pat_var "e"), (Pat_var "o"), []));
                     expr = (Exp_ident "p") },
                   []),
                  (Exp_if (
                     (Exp_apply ((Exp_ident "="),
                        (Exp_tuple
                           ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                        )),
                     (Exp_constant (Const_integer 0)),
                     (Some (Exp_apply ((Exp_ident "e"),
                              (Exp_apply ((Exp_ident "-"),
                                 (Exp_tuple
                                    ((Exp_ident "n"),
                                     (Exp_constant (Const_integer 1)), []))
                                 ))
                              )))
                     ))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "tie");
            expr =
            (Exp_apply ((Exp_ident "fixpoly"),
               (Exp_tuple ((Exp_ident "feven"), (Exp_ident "fodd"), []))))
            },
          [])
         ))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|type 'a foo = Foo;;
type bar = Bar of foo;; |};
  [%expect
    {|
    [(Str_adt (["a"], "foo", (("Foo", None), [])));
      (Str_adt ([], "bar", (("Bar", (Some (Type_construct ("foo", [])))), [])))] |}]
;;

let%expect_test "keyword" =
  test_program {|let (x: (int*char) option) = Some 5;; |};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_constraint ((Pat_var "x"),
              (Type_construct ("option",
                 [(Type_tuple
                     ((Type_construct ("int", [])),
                      (Type_construct ("char", [])), []))
                   ]
                 ))
              ));
           expr =
           (Exp_construct ("Some", (Some (Exp_constant (Const_integer 5))))) },
         [])
        ))
      ] |}]
;;

(*lists*)
let%expect_test "list1" =
  test_program
    {|let rec length xs =
   match xs with
   | [] -> 0
   | h::tl -> 1 + length tl;; |};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "length");
           expr =
           (Exp_fun (((Pat_var "xs"), []),
              (Exp_match ((Exp_ident "xs"),
                 ({ first = (Pat_construct ("[]", None));
                    second = (Exp_constant (Const_integer 0)) },
                  [{ first =
                     (Pat_construct ("::",
                        (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                     second =
                     (Exp_apply ((Exp_ident "+"),
                        (Exp_tuple
                           ((Exp_constant (Const_integer 1)),
                            (Exp_apply ((Exp_ident "length"), (Exp_ident "tl"))),
                            []))
                        ))
                     }
                    ])
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "list2" =
  test_program
    {|let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0
;; |};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "length_tail");
           expr =
           (Exp_let (Recursive,
              ({ pat = (Pat_var "helper");
                 expr =
                 (Exp_fun (((Pat_var "acc"), [(Pat_var "xs")]),
                    (Exp_match ((Exp_ident "xs"),
                       ({ first = (Pat_construct ("[]", None));
                          second = (Exp_ident "acc") },
                        [{ first =
                           (Pat_construct ("::",
                              (Some (Pat_tuple
                                       ((Pat_var "h"), (Pat_var "tl"), [])))
                              ));
                           second =
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "helper"),
                                 (Exp_apply ((Exp_ident "+"),
                                    (Exp_tuple
                                       ((Exp_ident "acc"),
                                        (Exp_constant (Const_integer 1)),
                                        []))
                                    ))
                                 )),
                              (Exp_ident "tl")))
                           }
                          ])
                       ))
                    ))
                 },
               []),
              (Exp_apply ((Exp_ident "helper"), (Exp_constant (Const_integer 0))
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "list3" =
  test_program
    {|let rec map f xs =
  match xs with
  | [] -> []
  | a::[] -> [f a]
  | a::b::[] -> [f a; f b]
  | a::b::c::[] -> [f a; f b; f c]
  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl
|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "map");
           expr =
           (Exp_fun (((Pat_var "f"), [(Pat_var "xs")]),
              (Exp_match ((Exp_ident "xs"),
                 ({ first = (Pat_construct ("[]", None));
                    second = (Exp_construct ("[]", None)) },
                  [{ first =
                     (Pat_construct ("::",
                        (Some (Pat_tuple
                                 ((Pat_var "a"), (Pat_construct ("[]", None)), [])))
                        ));
                     second =
                     (Exp_construct ("::",
                        (Some (Exp_tuple
                                 ((Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                  (Exp_construct ("[]", None)), [])))
                        ))
                     };
                    { first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple
                                  ((Pat_var "a"),
                                   (Pat_construct ("::",
                                      (Some (Pat_tuple
                                               ((Pat_var "b"),
                                                (Pat_construct ("[]", None)),
                                                [])))
                                      )),
                                   [])))
                         ));
                      second =
                      (Exp_construct ("::",
                         (Some (Exp_tuple
                                  ((Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                   (Exp_construct ("::",
                                      (Some (Exp_tuple
                                               ((Exp_apply ((Exp_ident "f"),
                                                   (Exp_ident "b"))),
                                                (Exp_construct ("[]", None)),
                                                [])))
                                      )),
                                   [])))
                         ))
                      };
                    { first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple
                                  ((Pat_var "a"),
                                   (Pat_construct ("::",
                                      (Some (Pat_tuple
                                               ((Pat_var "b"),
                                                (Pat_construct ("::",
                                                   (Some (Pat_tuple
                                                            ((Pat_var "c"),
                                                             (Pat_construct (
                                                                "[]", None)),
                                                             [])))
                                                   )),
                                                [])))
                                      )),
                                   [])))
                         ));
                      second =
                      (Exp_construct ("::",
                         (Some (Exp_tuple
                                  ((Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                   (Exp_construct ("::",
                                      (Some (Exp_tuple
                                               ((Exp_apply ((Exp_ident "f"),
                                                   (Exp_ident "b"))),
                                                (Exp_construct ("::",
                                                   (Some (Exp_tuple
                                                            ((Exp_apply (
                                                                (Exp_ident "f"),
                                                                (Exp_ident "c"))),
                                                             (Exp_construct (
                                                                "[]", None)),
                                                             [])))
                                                   )),
                                                [])))
                                      )),
                                   [])))
                         ))
                      };
                    { first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple
                                  ((Pat_var "a"),
                                   (Pat_construct ("::",
                                      (Some (Pat_tuple
                                               ((Pat_var "b"),
                                                (Pat_construct ("::",
                                                   (Some (Pat_tuple
                                                            ((Pat_var "c"),
                                                             (Pat_construct (
                                                                "::",
                                                                (Some (Pat_tuple
                                                                        ((
                                                                        Pat_var
                                                                        "d"),
                                                                        (Pat_var
                                                                        "tl"),
                                                                        [])))
                                                                )),
                                                             [])))
                                                   )),
                                                [])))
                                      )),
                                   [])))
                         ));
                      second =
                      (Exp_construct ("::",
                         (Some (Exp_tuple
                                  ((Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                   (Exp_construct ("::",
                                      (Some (Exp_tuple
                                               ((Exp_apply ((Exp_ident "f"),
                                                   (Exp_ident "b"))),
                                                (Exp_construct ("::",
                                                   (Some (Exp_tuple
                                                            ((Exp_apply (
                                                                (Exp_ident "f"),
                                                                (Exp_ident "c"))),
                                                             (Exp_construct (
                                                                "::",
                                                                (Some (Exp_tuple
                                                                        ((
                                                                        Exp_apply (
                                                                        (Exp_ident
                                                                        "f"),
                                                                        (Exp_ident
                                                                        "d"))),
                                                                        (Exp_apply (
                                                                        (Exp_apply (
                                                                        (Exp_ident
                                                                        "map"),
                                                                        (Exp_ident
                                                                        "f"))),
                                                                        (Exp_ident
                                                                        "tl"))),
                                                                        [])))
                                                                )),
                                                             [])))
                                                   )),
                                                [])))
                                      )),
                                   [])))
                         ))
                      }
                    ])
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "list4" =
  test_program
    {|let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys);;
|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "append");
           expr =
           (Exp_fun (((Pat_var "xs"), [(Pat_var "ys")]),
              (Exp_match ((Exp_ident "xs"),
                 ({ first = (Pat_construct ("[]", None));
                    second = (Exp_ident "ys") },
                  [{ first =
                     (Pat_construct ("::",
                        (Some (Pat_tuple ((Pat_var "x"), (Pat_var "xs"), [])))));
                     second =
                     (Exp_construct ("::",
                        (Some (Exp_tuple
                                 ((Exp_ident "x"),
                                  (Exp_apply (
                                     (Exp_apply ((Exp_ident "append"),
                                        (Exp_ident "xs"))),
                                     (Exp_ident "ys"))),
                                  [])))
                        ))
                     }
                    ])
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "list5" =
  test_program
    {|let concat =
  let rec helper xs =
    match xs with
    | [] -> []
    | h::tl -> append h (helper tl)
  in helper
;;
|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "concat");
           expr =
           (Exp_let (Recursive,
              ({ pat = (Pat_var "helper");
                 expr =
                 (Exp_fun (((Pat_var "xs"), []),
                    (Exp_match ((Exp_ident "xs"),
                       ({ first = (Pat_construct ("[]", None));
                          second = (Exp_construct ("[]", None)) },
                        [{ first =
                           (Pat_construct ("::",
                              (Some (Pat_tuple
                                       ((Pat_var "h"), (Pat_var "tl"), [])))
                              ));
                           second =
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "append"), (Exp_ident "h"))),
                              (Exp_apply ((Exp_ident "helper"), (Exp_ident "tl")
                                 ))
                              ))
                           }
                          ])
                       ))
                    ))
                 },
               []),
              (Exp_ident "helper")))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "list6" =
  test_program {|(1 :: 2) :: []
;;
|};
  [%expect
    {|
    [(Str_eval
        (Exp_construct ("::",
           (Some (Exp_tuple
                    ((Exp_construct ("::",
                        (Some (Exp_tuple
                                 ((Exp_constant (Const_integer 1)),
                                  (Exp_constant (Const_integer 2)), [])))
                        )),
                     (Exp_construct ("[]", None)), [])))
           )))
      ] |}]
;;

let%expect_test "list7" =
  test_program
    {|let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl;;
|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "iter");
           expr =
           (Exp_fun (((Pat_var "f"), [(Pat_var "xs")]),
              (Exp_match ((Exp_ident "xs"),
                 ({ first = (Pat_construct ("[]", None));
                    second = (Exp_construct ("()", None)) },
                  [{ first =
                     (Pat_construct ("::",
                        (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                     second =
                     (Exp_let (Nonrecursive,
                        ({ pat = (Pat_construct ("()", None));
                           expr = (Exp_apply ((Exp_ident "f"), (Exp_ident "h")))
                           },
                         []),
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "iter"), (Exp_ident "f"))),
                           (Exp_ident "tl")))
                        ))
                     }
                    ])
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "list8" =
  test_program
    {|let rec cartesian xs ys =
  match xs with
  | [] -> []
  | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys);;
|};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "cartesian");
           expr =
           (Exp_fun (((Pat_var "xs"), [(Pat_var "ys")]),
              (Exp_match ((Exp_ident "xs"),
                 ({ first = (Pat_construct ("[]", None));
                    second = (Exp_construct ("[]", None)) },
                  [{ first =
                     (Pat_construct ("::",
                        (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                     second =
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "append"),
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "map"),
                                 (Exp_fun (((Pat_var "a"), []),
                                    (Exp_tuple
                                       ((Exp_ident "h"), (Exp_ident "a"), []))
                                    ))
                                 )),
                              (Exp_ident "ys")))
                           )),
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "cartesian"), (Exp_ident "tl")
                              )),
                           (Exp_ident "ys")))
                        ))
                     }
                    ])
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "list9" =
  test_program
    {|let main =
  let () = iter print_int [1;2;3] in
  let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
  0
;;
|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "main");
           expr =
           (Exp_let (Nonrecursive,
              ({ pat = (Pat_construct ("()", None));
                 expr =
                 (Exp_apply (
                    (Exp_apply ((Exp_ident "iter"), (Exp_ident "print_int"))),
                    (Exp_construct ("::",
                       (Some (Exp_tuple
                                ((Exp_constant (Const_integer 1)),
                                 (Exp_construct ("::",
                                    (Some (Exp_tuple
                                             ((Exp_constant (Const_integer 2)),
                                              (Exp_construct ("::",
                                                 (Some (Exp_tuple
                                                          ((Exp_constant
                                                              (Const_integer 3)),
                                                           (Exp_construct ("[]",
                                                              None)),
                                                           [])))
                                                 )),
                                              [])))
                                    )),
                                 [])))
                       ))
                    ))
                 },
               []),
              (Exp_let (Nonrecursive,
                 ({ pat = (Pat_construct ("()", None));
                    expr =
                    (Exp_apply ((Exp_ident "print_int"),
                       (Exp_apply ((Exp_ident "length"),
                          (Exp_apply (
                             (Exp_apply ((Exp_ident "cartesian"),
                                (Exp_construct ("::",
                                   (Some (Exp_tuple
                                            ((Exp_constant (Const_integer 1)),
                                             (Exp_construct ("::",
                                                (Some (Exp_tuple
                                                         ((Exp_constant
                                                             (Const_integer 2)),
                                                          (Exp_construct ("[]",
                                                             None)),
                                                          [])))
                                                )),
                                             [])))
                                   ))
                                )),
                             (Exp_construct ("::",
                                (Some (Exp_tuple
                                         ((Exp_constant (Const_integer 1)),
                                          (Exp_construct ("::",
                                             (Some (Exp_tuple
                                                      ((Exp_constant
                                                          (Const_integer 2)),
                                                       (Exp_construct ("::",
                                                          (Some (Exp_tuple
                                                                   ((Exp_constant
                                                                       (Const_integer
                                                                        3)),
                                                                    (Exp_construct (
                                                                       "::",
                                                                       (Some (
                                                                       Exp_tuple
                                                                        ((
                                                                        Exp_constant
                                                                        (Const_integer
                                                                        4)),
                                                                        (Exp_construct (
                                                                        "[]",
                                                                        None)),
                                                                        [])))
                                                                       )),
                                                                    [])))
                                                          )),
                                                       [])))
                                             )),
                                          [])))
                                ))
                             ))
                          ))
                       ))
                    },
                  []),
                 (Exp_constant (Const_integer 0))))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "list9" =
  test_program {|type 'a list = 
    Cons of 'a * 'a list 
    | Nil;;|};
  [%expect
    {|
    [(Str_adt (["a"], "list",
        (("Cons",
          (Some (Type_tuple
                   ((Type_var "a"), (Type_construct ("list", [(Type_var "a")])),
                    [])))),
         [("Nil", None)])
        ))
      ] |}]
;;

let%expect_test "list9" =
  test_program
    {| 
let _1 = fun x y (a, _) -> (x + y - a) = 1

let _2 =
    let x, Some f = 1, Some ( "p1onerka was here" )
    in x

let _3 =  Some (1, "hi")

let _4 = let rec f x = f 5 in f

let _5 =
    let id x = x in
    match Some id with
      | Some f -> let _ = f "42" in f 42
      | None -> 0

let _6 = fun arg -> match arg with Some x -> let y = x in y;;

let int_of_option = function 
Some x -> x 
| None -> 0

let _42 = function 42 -> true | _ -> false

let id1, id2 = let id x = x in (id, id)
     
      
     |};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "_1");
           expr =
           (Exp_fun (
              ((Pat_var "x"),
               [(Pat_var "y"); (Pat_tuple ((Pat_var "a"), Pat_any, []))]),
              (Exp_apply ((Exp_ident "="),
                 (Exp_tuple
                    ((Exp_apply ((Exp_ident "-"),
                        (Exp_tuple
                           ((Exp_apply ((Exp_ident "+"),
                               (Exp_tuple ((Exp_ident "x"), (Exp_ident "y"), []))
                               )),
                            (Exp_ident "a"), []))
                        )),
                     (Exp_constant (Const_integer 1)), []))
                 ))
              ))
           },
         [])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "_2");
            expr =
            (Exp_let (Nonrecursive,
               ({ pat =
                  (Pat_tuple
                     ((Pat_var "x"),
                      (Pat_construct ("Some", (Some (Pat_var "f")))), []));
                  expr =
                  (Exp_tuple
                     ((Exp_constant (Const_integer 1)),
                      (Exp_construct ("Some",
                         (Some (Exp_constant (Const_string "p1onerka was here")))
                         )),
                      []))
                  },
                []),
               (Exp_ident "x")))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "_3");
            expr =
            (Exp_construct ("Some",
               (Some (Exp_tuple
                        ((Exp_constant (Const_integer 1)),
                         (Exp_constant (Const_string "hi")), [])))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "_4");
            expr =
            (Exp_let (Recursive,
               ({ pat = (Pat_var "f");
                  expr =
                  (Exp_fun (((Pat_var "x"), []),
                     (Exp_apply ((Exp_ident "f"),
                        (Exp_constant (Const_integer 5))))
                     ))
                  },
                []),
               (Exp_ident "f")))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "_5");
            expr =
            (Exp_let (Nonrecursive,
               ({ pat = (Pat_var "id");
                  expr = (Exp_fun (((Pat_var "x"), []), (Exp_ident "x"))) },
                []),
               (Exp_match ((Exp_construct ("Some", (Some (Exp_ident "id")))),
                  ({ first = (Pat_construct ("Some", (Some (Pat_var "f"))));
                     second =
                     (Exp_let (Nonrecursive,
                        ({ pat = Pat_any;
                           expr =
                           (Exp_apply ((Exp_ident "f"),
                              (Exp_constant (Const_string "42"))))
                           },
                         []),
                        (Exp_apply ((Exp_ident "f"),
                           (Exp_constant (Const_integer 42))))
                        ))
                     },
                   [{ first = (Pat_construct ("None", None));
                      second = (Exp_constant (Const_integer 0)) }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "_6");
            expr =
            (Exp_fun (((Pat_var "arg"), []),
               (Exp_match ((Exp_ident "arg"),
                  ({ first = (Pat_construct ("Some", (Some (Pat_var "x"))));
                     second =
                     (Exp_let (Nonrecursive,
                        ({ pat = (Pat_var "y"); expr = (Exp_ident "x") }, []),
                        (Exp_ident "y")))
                     },
                   [])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "int_of_option");
            expr =
            (Exp_function
               ({ first = (Pat_construct ("Some", (Some (Pat_var "x"))));
                  second = (Exp_ident "x") },
                [{ first = (Pat_construct ("None", None));
                   second = (Exp_constant (Const_integer 0)) }
                  ]))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "_42");
            expr =
            (Exp_function
               ({ first = (Pat_constant (Const_integer 42));
                  second = (Exp_construct ("true", None)) },
                [{ first = Pat_any; second = (Exp_construct ("false", None)) }]))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_tuple ((Pat_var "id1"), (Pat_var "id2"), []));
            expr =
            (Exp_let (Nonrecursive,
               ({ pat = (Pat_var "id");
                  expr = (Exp_fun (((Pat_var "x"), []), (Exp_ident "x"))) },
                []),
               (Exp_tuple ((Exp_ident "id"), (Exp_ident "id"), []))))
            },
          [])
         ))
      ] |}]
;;

let%expect_test "list9" =
  test_program
    {| 
let rec fix f x = f (fix f) x
let map f p = let (a,b) = p in (f a, f b)
let fixpoly l =
  fix (fun self l -> map (fun li x -> li (self l) x) l) l
let feven p n =
  let (e, o) = p in
  if n = 0 then 1 else o (n - 1)
let fodd p n =
  let (e, o) = p in
  if n = 0 then 0 else e (n - 1)
let tie = fixpoly (feven, fodd)

let rec meven n = if n = 0 then 1 else modd (n - 1)
and modd n = if n = 0 then 1 else meven (n - 1)
let main =
  let () = print_int (modd 1) in
  let () = print_int (meven 2) in
  let (even,odd) = tie in
  let () = print_int (odd 3) in
  let () = print_int (even 4) in
  0


      
     |};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "fix");
           expr =
           (Exp_fun (((Pat_var "f"), [(Pat_var "x")]),
              (Exp_apply (
                 (Exp_apply ((Exp_ident "f"),
                    (Exp_apply ((Exp_ident "fix"), (Exp_ident "f"))))),
                 (Exp_ident "x")))
              ))
           },
         [])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "map");
            expr =
            (Exp_fun (((Pat_var "f"), [(Pat_var "p")]),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_tuple ((Pat_var "a"), (Pat_var "b"), []));
                     expr = (Exp_ident "p") },
                   []),
                  (Exp_tuple
                     ((Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                      (Exp_apply ((Exp_ident "f"), (Exp_ident "b"))), []))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "fixpoly");
            expr =
            (Exp_fun (((Pat_var "l"), []),
               (Exp_apply (
                  (Exp_apply ((Exp_ident "fix"),
                     (Exp_fun (((Pat_var "self"), [(Pat_var "l")]),
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "map"),
                              (Exp_fun (((Pat_var "li"), [(Pat_var "x")]),
                                 (Exp_apply (
                                    (Exp_apply ((Exp_ident "li"),
                                       (Exp_apply ((Exp_ident "self"),
                                          (Exp_ident "l")))
                                       )),
                                    (Exp_ident "x")))
                                 ))
                              )),
                           (Exp_ident "l")))
                        ))
                     )),
                  (Exp_ident "l")))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "feven");
            expr =
            (Exp_fun (((Pat_var "p"), [(Pat_var "n")]),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_tuple ((Pat_var "e"), (Pat_var "o"), []));
                     expr = (Exp_ident "p") },
                   []),
                  (Exp_if (
                     (Exp_apply ((Exp_ident "="),
                        (Exp_tuple
                           ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                        )),
                     (Exp_constant (Const_integer 1)),
                     (Some (Exp_apply ((Exp_ident "o"),
                              (Exp_apply ((Exp_ident "-"),
                                 (Exp_tuple
                                    ((Exp_ident "n"),
                                     (Exp_constant (Const_integer 1)), []))
                                 ))
                              )))
                     ))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "fodd");
            expr =
            (Exp_fun (((Pat_var "p"), [(Pat_var "n")]),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_tuple ((Pat_var "e"), (Pat_var "o"), []));
                     expr = (Exp_ident "p") },
                   []),
                  (Exp_if (
                     (Exp_apply ((Exp_ident "="),
                        (Exp_tuple
                           ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                        )),
                     (Exp_constant (Const_integer 0)),
                     (Some (Exp_apply ((Exp_ident "e"),
                              (Exp_apply ((Exp_ident "-"),
                                 (Exp_tuple
                                    ((Exp_ident "n"),
                                     (Exp_constant (Const_integer 1)), []))
                                 ))
                              )))
                     ))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "tie");
            expr =
            (Exp_apply ((Exp_ident "fixpoly"),
               (Exp_tuple ((Exp_ident "feven"), (Exp_ident "fodd"), []))))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "meven");
            expr =
            (Exp_fun (((Pat_var "n"), []),
               (Exp_if (
                  (Exp_apply ((Exp_ident "="),
                     (Exp_tuple
                        ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                     )),
                  (Exp_constant (Const_integer 1)),
                  (Some (Exp_apply ((Exp_ident "modd"),
                           (Exp_apply ((Exp_ident "-"),
                              (Exp_tuple
                                 ((Exp_ident "n"),
                                  (Exp_constant (Const_integer 1)), []))
                              ))
                           )))
                  ))
               ))
            },
          [{ pat = (Pat_var "modd");
             expr =
             (Exp_fun (((Pat_var "n"), []),
                (Exp_if (
                   (Exp_apply ((Exp_ident "="),
                      (Exp_tuple
                         ((Exp_ident "n"), (Exp_constant (Const_integer 0)), []))
                      )),
                   (Exp_constant (Const_integer 1)),
                   (Some (Exp_apply ((Exp_ident "meven"),
                            (Exp_apply ((Exp_ident "-"),
                               (Exp_tuple
                                  ((Exp_ident "n"),
                                   (Exp_constant (Const_integer 1)), []))
                               ))
                            )))
                   ))
                ))
             }
            ])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "main");
            expr =
            (Exp_let (Nonrecursive,
               ({ pat = (Pat_construct ("()", None));
                  expr =
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply ((Exp_ident "modd"),
                        (Exp_constant (Const_integer 1))))
                     ))
                  },
                []),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_construct ("()", None));
                     expr =
                     (Exp_apply ((Exp_ident "print_int"),
                        (Exp_apply ((Exp_ident "meven"),
                           (Exp_constant (Const_integer 2))))
                        ))
                     },
                   []),
                  (Exp_let (Nonrecursive,
                     ({ pat = (Pat_tuple ((Pat_var "even"), (Pat_var "odd"), []));
                        expr = (Exp_ident "tie") },
                      []),
                     (Exp_let (Nonrecursive,
                        ({ pat = (Pat_construct ("()", None));
                           expr =
                           (Exp_apply ((Exp_ident "print_int"),
                              (Exp_apply ((Exp_ident "odd"),
                                 (Exp_constant (Const_integer 3))))
                              ))
                           },
                         []),
                        (Exp_let (Nonrecursive,
                           ({ pat = (Pat_construct ("()", None));
                              expr =
                              (Exp_apply ((Exp_ident "print_int"),
                                 (Exp_apply ((Exp_ident "even"),
                                    (Exp_constant (Const_integer 4))))
                                 ))
                              },
                            []),
                           (Exp_constant (Const_integer 0))))
                        ))
                     ))
                  ))
               ))
            },
          [])
         ))
      ] |}]
;;

let%expect_test "list9" =
  test_program
    {| 
let rec length xs =
  match xs with
  | [] -> 0
  | h::tl -> 1 + length tl

let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

let rec map f xs =
  match xs with
  | [] -> []
  | a::[] -> [f a]
  | a::b::[] -> [f a; f b]
  | a::b::c::[] -> [f a; f b; f c]
  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl

let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

let concat =
  let rec helper xs =
    match xs with
    | [] -> []
    | h::tl -> append h (helper tl)
  in helper

let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

let rec cartesian xs ys =
  match xs with
  | [] -> []
  | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)

let main =
  let () = iter print_int [1;2;3] in
  let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
  0

      
     |};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "length");
           expr =
           (Exp_fun (((Pat_var "xs"), []),
              (Exp_match ((Exp_ident "xs"),
                 ({ first = (Pat_construct ("[]", None));
                    second = (Exp_constant (Const_integer 0)) },
                  [{ first =
                     (Pat_construct ("::",
                        (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                     second =
                     (Exp_apply ((Exp_ident "+"),
                        (Exp_tuple
                           ((Exp_constant (Const_integer 1)),
                            (Exp_apply ((Exp_ident "length"), (Exp_ident "tl"))),
                            []))
                        ))
                     }
                    ])
                 ))
              ))
           },
         [])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "length_tail");
            expr =
            (Exp_let (Recursive,
               ({ pat = (Pat_var "helper");
                  expr =
                  (Exp_fun (((Pat_var "acc"), [(Pat_var "xs")]),
                     (Exp_match ((Exp_ident "xs"),
                        ({ first = (Pat_construct ("[]", None));
                           second = (Exp_ident "acc") },
                         [{ first =
                            (Pat_construct ("::",
                               (Some (Pat_tuple
                                        ((Pat_var "h"), (Pat_var "tl"), [])))
                               ));
                            second =
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "helper"),
                                  (Exp_apply ((Exp_ident "+"),
                                     (Exp_tuple
                                        ((Exp_ident "acc"),
                                         (Exp_constant (Const_integer 1)),
                                         []))
                                     ))
                                  )),
                               (Exp_ident "tl")))
                            }
                           ])
                        ))
                     ))
                  },
                []),
               (Exp_apply ((Exp_ident "helper"), (Exp_constant (Const_integer 0))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "map");
            expr =
            (Exp_fun (((Pat_var "f"), [(Pat_var "xs")]),
               (Exp_match ((Exp_ident "xs"),
                  ({ first = (Pat_construct ("[]", None));
                     second = (Exp_construct ("[]", None)) },
                   [{ first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple
                                  ((Pat_var "a"), (Pat_construct ("[]", None)),
                                   [])))
                         ));
                      second =
                      (Exp_construct ("::",
                         (Some (Exp_tuple
                                  ((Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                   (Exp_construct ("[]", None)), [])))
                         ))
                      };
                     { first =
                       (Pat_construct ("::",
                          (Some (Pat_tuple
                                   ((Pat_var "a"),
                                    (Pat_construct ("::",
                                       (Some (Pat_tuple
                                                ((Pat_var "b"),
                                                 (Pat_construct ("[]", None)),
                                                 [])))
                                       )),
                                    [])))
                          ));
                       second =
                       (Exp_construct ("::",
                          (Some (Exp_tuple
                                   ((Exp_apply ((Exp_ident "f"), (Exp_ident "a")
                                       )),
                                    (Exp_construct ("::",
                                       (Some (Exp_tuple
                                                ((Exp_apply ((Exp_ident "f"),
                                                    (Exp_ident "b"))),
                                                 (Exp_construct ("[]", None)),
                                                 [])))
                                       )),
                                    [])))
                          ))
                       };
                     { first =
                       (Pat_construct ("::",
                          (Some (Pat_tuple
                                   ((Pat_var "a"),
                                    (Pat_construct ("::",
                                       (Some (Pat_tuple
                                                ((Pat_var "b"),
                                                 (Pat_construct ("::",
                                                    (Some (Pat_tuple
                                                             ((Pat_var "c"),
                                                              (Pat_construct (
                                                                 "[]", None)),
                                                              [])))
                                                    )),
                                                 [])))
                                       )),
                                    [])))
                          ));
                       second =
                       (Exp_construct ("::",
                          (Some (Exp_tuple
                                   ((Exp_apply ((Exp_ident "f"), (Exp_ident "a")
                                       )),
                                    (Exp_construct ("::",
                                       (Some (Exp_tuple
                                                ((Exp_apply ((Exp_ident "f"),
                                                    (Exp_ident "b"))),
                                                 (Exp_construct ("::",
                                                    (Some (Exp_tuple
                                                             ((Exp_apply (
                                                                 (Exp_ident "f"),
                                                                 (Exp_ident "c")
                                                                 )),
                                                              (Exp_construct (
                                                                 "[]", None)),
                                                              [])))
                                                    )),
                                                 [])))
                                       )),
                                    [])))
                          ))
                       };
                     { first =
                       (Pat_construct ("::",
                          (Some (Pat_tuple
                                   ((Pat_var "a"),
                                    (Pat_construct ("::",
                                       (Some (Pat_tuple
                                                ((Pat_var "b"),
                                                 (Pat_construct ("::",
                                                    (Some (Pat_tuple
                                                             ((Pat_var "c"),
                                                              (Pat_construct (
                                                                 "::",
                                                                 (Some (Pat_tuple
                                                                        ((
                                                                        Pat_var
                                                                        "d"),
                                                                        (Pat_var
                                                                        "tl"),
                                                                        [])))
                                                                 )),
                                                              [])))
                                                    )),
                                                 [])))
                                       )),
                                    [])))
                          ));
                       second =
                       (Exp_construct ("::",
                          (Some (Exp_tuple
                                   ((Exp_apply ((Exp_ident "f"), (Exp_ident "a")
                                       )),
                                    (Exp_construct ("::",
                                       (Some (Exp_tuple
                                                ((Exp_apply ((Exp_ident "f"),
                                                    (Exp_ident "b"))),
                                                 (Exp_construct ("::",
                                                    (Some (Exp_tuple
                                                             ((Exp_apply (
                                                                 (Exp_ident "f"),
                                                                 (Exp_ident "c")
                                                                 )),
                                                              (Exp_construct (
                                                                 "::",
                                                                 (Some (Exp_tuple
                                                                        ((
                                                                        Exp_apply (
                                                                        (Exp_ident
                                                                        "f"),
                                                                        (Exp_ident
                                                                        "d"))),
                                                                        (Exp_apply (
                                                                        (Exp_apply (
                                                                        (Exp_ident
                                                                        "map"),
                                                                        (Exp_ident
                                                                        "f"))),
                                                                        (Exp_ident
                                                                        "tl"))),
                                                                        [])))
                                                                 )),
                                                              [])))
                                                    )),
                                                 [])))
                                       )),
                                    [])))
                          ))
                       }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "append");
            expr =
            (Exp_fun (((Pat_var "xs"), [(Pat_var "ys")]),
               (Exp_match ((Exp_ident "xs"),
                  ({ first = (Pat_construct ("[]", None));
                     second = (Exp_ident "ys") },
                   [{ first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple ((Pat_var "x"), (Pat_var "xs"), [])))));
                      second =
                      (Exp_construct ("::",
                         (Some (Exp_tuple
                                  ((Exp_ident "x"),
                                   (Exp_apply (
                                      (Exp_apply ((Exp_ident "append"),
                                         (Exp_ident "xs"))),
                                      (Exp_ident "ys"))),
                                   [])))
                         ))
                      }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "concat");
            expr =
            (Exp_let (Recursive,
               ({ pat = (Pat_var "helper");
                  expr =
                  (Exp_fun (((Pat_var "xs"), []),
                     (Exp_match ((Exp_ident "xs"),
                        ({ first = (Pat_construct ("[]", None));
                           second = (Exp_construct ("[]", None)) },
                         [{ first =
                            (Pat_construct ("::",
                               (Some (Pat_tuple
                                        ((Pat_var "h"), (Pat_var "tl"), [])))
                               ));
                            second =
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "append"), (Exp_ident "h")
                                  )),
                               (Exp_apply ((Exp_ident "helper"), (Exp_ident "tl")
                                  ))
                               ))
                            }
                           ])
                        ))
                     ))
                  },
                []),
               (Exp_ident "helper")))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "iter");
            expr =
            (Exp_fun (((Pat_var "f"), [(Pat_var "xs")]),
               (Exp_match ((Exp_ident "xs"),
                  ({ first = (Pat_construct ("[]", None));
                     second = (Exp_construct ("()", None)) },
                   [{ first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                      second =
                      (Exp_let (Nonrecursive,
                         ({ pat = (Pat_construct ("()", None));
                            expr = (Exp_apply ((Exp_ident "f"), (Exp_ident "h")))
                            },
                          []),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "iter"), (Exp_ident "f"))),
                            (Exp_ident "tl")))
                         ))
                      }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "cartesian");
            expr =
            (Exp_fun (((Pat_var "xs"), [(Pat_var "ys")]),
               (Exp_match ((Exp_ident "xs"),
                  ({ first = (Pat_construct ("[]", None));
                     second = (Exp_construct ("[]", None)) },
                   [{ first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                      second =
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "append"),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "map"),
                                  (Exp_fun (((Pat_var "a"), []),
                                     (Exp_tuple
                                        ((Exp_ident "h"), (Exp_ident "a"), []))
                                     ))
                                  )),
                               (Exp_ident "ys")))
                            )),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "cartesian"), (Exp_ident "tl")
                               )),
                            (Exp_ident "ys")))
                         ))
                      }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "main");
            expr =
            (Exp_let (Nonrecursive,
               ({ pat = (Pat_construct ("()", None));
                  expr =
                  (Exp_apply (
                     (Exp_apply ((Exp_ident "iter"), (Exp_ident "print_int"))),
                     (Exp_construct ("::",
                        (Some (Exp_tuple
                                 ((Exp_constant (Const_integer 1)),
                                  (Exp_construct ("::",
                                     (Some (Exp_tuple
                                              ((Exp_constant (Const_integer 2)),
                                               (Exp_construct ("::",
                                                  (Some (Exp_tuple
                                                           ((Exp_constant
                                                               (Const_integer 3)),
                                                            (Exp_construct ("[]",
                                                               None)),
                                                            [])))
                                                  )),
                                               [])))
                                     )),
                                  [])))
                        ))
                     ))
                  },
                []),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_construct ("()", None));
                     expr =
                     (Exp_apply ((Exp_ident "print_int"),
                        (Exp_apply ((Exp_ident "length"),
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "cartesian"),
                                 (Exp_construct ("::",
                                    (Some (Exp_tuple
                                             ((Exp_constant (Const_integer 1)),
                                              (Exp_construct ("::",
                                                 (Some (Exp_tuple
                                                          ((Exp_constant
                                                              (Const_integer 2)),
                                                           (Exp_construct ("[]",
                                                              None)),
                                                           [])))
                                                 )),
                                              [])))
                                    ))
                                 )),
                              (Exp_construct ("::",
                                 (Some (Exp_tuple
                                          ((Exp_constant (Const_integer 1)),
                                           (Exp_construct ("::",
                                              (Some (Exp_tuple
                                                       ((Exp_constant
                                                           (Const_integer 2)),
                                                        (Exp_construct ("::",
                                                           (Some (Exp_tuple
                                                                    ((Exp_constant
                                                                        (
                                                                        Const_integer
                                                                        3)),
                                                                     (Exp_construct (
                                                                        "::",
                                                                        (Some (
                                                                        Exp_tuple
                                                                        ((
                                                                        Exp_constant
                                                                        (Const_integer
                                                                        4)),
                                                                        (Exp_construct (
                                                                        "[]",
                                                                        None)),
                                                                        []))))),
                                                                     [])))
                                                           )),
                                                        [])))
                                              )),
                                           [])))
                                 ))
                              ))
                           ))
                        ))
                     },
                   []),
                  (Exp_constant (Const_integer 0))))
               ))
            },
          [])
         ))
      ] |}]
;;

let%expect_test "list6" =
  test_program "(1 :: 2) :: []";
  [%expect
    {|
      [(Str_eval
          (Exp_construct ("::",
             (Some (Exp_tuple
                      ((Exp_construct ("::",
                          (Some (Exp_tuple
                                   ((Exp_constant (Const_integer 1)),
                                    (Exp_constant (Const_integer 2)), [])))
                          )),
                       (Exp_construct ("[]", None)), [])))
             )))
        ] |}]
;;

let%expect_test "list5" =
  test_program
    "let concat = let rec helper xs = match xs with | [] -> [] | h::tl -> append h \
     (helper tl) in helper";
  [%expect
    {|
      [(Str_value (Nonrecursive,
          ({ pat = (Pat_var "concat");
             expr =
             (Exp_let (Recursive,
                ({ pat = (Pat_var "helper");
                   expr =
                   (Exp_fun (((Pat_var "xs"), []),
                      (Exp_match ((Exp_ident "xs"),
                         ({ first = (Pat_construct ("[]", None));
                            second = (Exp_construct ("[]", None)) },
                          [{ first =
                             (Pat_construct ("::",
                                (Some (Pat_tuple
                                         ((Pat_var "h"), (Pat_var "tl"), [])))
                                ));
                             second =
                             (Exp_apply (
                                (Exp_apply ((Exp_ident "append"), (Exp_ident "h"))),
                                (Exp_apply ((Exp_ident "helper"), (Exp_ident "tl")
                                   ))
                                ))
                             }
                            ])
                         ))
                      ))
                   },
                 []),
                (Exp_ident "helper")))
             },
           [])
          ))
        ] |}]
;;

let%expect_test "list_basic" =
  test_program "let lst = 1 :: 2 :: 3 :: [] in lst";
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "lst");
              expr =
              (Exp_construct ("::",
                 (Some (Exp_tuple
                          ((Exp_constant (Const_integer 1)),
                           (Exp_construct ("::",
                              (Some (Exp_tuple
                                       ((Exp_constant (Const_integer 2)),
                                        (Exp_construct ("::",
                                           (Some (Exp_tuple
                                                    ((Exp_constant
                                                        (Const_integer 3)),
                                                     (Exp_construct ("[]", None)),
                                                     [])))
                                           )),
                                        [])))
                              )),
                           [])))
                 ))
              },
            []),
           (Exp_ident "lst"))))
      ] |}]
;;

let%expect_test "list_match" =
  test_program "match 1 :: 2 :: 3 :: [] with | [] -> 0 | h :: _ -> h";
  [%expect
    {|
    [(Str_eval
        (Exp_match (
           (Exp_construct ("::",
              (Some (Exp_tuple
                       ((Exp_constant (Const_integer 1)),
                        (Exp_construct ("::",
                           (Some (Exp_tuple
                                    ((Exp_constant (Const_integer 2)),
                                     (Exp_construct ("::",
                                        (Some (Exp_tuple
                                                 ((Exp_constant (Const_integer 3)),
                                                  (Exp_construct ("[]", None)),
                                                  [])))
                                        )),
                                     [])))
                           )),
                        [])))
              )),
           ({ first = (Pat_construct ("[]", None));
              second = (Exp_constant (Const_integer 0)) },
            [{ first =
               (Pat_construct ("::",
                  (Some (Pat_tuple ((Pat_var "h"), Pat_any, [])))));
               second = (Exp_ident "h") }
              ])
           )))
      ] |}]
;;

let%expect_test "list_append" =
  test_program
    "let append xs ys = match xs with | [] -> ys | h :: t -> h :: append t ys in append \
     [1; 2] [3; 4]";
  [%expect
    {|
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "append");
              expr =
              (Exp_fun (((Pat_var "xs"), [(Pat_var "ys")]),
                 (Exp_match ((Exp_ident "xs"),
                    ({ first = (Pat_construct ("[]", None));
                       second = (Exp_ident "ys") },
                     [{ first =
                        (Pat_construct ("::",
                           (Some (Pat_tuple ((Pat_var "h"), (Pat_var "t"), [])))
                           ));
                        second =
                        (Exp_construct ("::",
                           (Some (Exp_tuple
                                    ((Exp_ident "h"),
                                     (Exp_apply (
                                        (Exp_apply ((Exp_ident "append"),
                                           (Exp_ident "t"))),
                                        (Exp_ident "ys"))),
                                     [])))
                           ))
                        }
                       ])
                    ))
                 ))
              },
            []),
           (Exp_apply (
              (Exp_apply ((Exp_ident "append"),
                 (Exp_construct ("::",
                    (Some (Exp_tuple
                             ((Exp_constant (Const_integer 1)),
                              (Exp_construct ("::",
                                 (Some (Exp_tuple
                                          ((Exp_constant (Const_integer 2)),
                                           (Exp_construct ("[]", None)),
                                           [])))
                                 )),
                              [])))
                    ))
                 )),
              (Exp_construct ("::",
                 (Some (Exp_tuple
                          ((Exp_constant (Const_integer 3)),
                           (Exp_construct ("::",
                              (Some (Exp_tuple
                                       ((Exp_constant (Const_integer 4)),
                                        (Exp_construct ("[]", None)), [])))
                              )),
                           [])))
                 ))
              ))
           )))
      ] |}]
;;

let%expect_test "()" =
  test_program
    {|
    let a =
      let b = 
        let rec f = (let x = 3 in x) + 1 
        in f
      in ();;
    let s = "string";;
    |};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "a");
           expr =
           (Exp_let (Nonrecursive,
              ({ pat = (Pat_var "b");
                 expr =
                 (Exp_let (Recursive,
                    ({ pat = (Pat_var "f");
                       expr =
                       (Exp_apply ((Exp_ident "+"),
                          (Exp_tuple
                             ((Exp_let (Nonrecursive,
                                 ({ pat = (Pat_var "x");
                                    expr = (Exp_constant (Const_integer 3)) },
                                  []),
                                 (Exp_ident "x"))),
                              (Exp_constant (Const_integer 1)), []))
                          ))
                       },
                     []),
                    (Exp_ident "f")))
                 },
               []),
              (Exp_construct ("()", None))))
           },
         [])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "s"); expr = (Exp_constant (Const_string "string")) },
          [])
         ))
      ] |}]
;;

let%expect_test "()" =
  test_program {|
    let rec iter f xs = match xs with [] -> ()
    |};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "iter");
           expr =
           (Exp_fun (((Pat_var "f"), [(Pat_var "xs")]),
              (Exp_match ((Exp_ident "xs"),
                 ({ first = (Pat_construct ("[]", None));
                    second = (Exp_construct ("()", None)) },
                  [])
                 ))
              ))
           },
         [])
        ))
      ] |}]
;;

let%expect_test "()" =
  test_program
    {|
let rec length xs =
  match xs with
  | [] -> 0
  | h::tl -> 1 + length tl

let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

let rec map f xs =
  match xs with
  | [] -> []
  | a::[] -> [f a]
  | a::b::[] -> [f a; f b]
  | a::b::c::[] -> [f a; f b; f c]
  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl

let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

let concat =
  let rec helper xs =
    match xs with
    | [] -> []
    | h::tl -> append h (helper tl)
  in helper

let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

let rec cartesian xs ys =
  match xs with
  | [] -> []
  | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)

let main =
  let () = iter print_int [1;2;3] in
  let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
  0
      |};
  [%expect
    {|
    [(Str_value (Recursive,
        ({ pat = (Pat_var "length");
           expr =
           (Exp_fun (((Pat_var "xs"), []),
              (Exp_match ((Exp_ident "xs"),
                 ({ first = (Pat_construct ("[]", None));
                    second = (Exp_constant (Const_integer 0)) },
                  [{ first =
                     (Pat_construct ("::",
                        (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                     second =
                     (Exp_apply ((Exp_ident "+"),
                        (Exp_tuple
                           ((Exp_constant (Const_integer 1)),
                            (Exp_apply ((Exp_ident "length"), (Exp_ident "tl"))),
                            []))
                        ))
                     }
                    ])
                 ))
              ))
           },
         [])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "length_tail");
            expr =
            (Exp_let (Recursive,
               ({ pat = (Pat_var "helper");
                  expr =
                  (Exp_fun (((Pat_var "acc"), [(Pat_var "xs")]),
                     (Exp_match ((Exp_ident "xs"),
                        ({ first = (Pat_construct ("[]", None));
                           second = (Exp_ident "acc") },
                         [{ first =
                            (Pat_construct ("::",
                               (Some (Pat_tuple
                                        ((Pat_var "h"), (Pat_var "tl"), [])))
                               ));
                            second =
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "helper"),
                                  (Exp_apply ((Exp_ident "+"),
                                     (Exp_tuple
                                        ((Exp_ident "acc"),
                                         (Exp_constant (Const_integer 1)),
                                         []))
                                     ))
                                  )),
                               (Exp_ident "tl")))
                            }
                           ])
                        ))
                     ))
                  },
                []),
               (Exp_apply ((Exp_ident "helper"), (Exp_constant (Const_integer 0))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "map");
            expr =
            (Exp_fun (((Pat_var "f"), [(Pat_var "xs")]),
               (Exp_match ((Exp_ident "xs"),
                  ({ first = (Pat_construct ("[]", None));
                     second = (Exp_construct ("[]", None)) },
                   [{ first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple
                                  ((Pat_var "a"), (Pat_construct ("[]", None)),
                                   [])))
                         ));
                      second =
                      (Exp_construct ("::",
                         (Some (Exp_tuple
                                  ((Exp_apply ((Exp_ident "f"), (Exp_ident "a"))),
                                   (Exp_construct ("[]", None)), [])))
                         ))
                      };
                     { first =
                       (Pat_construct ("::",
                          (Some (Pat_tuple
                                   ((Pat_var "a"),
                                    (Pat_construct ("::",
                                       (Some (Pat_tuple
                                                ((Pat_var "b"),
                                                 (Pat_construct ("[]", None)),
                                                 [])))
                                       )),
                                    [])))
                          ));
                       second =
                       (Exp_construct ("::",
                          (Some (Exp_tuple
                                   ((Exp_apply ((Exp_ident "f"), (Exp_ident "a")
                                       )),
                                    (Exp_construct ("::",
                                       (Some (Exp_tuple
                                                ((Exp_apply ((Exp_ident "f"),
                                                    (Exp_ident "b"))),
                                                 (Exp_construct ("[]", None)),
                                                 [])))
                                       )),
                                    [])))
                          ))
                       };
                     { first =
                       (Pat_construct ("::",
                          (Some (Pat_tuple
                                   ((Pat_var "a"),
                                    (Pat_construct ("::",
                                       (Some (Pat_tuple
                                                ((Pat_var "b"),
                                                 (Pat_construct ("::",
                                                    (Some (Pat_tuple
                                                             ((Pat_var "c"),
                                                              (Pat_construct (
                                                                 "[]", None)),
                                                              [])))
                                                    )),
                                                 [])))
                                       )),
                                    [])))
                          ));
                       second =
                       (Exp_construct ("::",
                          (Some (Exp_tuple
                                   ((Exp_apply ((Exp_ident "f"), (Exp_ident "a")
                                       )),
                                    (Exp_construct ("::",
                                       (Some (Exp_tuple
                                                ((Exp_apply ((Exp_ident "f"),
                                                    (Exp_ident "b"))),
                                                 (Exp_construct ("::",
                                                    (Some (Exp_tuple
                                                             ((Exp_apply (
                                                                 (Exp_ident "f"),
                                                                 (Exp_ident "c")
                                                                 )),
                                                              (Exp_construct (
                                                                 "[]", None)),
                                                              [])))
                                                    )),
                                                 [])))
                                       )),
                                    [])))
                          ))
                       };
                     { first =
                       (Pat_construct ("::",
                          (Some (Pat_tuple
                                   ((Pat_var "a"),
                                    (Pat_construct ("::",
                                       (Some (Pat_tuple
                                                ((Pat_var "b"),
                                                 (Pat_construct ("::",
                                                    (Some (Pat_tuple
                                                             ((Pat_var "c"),
                                                              (Pat_construct (
                                                                 "::",
                                                                 (Some (Pat_tuple
                                                                        ((
                                                                        Pat_var
                                                                        "d"),
                                                                        (Pat_var
                                                                        "tl"),
                                                                        [])))
                                                                 )),
                                                              [])))
                                                    )),
                                                 [])))
                                       )),
                                    [])))
                          ));
                       second =
                       (Exp_construct ("::",
                          (Some (Exp_tuple
                                   ((Exp_apply ((Exp_ident "f"), (Exp_ident "a")
                                       )),
                                    (Exp_construct ("::",
                                       (Some (Exp_tuple
                                                ((Exp_apply ((Exp_ident "f"),
                                                    (Exp_ident "b"))),
                                                 (Exp_construct ("::",
                                                    (Some (Exp_tuple
                                                             ((Exp_apply (
                                                                 (Exp_ident "f"),
                                                                 (Exp_ident "c")
                                                                 )),
                                                              (Exp_construct (
                                                                 "::",
                                                                 (Some (Exp_tuple
                                                                        ((
                                                                        Exp_apply (
                                                                        (Exp_ident
                                                                        "f"),
                                                                        (Exp_ident
                                                                        "d"))),
                                                                        (Exp_apply (
                                                                        (Exp_apply (
                                                                        (Exp_ident
                                                                        "map"),
                                                                        (Exp_ident
                                                                        "f"))),
                                                                        (Exp_ident
                                                                        "tl"))),
                                                                        [])))
                                                                 )),
                                                              [])))
                                                    )),
                                                 [])))
                                       )),
                                    [])))
                          ))
                       }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "append");
            expr =
            (Exp_fun (((Pat_var "xs"), [(Pat_var "ys")]),
               (Exp_match ((Exp_ident "xs"),
                  ({ first = (Pat_construct ("[]", None));
                     second = (Exp_ident "ys") },
                   [{ first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple ((Pat_var "x"), (Pat_var "xs"), [])))));
                      second =
                      (Exp_construct ("::",
                         (Some (Exp_tuple
                                  ((Exp_ident "x"),
                                   (Exp_apply (
                                      (Exp_apply ((Exp_ident "append"),
                                         (Exp_ident "xs"))),
                                      (Exp_ident "ys"))),
                                   [])))
                         ))
                      }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "concat");
            expr =
            (Exp_let (Recursive,
               ({ pat = (Pat_var "helper");
                  expr =
                  (Exp_fun (((Pat_var "xs"), []),
                     (Exp_match ((Exp_ident "xs"),
                        ({ first = (Pat_construct ("[]", None));
                           second = (Exp_construct ("[]", None)) },
                         [{ first =
                            (Pat_construct ("::",
                               (Some (Pat_tuple
                                        ((Pat_var "h"), (Pat_var "tl"), [])))
                               ));
                            second =
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "append"), (Exp_ident "h")
                                  )),
                               (Exp_apply ((Exp_ident "helper"), (Exp_ident "tl")
                                  ))
                               ))
                            }
                           ])
                        ))
                     ))
                  },
                []),
               (Exp_ident "helper")))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "iter");
            expr =
            (Exp_fun (((Pat_var "f"), [(Pat_var "xs")]),
               (Exp_match ((Exp_ident "xs"),
                  ({ first = (Pat_construct ("[]", None));
                     second = (Exp_construct ("()", None)) },
                   [{ first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                      second =
                      (Exp_let (Nonrecursive,
                         ({ pat = (Pat_construct ("()", None));
                            expr = (Exp_apply ((Exp_ident "f"), (Exp_ident "h")))
                            },
                          []),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "iter"), (Exp_ident "f"))),
                            (Exp_ident "tl")))
                         ))
                      }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Recursive,
         ({ pat = (Pat_var "cartesian");
            expr =
            (Exp_fun (((Pat_var "xs"), [(Pat_var "ys")]),
               (Exp_match ((Exp_ident "xs"),
                  ({ first = (Pat_construct ("[]", None));
                     second = (Exp_construct ("[]", None)) },
                   [{ first =
                      (Pat_construct ("::",
                         (Some (Pat_tuple ((Pat_var "h"), (Pat_var "tl"), [])))));
                      second =
                      (Exp_apply (
                         (Exp_apply ((Exp_ident "append"),
                            (Exp_apply (
                               (Exp_apply ((Exp_ident "map"),
                                  (Exp_fun (((Pat_var "a"), []),
                                     (Exp_tuple
                                        ((Exp_ident "h"), (Exp_ident "a"), []))
                                     ))
                                  )),
                               (Exp_ident "ys")))
                            )),
                         (Exp_apply (
                            (Exp_apply ((Exp_ident "cartesian"), (Exp_ident "tl")
                               )),
                            (Exp_ident "ys")))
                         ))
                      }
                     ])
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "main");
            expr =
            (Exp_let (Nonrecursive,
               ({ pat = (Pat_construct ("()", None));
                  expr =
                  (Exp_apply (
                     (Exp_apply ((Exp_ident "iter"), (Exp_ident "print_int"))),
                     (Exp_construct ("::",
                        (Some (Exp_tuple
                                 ((Exp_constant (Const_integer 1)),
                                  (Exp_construct ("::",
                                     (Some (Exp_tuple
                                              ((Exp_constant (Const_integer 2)),
                                               (Exp_construct ("::",
                                                  (Some (Exp_tuple
                                                           ((Exp_constant
                                                               (Const_integer 3)),
                                                            (Exp_construct ("[]",
                                                               None)),
                                                            [])))
                                                  )),
                                               [])))
                                     )),
                                  [])))
                        ))
                     ))
                  },
                []),
               (Exp_let (Nonrecursive,
                  ({ pat = (Pat_construct ("()", None));
                     expr =
                     (Exp_apply ((Exp_ident "print_int"),
                        (Exp_apply ((Exp_ident "length"),
                           (Exp_apply (
                              (Exp_apply ((Exp_ident "cartesian"),
                                 (Exp_construct ("::",
                                    (Some (Exp_tuple
                                             ((Exp_constant (Const_integer 1)),
                                              (Exp_construct ("::",
                                                 (Some (Exp_tuple
                                                          ((Exp_constant
                                                              (Const_integer 2)),
                                                           (Exp_construct ("[]",
                                                              None)),
                                                           [])))
                                                 )),
                                              [])))
                                    ))
                                 )),
                              (Exp_construct ("::",
                                 (Some (Exp_tuple
                                          ((Exp_constant (Const_integer 1)),
                                           (Exp_construct ("::",
                                              (Some (Exp_tuple
                                                       ((Exp_constant
                                                           (Const_integer 2)),
                                                        (Exp_construct ("::",
                                                           (Some (Exp_tuple
                                                                    ((Exp_constant
                                                                        (
                                                                        Const_integer
                                                                        3)),
                                                                     (Exp_construct (
                                                                        "::",
                                                                        (Some (
                                                                        Exp_tuple
                                                                        ((
                                                                        Exp_constant
                                                                        (Const_integer
                                                                        4)),
                                                                        (Exp_construct (
                                                                        "[]",
                                                                        None)),
                                                                        []))))),
                                                                     [])))
                                                           )),
                                                        [])))
                                              )),
                                           [])))
                                 ))
                              ))
                           ))
                        ))
                     },
                   []),
                  (Exp_constant (Const_integer 0))))
               ))
            },
          [])
         ))
      ]

    |}]
;;

let%expect_test "simple adt with pattern matching function (else case) + printing" =
  test_program {|
type shape = Circle of int
  | Rectangle of (int*int) * int
;;
  |};
  [%expect
    {|
    [(Str_adt ([], "shape",
        (("Circle", (Some (Type_construct ("int", [])))),
         [("Rectangle",
           (Some (Type_tuple
                    ((Type_tuple
                        ((Type_construct ("int", [])),
                         (Type_construct ("int", [])), [])),
                     (Type_construct ("int", [])), []))))
           ])
        ))
      ] |}]
;;

let%expect_test "one arg adt v2" =
  test_program {|
type ('a) shape = Circle of int
  | Rectangle of (int*int) * int
;;
  |};
  [%expect
    {|
    [(Str_adt (["a"], "shape",
        (("Circle", (Some (Type_construct ("int", [])))),
         [("Rectangle",
           (Some (Type_tuple
                    ((Type_tuple
                        ((Type_construct ("int", [])),
                         (Type_construct ("int", [])), [])),
                     (Type_construct ("int", [])), []))))
           ])
        ))
      ] |}]
;;

let%expect_test "multiple args adt v2" =
  test_program {|
   type ('a, 'b) s9CG0K = 
   | R
  | F
  | H of f
;;
|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "s9CG0K",
        (("R", None), [("F", None); ("H", (Some (Type_construct ("f", []))))])))
      ] |}]
;;

let%expect_test "multiple args adt v3" =
  test_program {|
   type ('a, 'b, 'c, 'd) s9CG0K = 
   | R
  | F
  | H of f
;;
|};
  [%expect
    {|
    [(Str_adt (["a"; "b"; "c"; "d"], "s9CG0K",
        (("R", None), [("F", None); ("H", (Some (Type_construct ("f", []))))])))
      ] |}]
;;

let%expect_test "multiple args adt v4" =
  test_program {|
   type '_3d f =
  | J of _f
  | K
;;
|};
  [%expect
    {|
    [(Str_adt (["_3d"], "f",
        (("J", (Some (Type_construct ("_f", [])))), [("K", None)])))
      ] |}]
;;

let%expect_test "multiple args adt v4 (capitalized idents in constr_args)" =
  test_program
    {|
   type ('ot, '_a, 't, '_v) i_ =
  | L_ of Z
  | Dl of _f
  | G of uG_
  | Egd of _a
;;
|};
  [%expect
    {|
    [(Str_adt (["ot"; "_a"; "t"; "_v"], "i_",
        (("L_", (Some (Type_construct ("Z", [])))),
         [("Dl", (Some (Type_construct ("_f", []))));
           ("G", (Some (Type_construct ("uG_", []))));
           ("Egd", (Some (Type_construct ("_a", []))))])
        ))
      ] |}]
;;

let%expect_test "adt from default types" =
  test_program {|
type point = int * int;;
|};
  [%expect
    {|
  [(Str_adt ([], "point",
      (("",
        (Some (Type_tuple
                 ((Type_construct ("int", [])), (Type_construct ("int", [])),
                  [])))),
       [])
      ))
    ]
  |}]
;;

let%expect_test "adt match case (pat_any)" =
  test_program
    {|
let area s = 
    match s with
    | Square c -> 0
    | Circle c -> 3 
    | _ -> 10
;;
|};
  [%expect
    {|
  [(Str_value (Nonrecursive,
      ({ pat = (Pat_var "area");
         expr =
         (Exp_fun (((Pat_var "s"), []),
            (Exp_match ((Exp_ident "s"),
               ({ first = (Pat_construct ("Square", (Some (Pat_var "c"))));
                  second = (Exp_constant (Const_integer 0)) },
                [{ first = (Pat_construct ("Circle", (Some (Pat_var "c"))));
                   second = (Exp_constant (Const_integer 3)) };
                  { first = Pat_any; second = (Exp_constant (Const_integer 10))
                    }
                  ])
               ))
            ))
         },
       [])
      ))
    ]
  |}]
;;

let%expect_test "006partial2" =
  test_program
    {|
let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)

let foo x = foo true (foo false (foo true (foo false x)))
let main =
  let () = print_int (foo 11) in
  0
|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "foo");
           expr =
           (Exp_fun (((Pat_var "b"), []),
              (Exp_if ((Exp_ident "b"),
                 (Exp_fun (((Pat_var "foo"), []),
                    (Exp_apply ((Exp_ident "+"),
                       (Exp_tuple
                          ((Exp_ident "foo"), (Exp_constant (Const_integer 2)),
                           []))
                       ))
                    )),
                 (Some (Exp_fun (((Pat_var "foo"), []),
                          (Exp_apply ((Exp_ident "*"),
                             (Exp_tuple
                                ((Exp_ident "foo"),
                                 (Exp_constant (Const_integer 10)), []))
                             ))
                          )))
                 ))
              ))
           },
         [])
        ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "foo");
            expr =
            (Exp_fun (((Pat_var "x"), []),
               (Exp_apply (
                  (Exp_apply ((Exp_ident "foo"), (Exp_construct ("true", None)))),
                  (Exp_apply (
                     (Exp_apply ((Exp_ident "foo"),
                        (Exp_construct ("false", None)))),
                     (Exp_apply (
                        (Exp_apply ((Exp_ident "foo"),
                           (Exp_construct ("true", None)))),
                        (Exp_apply (
                           (Exp_apply ((Exp_ident "foo"),
                              (Exp_construct ("false", None)))),
                           (Exp_ident "x")))
                        ))
                     ))
                  ))
               ))
            },
          [])
         ));
      (Str_value (Nonrecursive,
         ({ pat = (Pat_var "main");
            expr =
            (Exp_let (Nonrecursive,
               ({ pat = (Pat_construct ("()", None));
                  expr =
                  (Exp_apply ((Exp_ident "print_int"),
                     (Exp_apply ((Exp_ident "foo"),
                        (Exp_constant (Const_integer 11))))
                     ))
                  },
                []),
               (Exp_constant (Const_integer 0))))
            },
          [])
         ))
      ] |}]
;;
