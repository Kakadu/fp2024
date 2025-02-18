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
           (Exp_ident "x"))))
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
    [(Str_eval
        (Exp_let (Nonrecursive,
           ({ pat = (Pat_var "reca"); expr = (Exp_constant (Const_integer 1)) },
            []),
           (Exp_constant (Const_integer 1)))))
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
  test_program {|(f : (Int -> int -> int));;|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_tests__Parser.test_program in file "tests/parser.ml", line 9, characters 51-66
  Called from Ocamladt_tests__Parser.(fun) in file "tests/parser.ml", line 949, characters 2-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
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

(*good*)
let%expect_test "_" =
  test_program
    {|('v' : (SqEcf8boz* L58r6D_P_bX___yy_93GPH__04_r___d9Zc_1U2__c8XmN1n_F_WBqxl68h_8_TCGqp3B_5w_Y_53a6_d_6_H9845__c5__09s* Sh__7ud_43* E_KKm_z3r5__jHMLw_qd1760R_G__nI6_J040__AB_6s0__D__d__e32Te6H_4__Ec_V_E__f_* o0_a_W_* f__LcPREH13__mY_CezffoI5_8_u_zU__ZncOnf_v4_L8_44Y72_3_A5_B758TViP_u_vyFU9_1* QD0* g4wp33A_W* E1V_gi_6y* x_Sv_PZ)) ;; |};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_tests__Parser.test_program in file "tests/parser.ml", line 9, characters 51-66
  Called from Ocamladt_tests__Parser.(fun) in file "tests/parser.ml", line 1005, characters 2-356
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
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
  [%expect {| [(Str_adt ([], "shape", (("Circle", []), [])))] |}]
;;

let%expect_test "adt v1" =
  test_program {|type shape = Circle | Square of int;;|};
  [%expect
    {|
    [(Str_adt ([], "shape",
        (("Circle", []), [("Square", [(Type_construct ("int", []))])])))
      ] |}]
;;

let%expect_test "adt v2" =
  test_program {|type shape = Circle | Square;;|};
  [%expect {|
    [(Str_adt ([], "shape", (("Circle", []), [("Square", [])])))] |}]
;;

let%expect_test "adt v3" =
  test_program {|type shape = Circle | Square of int * int;;|};
  [%expect
    {|
    [(Str_adt ([], "shape",
        (("Circle", []),
         [("Square", [(Type_construct ("int", [])); (Type_construct ("int", []))])
           ])
        ))
      ] |}]
;;

let%expect_test "adt with poly" =
  test_program {|type 'a shape = Circle | Square of 'a * 'a ;;|};
  [%expect
    {|
    [(Str_adt (["a"], "shape",
        (("Circle", []), [("Square", [(Type_var "a"); (Type_var "a")])])))
      ] |}]
;;

let%expect_test "bad adt with poly (wrong types)" =
  test_program {|type 'a shape = Circle | Square of 'b;;|};
  [%expect
    {|
    [(Str_adt (["a"], "shape", (("Circle", []), [("Square", [(Type_var "b")])])))
      ] |}]
;;

let%expect_test "adt with poly (not poly in variant)" =
  test_program {|type 'a shape = Circle | Square of int;;|};
  [%expect
    {|
    [(Str_adt (["a"], "shape",
        (("Circle", []), [("Square", [(Type_construct ("int", []))])])))
      ] |}]
;;

let%expect_test "adt with poly v.easy" =
  test_program {|type 'a shape = Circle;;|};
  [%expect {| [(Str_adt (["a"], "shape", (("Circle", []), [])))] |}]
;;

let%expect_test "adt with multiple poly v1" =
  test_program {|type ('a, 'b) shape = Circle | Square of 'a;;|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "shape",
        (("Circle", []), [("Square", [(Type_var "a")])])))
      ] |}]
;;

let%expect_test "adt with multiple poly v2" =
  test_program {|type ('a, 'b) shape = Circle | Square of ('a,'b) shape;;|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "shape",
        (("Circle", []),
         [("Square",
           [(Type_construct ("shape", [(Type_var "a"); (Type_var "b")]))])])
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
        (("Circle", []),
         [("Square", [(Type_construct ("int", [])); (Type_construct ("int", []))])
           ])
        ))
      ] |}]
;;

let%expect_test "adt with recursive poly variant" =
  test_program {|type ('a, 'b) shape = Circle | Square of 'a shape;;|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "shape",
        (("Circle", []),
         [("Square", [(Type_construct ("shape", [(Type_var "a")]))])])
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
        (("Nil", []),
         [("Cons",
           [(Type_var "a"); (Type_construct ("my_list", [(Type_var "a")]))])])
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
        (("Nil", []),
         [("Cons",
           [(Type_var "a"); (Type_construct ("nested_list", [(Type_var "a")]))]);
           ("List", [(Type_construct ("nested_list", [(Type_var "a")]))])])
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
        (("Nil", []),
         [("Cons",
           [(Type_var "a"); (Type_construct ("nested_list", [(Type_var "a")]))]);
           ("List",
            [(Type_construct ("nested_list",
                [(Type_var "a"); (Type_construct ("nested_list", []))]))
              ])
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
        (("Leaf", []),
         [("Node",
           [(Type_var "a"); (Type_construct ("tree", [(Type_var "a")]));
             (Type_construct ("tree", [(Type_var "a")]))])
           ])
        ))
      ]|}]
;;

(*bad*)
let%expect_test "adt list with pair" =
  test_program
    {| type ('a, 'b) pair_list = Nil 
    | Cons of ('a * 'b) * ('a, 'b) pair_list;;
|};
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_tests__Parser.test_program in file "tests/parser.ml", line 9, characters 51-66
  Called from Ocamladt_tests__Parser.(fun) in file "tests/parser.ml", line 1276, characters 2-102
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "adt list with 2 el in node" =
  test_program
    {| type ('a, 'b) pair_list = Nil 
    | Cons of 'a * 'b * ('a, 'b) pair_list;;
|};
  [%expect
    {|
    [(Str_adt (["a"; "b"], "pair_list",
        (("Nil", []),
         [("Cons",
           [(Type_var "a"); (Type_var "b");
             (Type_construct ("pair_list", [(Type_var "a"); (Type_var "b")]))])
           ])
        ))
      ]
        |}]
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
        (("Point", [(Type_construct ("int", []))]),
         [("Circle", [(Type_construct ("int", [])); (Type_construct ("int", []))]);
           ("Rect",
            [(Type_construct ("int", [])); (Type_construct ("int", []));
              (Type_construct ("int", []))])
           ])
        ))
      ] |}]
;;

let%expect_test "simple adt with pattern matching function (else case) + printing" =
  test_program
    {|
type shape = Circle of int
  | Rectangle of int * int 
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
        (("Circle", [(Type_construct ("int", []))]),
         [("Rectangle",
           [(Type_construct ("int", [])); (Type_construct ("int", []))]);
           ("Square", [(Type_construct ("int", []))])])
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
      ]|}]
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
        ({ pat = (Pat_constraint ((Pat_var "x"), (Type_var "char")));
           expr = (Exp_constant (Const_integer 20)) },
         [])
        ))
      ] |}]
;;

let%expect_test "keyword" =
  test_program {|let (x:(char*char)) = 20;;|};
  [%expect{|
    [(Str_value (Nonrecursive,
        ({ pat =
           (Pat_constraint ((Pat_var "x"),
              (Type_tuple ((Type_var "char"), (Type_var "char"), []))));
           expr = (Exp_constant (Const_integer 20)) },
         [])
        ))
      ] |}]
;;
let%expect_test "keyword" =
  test_program {|let (x:int option) = 20;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Ocamladt_tests__Parser.test_programm in file "tests/parser.ml", line 9, characters 52-67
  Called from Ocamladt_tests__Parser.(fun) in file "tests/parser.ml", line 1183, characters 2-45
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]
;;

let%expect_test "keyword" =
  test_program {||};
  [%expect{| [] |}]
;;

let%expect_test "keyword" =
  test_program {|let () =  print_int 5;;|};
  [%expect{|
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
  [%expect{|
    [(Str_value (Nonrecursive,
        ({ pat = (Pat_var "addi");
           expr =
           (Exp_fun (((Pat_var "f"), [(Pat_var "g"); (Pat_var "x")]),
              (Exp_constraint (
                 (Exp_apply ((Exp_apply ((Exp_ident "f"), (Exp_ident "x"))),
                    (Exp_constraint (
                       (Exp_apply ((Exp_ident "g"), (Exp_ident "x"))),
                       (Type_var "bool")))
                    )),
                 (Type_var "int")))
              ))
           },
         [])
        ))
      ] |}]

let%expect_test "simple adt with pattern matching function + printing v3" =
  test_program
    {|
type shape = Circle of int
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
    [(Str_adt ([], "shape",
        (("Circle", [(Type_construct ("int", []))]),
         [("Rectangle",
           [(Type_construct ("int", [])); (Type_construct ("int", []))]);
           ("Square", [(Type_construct ("int", []))])])
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
      ]|}]
;;
