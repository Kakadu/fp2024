(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast
open OCamlRV_lib.Parser

let parse_to_unit input =
  match parse input with
  | Ok structure -> Stdlib.Format.printf "%s\n" (show_structure structure)
  | Error err -> Stdlib.Format.printf "%s\n" err
;;

let parse_to_bool input =
  match parse input with
  | Ok _ -> true
  | Error _ -> false
;;

(* if-then-else tests *)
let%test _ = parse_to_bool "if x then y else z;;"
let%test _ = parse_to_bool "if 5 > 3 then true else false;;"
let%test _ = parse_to_bool "if a then b else c;;"
let%test _ = parse_to_bool "if x then 1 + 2 else 3;;"
let%test _ = parse_to_bool "if true then false else true;;"

(* number tests *)
let%test _ = parse_to_bool "-5;;"
let%test _ = parse_to_bool "2134324;;"
let%test _ = parse_to_bool "-525;;"
let%test _ = parse_to_bool "true;;"
let%test _ = parse_to_bool "false;;"

(* binary operator tests *)
let%test _ = parse_to_bool "5 + 5;;"
let%test _ = parse_to_bool "5+5;;"
let%test _ = parse_to_bool "2 - 3;;"
let%test _ = parse_to_bool " 2 -2 -2;;"
let%test _ = parse_to_bool "4 * 4;;"

(* -------------------simple let expressions-------------------*)

let%expect_test _ =
  parse_to_unit "let x = 5;;";
  [%expect {| [(SValue (NonRec, ((PVar "x"), (ExprConstant (CInt 5))), []))] |}]
;;

let%expect_test _ =
  parse_to_unit "let feets = 5280;;";
  [%expect {| [(SValue (NonRec, ((PVar "feets"), (ExprConstant (CInt 5280))), []))] |}]
;;

let%expect_test _ =
  parse_to_unit {| let lie = "i love Ocaml";; |};
  [%expect
    {|
[(SValue (NonRec, ((PVar "lie"), (ExprConstant (CString "i love Ocaml"))), 
    []))
  ]|}]
;;

let%expect_test _ =
  parse_to_unit "let list = [];;";
  [%expect {| [(SValue (NonRec, ((PVar "list"), (ExprConstant CNil)), []))] |}]
;;

let%expect_test _ =
  parse_to_unit {| let t = (1, "2", 3);; |};
  [%expect
    {|
    [(SValue (NonRec,
        ((PVar "t"),
         (ExprTuple ((ExprConstant (CInt 1)), (ExprConstant (CString "2")),
            [(ExprConstant (CInt 3))]))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_to_unit "a::b;;";
  [%expect {|
    [(SEval (ExprCons ((ExprVariable "a"), (ExprVariable "b"))))] |}]
;;

(*-------------------if expressions-------------------*)

let%expect_test _ =
  parse_to_unit "if 5 > 3 then true else false;;";
  [%expect
    {|
         [(SEval
             (ExprIf (
                (ExprBinOperation (Gt, (ExprConstant (CInt 5)),
                   (ExprConstant (CInt 3)))),
                (ExprConstant (CBool true)), (Some (ExprConstant (CBool false))))))
           ] |}]
;;

let%expect_test _ =
  parse_to_unit "if x then 1 + 2 else 3;;";
  [%expect
    {|
     [(SEval
         (ExprIf ((ExprVariable "x"),
            (ExprBinOperation (Add, (ExprConstant (CInt 1)),
               (ExprConstant (CInt 2)))),
            (Some (ExprConstant (CInt 3))))))
       ] |}]
;;

let%expect_test _ =
  parse_to_unit "if x > y then x * y else square x;;";
  [%expect
    {|
     [(SEval
         (ExprIf ((ExprBinOperation (Gt, (ExprVariable "x"), (ExprVariable "y"))),
            (ExprBinOperation (Mul, (ExprVariable "x"), (ExprVariable "y"))),
            (Some (ExprApply ((ExprVariable "square"), (ExprVariable "x")))))))
       ] |}]
;;

let%expect_test _ =
  parse_to_unit "let (a, b) = (1, 2);;";
  [%expect
    {|
  [(SValue (NonRec,
      ((PTuple ((PVar "a"), (PVar "b"), [])),
       (ExprTuple ((ExprConstant (CInt 1)), (ExprConstant (CInt 2)), []))),
      []))
    ] |}]
;;

let%expect_test _ =
  parse_to_unit {|
  match a with 
    | None -> ()
    | Some e -> ()
  ;;
  |};
  [%expect
    {|
    [(SEval
        (ExprMatch ((ExprVariable "a"), ((POption None), (ExprConstant CUnit)),
           [((POption (Some (PVar "e"))), (ExprConstant CUnit))])))
      ] |}]
;;

(*------------------- Factorial and Fibonacci -------------------*)

let%expect_test "fibo test" =
  parse_to_unit "let rec fibo n = if n <= 1 then n else fibo (n - 1) + fibo (n - 2);;";
  [%expect
    {|
     [(SValue (Rec,
         ((PVar "fibo"),
          (ExprFun ((PVar "n"),
             (ExprIf (
                (ExprBinOperation (Lte, (ExprVariable "n"),
                   (ExprConstant (CInt 1)))),
                (ExprVariable "n"),
                (Some (ExprBinOperation (Add,
                         (ExprApply ((ExprVariable "fibo"),
                            (ExprBinOperation (Sub, (ExprVariable "n"),
                               (ExprConstant (CInt 1))))
                            )),
                         (ExprApply ((ExprVariable "fibo"),
                            (ExprBinOperation (Sub, (ExprVariable "n"),
                               (ExprConstant (CInt 2))))
                            ))
                         )))
                ))
             ))),
         []))
       ] |}]
;;

let%expect_test "fib test" =
  parse_to_unit
    "let rec fib_loop m n i = if i = 0 then m else fib_loop n (n + m) (i - 1);;";
  [%expect
    {|
     [(SValue (Rec,
         ((PVar "fib_loop"),
          (ExprFun ((PVar "m"),
             (ExprFun ((PVar "n"),
                (ExprFun ((PVar "i"),
                   (ExprIf (
                      (ExprBinOperation (Eq, (ExprVariable "i"),
                         (ExprConstant (CInt 0)))),
                      (ExprVariable "m"),
                      (Some (ExprApply (
                               (ExprApply (
                                  (ExprApply ((ExprVariable "fib_loop"),
                                     (ExprVariable "n"))),
                                  (ExprBinOperation (Add, (ExprVariable "n"),
                                     (ExprVariable "m")))
                                  )),
                               (ExprBinOperation (Sub, (ExprVariable "i"),
                                  (ExprConstant (CInt 1))))
                               )))
                      ))
                   ))
                ))
             ))),
         []))
       ] |}]
;;

(* Factorial test *)
let%expect_test "factorial test" =
  parse_to_unit "let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;";
  [%expect
    {|
[(SValue (Rec,
    ((PVar "fact"),
     (ExprFun ((PVar "n"),
        (ExprIf (
           (ExprBinOperation (Lte, (ExprVariable "n"),
              (ExprConstant (CInt 1)))),
           (ExprConstant (CInt 1)),
           (Some (ExprBinOperation (Mul, (ExprVariable "n"),
                    (ExprApply ((ExprVariable "fact"),
                       (ExprBinOperation (Sub, (ExprVariable "n"),
                          (ExprConstant (CInt 1))))
                       ))
                    )))
           ))
        ))),
    []))
  ]
|}]
;;

(*------------------- Mathc expression tests -------------------*)

let%expect_test "match" =
  parse_to_unit {|match x with
| 0 -> "zero"
| 1 -> "one"
| _ -> "other"
;;
|};
  [%expect
    {|
[(SEval
    (ExprMatch ((ExprVariable "x"),
       ((PConstant (CInt 0)), (ExprConstant (CString "zero"))),
       [((PConstant (CInt 1)), (ExprConstant (CString "one")));
         (PAny, (ExprConstant (CString "other")))]
       )))
  ]
|}]
;;

let%expect_test "value equals match" =
  parse_to_unit
    {|let numder = match arabic with 
| 1 -> "one"
| 2 -> "two"
| 3 -> "three";;|};
  [%expect
    {|
     [(SValue (NonRec,
         ((PVar "numder"),
          (ExprMatch ((ExprVariable "arabic"),
             ((PConstant (CInt 1)), (ExprConstant (CString "one"))),
             [((PConstant (CInt 2)), (ExprConstant (CString "two")));
               ((PConstant (CInt 3)), (ExprConstant (CString "three")))]
             ))),
         []))
       ] |}]
;;

let%expect_test "bin operations with if then else" =
  parse_to_unit {| 1 + if a then b else c;; |};
  [%expect
    {|
    [(SEval
        (ExprBinOperation (Add, (ExprConstant (CInt 1)),
           (ExprIf ((ExprVariable "a"), (ExprVariable "b"),
              (Some (ExprVariable "c"))))
           )))
      ]
  |}]
;;

let%expect_test "cons test" =
  parse_to_unit "1::2::3::[];;";
  [%expect
    {|
    [(SEval
        (ExprCons ((ExprConstant (CInt 1)),
           (ExprCons ((ExprConstant (CInt 2)),
              (ExprCons ((ExprConstant (CInt 3)), (ExprConstant CNil)))))
           )))
      ]
  |}]
;;

let%expect_test "sum list test" =
  parse_to_unit
    {| let rec sum_list lst = match lst with | [] -> 0 | x::xs -> x + sum_list xs;; |};
  [%expect
    {|
    [(SValue (Rec,
        ((PVar "sum_list"),
         (ExprFun ((PVar "lst"),
            (ExprMatch ((ExprVariable "lst"),
               ((PConstant CNil), (ExprConstant (CInt 0))),
               [((PCons ((PVar "x"), (PVar "xs"))),
                 (ExprBinOperation (Add, (ExprVariable "x"),
                    (ExprApply ((ExprVariable "sum_list"), (ExprVariable "xs")))
                    )))
                 ]
               ))
            ))),
        []))
      ] |}]
;;

let%expect_test "double list test" =
  parse_to_unit
    "let rec double_list lst = match lst with | [] -> [] | x::xs -> (2 * x)::double_list \
     xs;;";
  [%expect
    {|
     [(SValue (Rec,
         ((PVar "double_list"),
          (ExprFun ((PVar "lst"),
             (ExprMatch ((ExprVariable "lst"),
                ((PConstant CNil), (ExprConstant CNil)),
                [((PCons ((PVar "x"), (PVar "xs"))),
                  (ExprCons (
                     (ExprBinOperation (Mul, (ExprConstant (CInt 2)),
                        (ExprVariable "x"))),
                     (ExprApply ((ExprVariable "double_list"), (ExprVariable "xs")
                        ))
                     )))
                  ]
                ))
             ))),
         []))
       ] |}]
;;

let%expect_test "unary tests" =
  parse_to_unit "let b = not (x > 5);;";
  [%expect
    {|
    [(SValue (NonRec,
        ((PVar "b"),
         (ExprUnOperation (UnaryNeg,
            (ExprBinOperation (Gt, (ExprVariable "x"), (ExprConstant (CInt 5))))
            ))),
        []))
      ] |}]
;;

(* type annotation tests *)

let%expect_test "" =
  parse_to_unit "let (a : int) = 5;;";
  [%expect
    {|
      [(SValue (NonRec, ((PType ((PVar "a"), AInt)), (ExprConstant (CInt 5))), []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit {| let (a : string) = "hello";; |};
  [%expect
    {|
      [(SValue (NonRec,
          ((PType ((PVar "a"), AString)), (ExprConstant (CString "hello"))),
          []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit "let (a : bool) = true;;";
  [%expect
    {|
      [(SValue (NonRec, ((PType ((PVar "a"), ABool)), (ExprConstant (CBool true))),
          []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit "let (a : unit) = ();;";
  [%expect
    {|
      [(SValue (NonRec, ((PType ((PVar "a"), AUnit)), (ExprConstant CUnit)), []))] |}]
;;

let%expect_test "" =
  parse_to_unit "let (a : int list) = [];;";
  [%expect
    {|
      [(SValue (NonRec, ((PType ((PVar "a"), (AList AInt))), (ExprConstant CNil)),
          []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit "let f (x : int) (y : int) = x + y;;";
  [%expect
    {|
      [(SValue (NonRec,
          ((PVar "f"),
           (ExprFun ((PType ((PVar "x"), AInt)),
              (ExprFun ((PType ((PVar "y"), AInt)),
                 (ExprBinOperation (Add, (ExprVariable "x"), (ExprVariable "y")))))
              ))),
          []))
        ] |}]
;;
