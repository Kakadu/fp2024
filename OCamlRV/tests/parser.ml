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
let%test _ = parse_to_bool "if x then y else z"
let%test _ = parse_to_bool "if 5 > 3 then true else false"
let%test _ = parse_to_bool "if a then b else c"
let%test _ = parse_to_bool "if x then 1 + 2 else 3"
let%test _ = parse_to_bool "if true then false else true"

(* number tests *)
let%test _ = parse_to_bool "-5"
let%test _ = parse_to_bool "2134324"
let%test _ = parse_to_bool "-525"
let%test _ = parse_to_bool "true"
let%test _ = parse_to_bool "false"

(* binary operator tests *)
let%test _ = parse_to_bool "5 + 5"
let%test _ = parse_to_bool "5+5"
let%test _ = parse_to_bool "2 - 3"
let%test _ = parse_to_bool " 2 -2 -2"
let%test _ = parse_to_bool "4 * 4"

(* -------------------simple let expressions-------------------*)

let%expect_test _ =
  parse_to_unit "let x = 5";
  [%expect {| [(SValue (NonRec, ((PVar "x"), (ExprLiteral (IntLiteral 5))), []))] |}]
;;

let%expect_test _ =
  parse_to_unit "let feets = 5280;;";
  [%expect
    {| [(SValue (NonRec, ((PVar "feets"), (ExprLiteral (IntLiteral 5280))), []))] |}]
;;

let%expect_test _ =
  parse_to_unit {|let lie = "i love Ocaml|};
  [%expect {|
     [] |}]
;;

let%expect_test _ =
  parse_to_unit "let list = []";
  [%expect {| [(SValue (NonRec, ((PVar "list"), (ExprLiteral NilLiteral)), []))] |}]
;;

let%expect_test _ =
  parse_to_unit "let t = (1, \"2\", 3)";
  [%expect
    {|
    [(SValue (NonRec,
        ((PVar "t"),
         (ExprTuple ((ExprLiteral (IntLiteral 1)),
            (ExprLiteral (StringLiteral "2")), [(ExprLiteral (IntLiteral 3))]))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_to_unit "a::b";
  [%expect {|
    [(SEval (ExprCons ((ExprVariable "a"), (ExprVariable "b"))))] |}]
;;

(*-------------------if expressions-------------------*)

let%expect_test _ =
  parse_to_unit "if 5 > 3 then true else false";
  [%expect
    {|
         [(SEval
             (ExprIf (
                (ExprBinOperation (Gt, (ExprLiteral (IntLiteral 5)),
                   (ExprLiteral (IntLiteral 3)))),
                (ExprLiteral (BoolLiteral true)),
                (Some (ExprLiteral (BoolLiteral false))))))
           ] |}]
;;

let%expect_test _ =
  parse_to_unit "if x then 1 + 2 else 3";
  [%expect
    {|
     [(SEval
         (ExprIf ((ExprVariable "x"),
            (ExprBinOperation (Add, (ExprLiteral (IntLiteral 1)),
               (ExprLiteral (IntLiteral 2)))),
            (Some (ExprLiteral (IntLiteral 3))))))
       ] |}]
;;

let%expect_test _ =
  parse_to_unit "if x > y then x * y else square x";
  [%expect
    {|
     [(SEval
         (ExprIf ((ExprBinOperation (Gt, (ExprVariable "x"), (ExprVariable "y"))),
            (ExprBinOperation (Mul, (ExprVariable "x"), (ExprVariable "y"))),
            (Some (ExprApply ((ExprVariable "square"), (ExprVariable "x")))))))
       ] |}]
;;

let%expect_test _ =
  parse_to_unit "let (a, b) = (1, 2)";
  [%expect
    {|
  [(SValue (NonRec,
      ((PTuple ((PVar "a"), (PVar "b"), [])),
       (ExprTuple ((ExprLiteral (IntLiteral 1)), (ExprLiteral (IntLiteral 2)),
          []))),
      []))
    ] |}]
;;

let%expect_test _ =
  parse_to_unit {|
  match a with 
    | None -> ()
    | Some e -> ()
  |};
  [%expect
    {|
    [(SEval
        (ExprMatch ((ExprVariable "a"),
           ((POption None), (ExprLiteral UnitLiteral)),
           [((POption (Some (PVar "e"))), (ExprLiteral UnitLiteral))])))
      ] |}]
;;

(*------------------- Factorial and Fibonacci -------------------*)

let%expect_test "fibo test" =
  parse_to_unit " let rec fibo n = if n <= 1 then n else fibo (n - 1) + fibo (n - 2)";
  [%expect
    {|
     [(SValue (Rec,
         ((PVar "fibo"),
          (ExprFun ((PVar "n"),
             (ExprIf (
                (ExprBinOperation (Lte, (ExprVariable "n"),
                   (ExprLiteral (IntLiteral 1)))),
                (ExprVariable "n"),
                (Some (ExprBinOperation (Add,
                         (ExprApply ((ExprVariable "fibo"),
                            (ExprBinOperation (Sub, (ExprVariable "n"),
                               (ExprLiteral (IntLiteral 1))))
                            )),
                         (ExprApply ((ExprVariable "fibo"),
                            (ExprBinOperation (Sub, (ExprVariable "n"),
                               (ExprLiteral (IntLiteral 2))))
                            ))
                         )))
                ))
             ))),
         []))
       ] |}]
;;

let%expect_test "fib test" =
  parse_to_unit "let rec fib_loop m n i = if i = 0 then m else fib_loop n (n + m) (i - 1)";
  [%expect
    {|
     [(SValue (Rec,
         ((PVar "fib_loop"),
          (ExprFun ((PVar "m"),
             (ExprFun ((PVar "n"),
                (ExprFun ((PVar "i"),
                   (ExprIf (
                      (ExprBinOperation (Eq, (ExprVariable "i"),
                         (ExprLiteral (IntLiteral 0)))),
                      (ExprVariable "m"),
                      (Some (ExprApply (
                               (ExprApply (
                                  (ExprApply ((ExprVariable "fib_loop"),
                                     (ExprVariable "n"))),
                                  (ExprBinOperation (Add, (ExprVariable "n"),
                                     (ExprVariable "m")))
                                  )),
                               (ExprBinOperation (Sub, (ExprVariable "i"),
                                  (ExprLiteral (IntLiteral 1))))
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
  parse_to_unit "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)";
  [%expect
    {|
[(SValue (Rec,
    ((PVar "fact"),
     (ExprFun ((PVar "n"),
        (ExprIf (
           (ExprBinOperation (Lte, (ExprVariable "n"),
              (ExprLiteral (IntLiteral 1)))),
           (ExprLiteral (IntLiteral 1)),
           (Some (ExprBinOperation (Mul, (ExprVariable "n"),
                    (ExprApply ((ExprVariable "fact"),
                       (ExprBinOperation (Sub, (ExprVariable "n"),
                          (ExprLiteral (IntLiteral 1))))
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
| _ -> "other"|};
  [%expect
    {|
[(SEval
    (ExprMatch ((ExprVariable "x"),
       ((PLiteral (IntLiteral 0)), (ExprLiteral (StringLiteral "zero"))),
       [((PLiteral (IntLiteral 1)), (ExprLiteral (StringLiteral "one")));
         (PAny, (ExprLiteral (StringLiteral "other")))]
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
             ((PLiteral (IntLiteral 1)), (ExprLiteral (StringLiteral "one"))),
             [((PLiteral (IntLiteral 2)), (ExprLiteral (StringLiteral "two")));
               ((PLiteral (IntLiteral 3)), (ExprLiteral (StringLiteral "three")))]
             ))),
         []))
       ] |}]
;;

let%expect_test "bin operations with if then else" =
  parse_to_unit {|1 + if a then b else c|};
  [%expect
    {|
    [(SEval
        (ExprBinOperation (Add, (ExprLiteral (IntLiteral 1)),
           (ExprIf ((ExprVariable "a"), (ExprVariable "b"),
              (Some (ExprVariable "c"))))
           )))
      ]
  |}]
;;

let%expect_test "cons test" =
  parse_to_unit {| 1::2::3::[];; |};
  [%expect
    {|
    [(SEval
        (ExprCons ((ExprLiteral (IntLiteral 1)),
           (ExprCons ((ExprLiteral (IntLiteral 2)),
              (ExprCons ((ExprLiteral (IntLiteral 3)), (ExprLiteral NilLiteral)))
              ))
           )))
      ]
  |}]
;;

let%expect_test "sum list test" =
  parse_to_unit
    {|let rec sum_list lst = match lst with | [] -> 0 | x::xs -> x + sum_list xs|};
  [%expect
    {|
    [(SValue (Rec,
        ((PVar "sum_list"),
         (ExprFun ((PVar "lst"),
            (ExprMatch ((ExprVariable "lst"),
               ((PLiteral NilLiteral), (ExprLiteral (IntLiteral 0))),
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
     xs";
  [%expect
    {|
     [(SValue (Rec,
         ((PVar "double_list"),
          (ExprFun ((PVar "lst"),
             (ExprMatch ((ExprVariable "lst"),
                ((PLiteral NilLiteral), (ExprLiteral NilLiteral)),
                [((PCons ((PVar "x"), (PVar "xs"))),
                  (ExprCons (
                     (ExprBinOperation (Mul, (ExprLiteral (IntLiteral 2)),
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
  parse_to_unit "let b = not (x > 5)";
  [%expect
    {|
    [(SValue (NonRec,
        ((PVar "b"),
         (ExprUnOperation (UnaryNeg,
            (ExprBinOperation (Gt, (ExprVariable "x"),
               (ExprLiteral (IntLiteral 5))))
            ))),
        []))
      ] |}]
;;

(* type annotation tests *)

let%expect_test "" =
  parse_to_unit "let (a : int) = 5";
  [%expect
    {|
      [(SValue (NonRec, ((PType ((PVar "a"), AInt)), (ExprLiteral (IntLiteral 5))),
          []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit {| let (a : string) = "hello" |};
  [%expect
    {|
      [(SValue (NonRec,
          ((PType ((PVar "a"), AString)), (ExprLiteral (StringLiteral "hello"))),
          []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit "let (a : bool) = true";
  [%expect
    {|
      [(SValue (NonRec,
          ((PType ((PVar "a"), ABool)), (ExprLiteral (BoolLiteral true))),
          []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit "let (a : unit) = ()";
  [%expect
    {|
      [(SValue (NonRec, ((PType ((PVar "a"), AUnit)), (ExprLiteral UnitLiteral)),
          []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit "let (a : int list) = []";
  [%expect
    {|
      [(SValue (NonRec,
          ((PType ((PVar "a"), (AList AInt))), (ExprLiteral NilLiteral)), []))
        ] |}]
;;

let%expect_test "" =
  parse_to_unit "let (a : int * int) = (1, 2)";
  [%expect {| [] |}]
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
