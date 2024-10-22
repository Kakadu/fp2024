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
  [%expect {| [(SValue (NonRec, ((PVar "x"), (ExprLiteral (IntLiteral 5)))))] |}]
;;

let%expect_test _ =
  parse_to_unit "let feets = 5280;;";
  [%expect {| [(SValue (NonRec, ((PVar "feets"), (ExprLiteral (IntLiteral 5280)))))] |}]
;;

let%expect_test _ =
  parse_to_unit "let lie = \"i love Ocaml\"";
  [%expect
    {|
     [(SValue (NonRec,
         ((PVar "lie"), (ExprLiteral (StringLiteral "i love Ocaml")))))
       ] |}]
;;

let%expect_test _ =
  parse_to_unit "let list = []";
  [%expect {| [(SValue (NonRec, ((PVar "list"), (ExprLiteral NilLiteral))))] |}]
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

(*------------------- Factorial and Fibonacci -------------------*)

let%expect_test _ =
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
             )))
         ))
       ] |}]
;;

let%expect_test _ =
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
             )))
         ))
       ] |}]
;;

(* Factorial test *)
let%expect_test _ =
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
        )))
    ))
  ]
|}]
;;
