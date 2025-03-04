(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Ast

let test_parser s =
  match parse s with
  | Ok s -> print_endline (show_program s)
  | Error e -> print_endline "Parsing error"
;;

let%expect_test "parse simple let" =
  test_parser "let x = 23";
  [%expect {|[(SValue (Nonrecursive, ((PVar "x"), (EConst (CInt 23))), []))]|}]
;;

let%expect_test "parse factorial function" =
  test_parser "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1)";
  [%expect
    {|
  [(SValue (Recursive,
      ((PVar "factorial"),
       (EFun ((PVar "n"),
          (EIf ((EBinary (Lt, (EVar "n"), (EConst (CInt 2)))),
             (EConst (CInt 1)),
             (Some (EBinary (Mul, (EVar "n"),
                      (EApply ((EVar "factorial"),
                         (EBinary (Sub, (EVar "n"), (EConst (CInt 1))))))
                      )))
             ))
          ))),
      []))
    ]|}]
;;

(* ========== const ========== *)

let%expect_test "parse int" =
  test_parser "let () = 23";
  [%expect {|[(SValue (Nonrecursive, (PUnit, (EConst (CInt 23))), []))]|}]
;;

let%expect_test "parse bool t" =
  test_parser "let () = true";
  [%expect {|[(SValue (Nonrecursive, (PUnit, (EConst (CBool true))), []))]|}]
;;

let%expect_test "parse bool f" =
  test_parser "let () = false";
  [%expect {|[(SValue (Nonrecursive, (PUnit, (EConst (CBool false))), []))]|}]
;;

(* ========== bop ========== *)

let%expect_test "parse compare int" =
  test_parser "let () = 2 > 56";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EBinary (Gt, (EConst (CInt 2)), (EConst (CInt 56))))), []))
      ]|}]
;;

let%expect_test "parse compare int" =
  test_parser "let () = 2 < 56";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EBinary (Lt, (EConst (CInt 2)), (EConst (CInt 56))))), []))
      ]|}]
;;

let%expect_test "parse compare int" =
  test_parser "let () = 2 >= 56";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EBinary (Gte, (EConst (CInt 2)), (EConst (CInt 56))))),
        []))
      ]|}]
;;

let%expect_test "parse compare int" =
  test_parser "let () = 2 <= 56";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EBinary (Lte, (EConst (CInt 2)), (EConst (CInt 56))))),
        []))
      ]|}]
;;

let%expect_test "parse compare int" =
  test_parser "let () = 2 = 56";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EBinary (Eq, (EConst (CInt 2)), (EConst (CInt 56))))), []))
      ]|}]
;;

let%expect_test "parse compare int" =
  test_parser "let () = 2 <> 56";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EBinary (NEq, (EConst (CInt 2)), (EConst (CInt 56))))),
        []))
      ]|}]
;;

let%expect_test "parse compare and" =
  test_parser "let () = true && false";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EBinary (And, (EConst (CBool true)), (EConst (CBool false))))),
        []))
      ]|}]
;;

let%expect_test "parse compare or" =
  test_parser "let () = false || true";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EBinary (Or, (EConst (CBool false)), (EConst (CBool true))))),
        []))
      ]|}]
;;

(* ========== tuples ========== *)

let%expect_test "parse tuple 2" =
  test_parser "let () = (12, 23)";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (ETuple ((EConst (CInt 12)), (EConst (CInt 23)), []))), []))
      ]|}]
;;

let%expect_test "parse tuple 3" =
  test_parser "let () = (12, 23, 45)";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit,
         (ETuple ((EConst (CInt 12)), (EConst (CInt 23)), [(EConst (CInt 45))]))),
        []))
      ]|}]
;;

let%expect_test "parse tuple 2 d" =
  test_parser "let () = (12, true)";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (ETuple ((EConst (CInt 12)), (EConst (CBool true)), []))),
        []))
      ]|}]
;;

let%expect_test "parse tuple 3 d" =
  test_parser "let () = (12, true, ())";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit,
         (ETuple ((EConst (CInt 12)), (EConst (CBool true)), [(EConst CUnit)]))),
        []))
      ]|}]
;;

let%expect_test "parse tuple inn" =
  test_parser "let () = ((12, 23), 45, (345, false, true))";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit,
         (ETuple ((ETuple ((EConst (CInt 12)), (EConst (CInt 23)), [])),
            (EConst (CInt 45)),
            [(ETuple ((EConst (CInt 345)), (EConst (CBool false)),
                [(EConst (CBool true))]))
              ]
            ))),
        []))
      ]|}]
;;

(* ========== list ========== *)

let%expect_test "parse list empty" =
  test_parser "let () = []";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EList [])), []))]|}]
;;

let%expect_test "parse list 1" =
  test_parser "let () = [23]";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EList [(EConst (CInt 23))])), []))]|}]
;;

let%expect_test "parse list 2" =
  test_parser "let () = [23; 45]";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit, (EList [(EConst (CInt 23)); (EConst (CInt 45))])), []))
      ]|}]
;;

let%expect_test "parse list list" =
  test_parser "let () = [[234; 456]; []]";
  [%expect
    {|
    [(SValue (Nonrecursive,
        (PUnit,
         (EList [(EList [(EConst (CInt 234)); (EConst (CInt 456))]); (EList [])])),
        []))
      ]|}]
;;

(* ========== option ========== *)

let%expect_test "parse option var" =
  test_parser "let () = Some x";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EOption (Some (EVar "x")))), []))]|}]
;;

let%expect_test "parse option p var" =
  test_parser "let () = Some (x)";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EOption (Some (EVar "x")))), []))]|}]
;;

let%expect_test "parse option none" =
  test_parser "let () = None";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EOption None)), []))]|}]
;;

(* ========== vars ========== *)

let%expect_test "parse var simple" =
  test_parser "let () = asd";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EVar "asd")), []))]|}]
;;

let%expect_test "parse var start underscore" =
  test_parser "let () = _asd";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EVar "_asd")), []))]|}]
;;

let%expect_test "parse var underscore fail" =
  test_parser "let () = _";
  [%expect {|
    Parsing error|}]
;;

let%expect_test "parse var double underscore not fail" =
  test_parser "let () = __";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EVar "__")), []))]|}]
;;

let%expect_test "parse underscore number" =
  test_parser "let () = _0kajsndf";
  [%expect {|
    [(SValue (Nonrecursive, (PUnit, (EVar "_0kajsndf")), []))]|}]
;;

let%expect_test "parse number" =
  test_parser "let () = 0kajsndf";
  [%expect {|
    Parsing error|}]
;;

(* ========== fun ========== *)

let%expect_test "parse fun simple" =
  test_parser "let () = fun x -> y";
  [%expect
    {|
    [(SValue (Nonrecursive, (PUnit, (EFun ((PVar "x"), (EVar "y")))), []))]|}]
;;
