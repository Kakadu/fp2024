(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Ast

let test_parser s =
  match parse s with
  | Ok s -> print_endline (show_program s)
  | Error e -> print_endline e
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
