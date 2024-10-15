(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Ast
open Miniml.Parser
open Miniml.Parser_utility
open Miniml.Printer

(* Tests for program parser *)
let%expect_test _ =
  print_string
    (string_of_parse_result
       show_program
       (parse
          program_parser
          {|
        let rec factorial n = if (n > 1) then n * factorial(n-1) else 1;;
        factorial 5;;
        |}));
  [%expect
    {|
    [(DefineItem
        (Recursive,
         [((PVar "factorial"),
           (Lambda ([(PVar "n")],
              (If ((Binary ((Variable "n"), Gt, (Const (IntLiteral 1)))),
                 (Binary ((Variable "n"), Multiply,
                    (Apply ((Variable "factorial"),
                       [(Binary ((Variable "n"), Subtract, (Const (IntLiteral 1))
                           ))
                         ]
                       ))
                    )),
                 (Some (Const (IntLiteral 1)))))
              )))
           ]));
      (EvalItem (Apply ((Variable "factorial"), [(Const (IntLiteral 5))])))]
     |}]
;;
