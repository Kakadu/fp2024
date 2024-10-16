(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Ast
open Miniml.Parser
open Miniml.Parser_utility
open Miniml.Printer

let test input =
  print_string (string_of_parse_result show_program (parse program_parser input))
;;

let%expect_test _ =
  test
    {|
   let rec factorial n = if (n > 1) then n * factorial(n-1) else 1;;
   factorial 5;;
   |};
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
      (EvalItem (Apply ((Variable "factorial"), [(Const (IntLiteral 5))])))] |}];
  test
    {|
   let rec f x = 
      if x > 0 then x + (g (x-1)) else 0 
   and g x = 
      (f (x / 2)) - x 
   ;;
   |};
  [%expect
    {|
    [(DefineItem
        (Recursive,
         [((PVar "f"),
           (Lambda ([(PVar "x")],
              (If ((Binary ((Variable "x"), Gt, (Const (IntLiteral 0)))),
                 (Binary ((Variable "x"), Add,
                    (Apply ((Variable "g"),
                       [(Binary ((Variable "x"), Subtract, (Const (IntLiteral 1))
                           ))
                         ]
                       ))
                    )),
                 (Some (Const (IntLiteral 0)))))
              )));
           ((PVar "g"),
            (Lambda ([(PVar "x")],
               (Binary (
                  (Apply ((Variable "f"),
                     [(Binary ((Variable "x"), Division, (Const (IntLiteral 2))))
                       ]
                     )),
                  Subtract, (Variable "x")))
               )))
           ]))
      ] |}];
  test
    {|
   let t1 = (1,2,3, f x);;
   let t2 = (print_endline (f -y); (not x) && g (x = y));;
   (t1, t2);;
   |};
  [%expect
    {|
    [(DefineItem
        (Nonrecursive,
         [((PVar "t1"),
           (Tuple
              [(Const (IntLiteral 1)); (Const (IntLiteral 2));
                (Const (IntLiteral 3));
                (Apply ((Variable "f"), [(Variable "x")]))]))
           ]));
      (DefineItem
         (Nonrecursive,
          [((PVar "t2"),
            (ExpressionBlock
               [(Apply ((Variable "print_endline"),
                   [(Binary ((Variable "f"), Subtract, (Variable "y")))]));
                 (Binary ((Apply ((Variable "not"), [(Variable "x")])), And,
                    (Apply ((Variable "g"),
                       [(Binary ((Variable "x"), Equals, (Variable "y")))]))
                    ))
                 ]))
            ]));
      (EvalItem (Tuple [(Variable "t1"); (Variable "t2")]))] |}]
;;
