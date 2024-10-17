(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Ast
open Miniml.Parser
open Miniml.Parser_utility
open Miniml.Printer

let test conv p input = print_string (string_of_parse_result conv (parse p input))
let test_program = test show_program program_parser

let%expect_test _ =
  test_program {|if f x then g 10;;|};
  [%expect
    {|
    [(EvalItem
        (If ((Apply ((Variable "f"), [(Variable "x")])),
           (Apply ((Variable "g"), [(Const (IntLiteral 10))])), None)))
      ] |}];
  test_program {|iff x then g 10;;|};
  [%expect {| [] |}];
  test_program {|if f x theng 10;;|};
  [%expect {| ParseError(line=1 pos=15): Not found 'then' branch for if-expression |}];
  test_program {|if f x then g 10 else ~-10;;|};
  [%expect
    {|
    [(EvalItem
        (If ((Apply ((Variable "f"), [(Variable "x")])),
           (Apply ((Variable "g"), [(Const (IntLiteral 10))])),
           (Some (Unary (Negate, (Const (IntLiteral 10))))))))
      ] |}];
  test_program {|if f x then g 10 else~-10;;|};
  [%expect
    {|
    [(EvalItem
        (If ((Apply ((Variable "f"), [(Variable "x")])),
           (Apply ((Variable "g"), [(Const (IntLiteral 10))])),
           (Some (Unary (Negate, (Const (IntLiteral 10))))))))
      ] |}];
  test_program {|if f x then g 10 else(~-10);;|};
  [%expect
    {|
    [(EvalItem
        (If ((Apply ((Variable "f"), [(Variable "x")])),
           (Apply ((Variable "g"), [(Const (IntLiteral 10))])),
           (Some (Unary (Negate, (Const (IntLiteral 10))))))))
      ] |}];
  test_program {|1+(f x y) - (g x-y);;|};
  [%expect
    {|
    [(EvalItem
        (Binary (
           (Binary ((Const (IntLiteral 1)), Add,
              (Apply ((Variable "f"), [(Variable "x"); (Variable "y")])))),
           Subtract,
           (Apply ((Variable "g"),
              [(Variable "x"); (Unary (Negate, (Variable "y")))]))
           )))
      ] |}];
  test_program {|(1,);;|};
  [%expect
    {| ParseError(line=1 pos=3): Not found expression after tuple separator: ',' |}];
  test_program {|(1;);;|};
  [%expect {| [(EvalItem (Const (IntLiteral 1)))] |}];
  test_program {|((1,2);(3,4));;|};
  [%expect
    {|
    [(EvalItem
        (ExpressionBlock
           [(Tuple [(Const (IntLiteral 1)); (Const (IntLiteral 2))]);
             (Tuple [(Const (IntLiteral 3)); (Const (IntLiteral 4))])]))
      ] |}];
  test_program {|   (   1   ,   (    2    ;   3 )   ,  4    )   ;;|};
  [%expect
    {|
    [(EvalItem
        (Tuple
           [(Const (IntLiteral 1));
             (ExpressionBlock [(Const (IntLiteral 2)); (Const (IntLiteral 3))]);
             (Const (IntLiteral 4))]))
      ] |}];
  test_program {| 1  ,  2  ;  3  ,  4  ;;|};
  [%expect
    {|
    [(EvalItem
        (ExpressionBlock
           [(Tuple [(Const (IntLiteral 1)); (Const (IntLiteral 2))]);
             (Tuple [(Const (IntLiteral 3)); (Const (IntLiteral 4))])]))
      ] |}];
  test_program {|(1,2);3,4;;|};
  [%expect
    {|
    [(EvalItem
        (ExpressionBlock
           [(Tuple [(Const (IntLiteral 1)); (Const (IntLiteral 2))]);
             (Tuple [(Const (IntLiteral 3)); (Const (IntLiteral 4))])]))
      ] |}];
  test_program {|(let f x = -x in f) 10;;|};
  [%expect
    {|
    [(EvalItem
        (Apply (
           (Define (
              (Nonrecursive,
               [((PVar "f"),
                 (Lambda ([(PVar "x")], (Unary (Negate, (Variable "x"))))))]),
              (Variable "f"))),
           [(Const (IntLiteral 10))])))
      ] |}];
  test_program {|(let f x = -x in f 10);;|};
  [%expect
    {|
    [(EvalItem
        (Define (
           (Nonrecursive,
            [((PVar "f"),
              (Lambda ([(PVar "x")], (Unary (Negate, (Variable "x"))))))]),
           (Apply ((Variable "f"), [(Const (IntLiteral 10))])))))
      ] |}];
  test_program
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
  test_program
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
  test_program
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
      (EvalItem (Tuple [(Variable "t1"); (Variable "t2")]))] |}];
  test_program
    {|
   let t1 = 1,2,3, f x;;
   let t2 = print_endline (f -y); (not x) && g (x = y);;
   t1, t2;;
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
      (EvalItem (Tuple [(Variable "t1"); (Variable "t2")]))] |}];
  test_program {|
   let f = fun x -> fun y -> x / (y - 2);;
   f ~-x ~-(f 10 30);;
   |};
  [%expect
    {|
    [(DefineItem
        (Nonrecursive,
         [((PVar "f"),
           (Lambda ([(PVar "x")],
              (Lambda ([(PVar "y")],
                 (Binary ((Variable "x"), Division,
                    (Binary ((Variable "y"), Subtract, (Const (IntLiteral 2))))))
                 ))
              )))
           ]));
      (EvalItem
         (Apply ((Variable "f"),
            [(Unary (Negate, (Variable "x")));
              (Unary (Negate,
                 (Apply ((Variable "f"),
                    [(Const (IntLiteral 10)); (Const (IntLiteral 30))]))
                 ))
              ]
            )))
      ] |}]
;;
