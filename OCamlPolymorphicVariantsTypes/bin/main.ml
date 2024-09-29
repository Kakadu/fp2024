(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Ast

let () =
  let fact_ast : program =
    ( "let rec factorial n = if (n > 1) then n * factorial(n-1) else 1"
    , [ DefinitionStatement
          ( Recursive
          , Global
          , "factorial"
          , [ "n" ]
          , [ EvalStatement
                (IfExpression
                   ( [ EvalStatement
                         (BinaryExpression (Variable "n", Gt, Const (IntLiteral 1)))
                     ]
                   , [ EvalStatement
                         (BinaryExpression
                            ( Variable "n"
                            , Multiply
                            , Apply
                                ( Variable "factorial"
                                , [ EvalStatement
                                      (BinaryExpression
                                         (Variable "n", Subtract, Const (IntLiteral 1)))
                                  ] ) ))
                     ]
                   , [ EvalStatement (Const (IntLiteral 1)) ] ))
            ] )
      ] )
  in
  print_endline (show_program fact_ast)
;;
