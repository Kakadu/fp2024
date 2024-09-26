(** Copyright 2021-2024, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Ast

let () =
  let fact_ast : program =
    ( "factorial_example.ml"
    , [ DefinitionStatement
          ( Recursive
          , Global
          , "factorial"
          , [ "n" ]
          , [ IfStatement
                ( BinaryExpression (Variable "n", Gt, Const (IntLiteral 1))
                , [ EvalStatement
                      (BinaryExpression
                         ( Variable "n"
                         , Multiply
                         , Apply
                             ( "factorial"
                             , [ BinaryExpression
                                   (Variable "n", BinaryMinus, Const (IntLiteral 1))
                               ] ) ))
                  ]
                , [ EvalStatement (Const (IntLiteral 1)) ] )
            ] )
      ] )
  in
  print_endline (show_program fact_ast)
;;
