(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Ast

let () =
  (*
     let rec factorial n =if (n > 1) then n * factorial(n-1) else 1;;
     factorial 5;;
  *)
  let factorial_ast : program =
    ( "factorial.ml"
    , [ DefineItem
          ( "factorial"
          , Recursive
          , [ PVar "n" ]
          , EvalStatement
              (If
                 ( EvalStatement (Binary (Variable "n", Gt, Const (IntLiteral 1)))
                 , EvalStatement
                     (Binary
                        ( Variable "n"
                        , Multiply
                        , Apply
                            ( Variable "factorial"
                            , [ EvalStatement
                                  (Binary (Variable "n", Subtract, Const (IntLiteral 1)))
                              ] ) ))
                 , EvalStatement (Const (IntLiteral 1)) )) )
      ; StatementItem
          (EvalStatement
             (Apply (Variable "factorial", [ EvalStatement (Const (IntLiteral 5)) ])))
      ] )
  in
  print_endline (show_program factorial_ast)
;;
