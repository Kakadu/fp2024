(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Ast

let () =
  (*
     let rec factorial n = if (n > 1) then n * factorial(n-1) else 1;;
     factorial 5;;
  *)
  let factorial_ast : program =
    [ DefineItem
        ( Recursive
        , [ ( PVar "factorial"
            , Lambda
                ( [ PVar "n" ]
                , If
                    ( Binary (Variable "n", Gt, Const (IntLiteral 1))
                    , Binary (Variable "n", Multiply, Const (IntLiteral 1))
                    , Some (Const (IntLiteral 1)) ) ) )
          ] )
    ; EvalItem (Apply (Variable "factorial", [ Const (IntLiteral 5) ]))
    ]
  in
  print_endline (show_program factorial_ast)
;;
