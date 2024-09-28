(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Ast

let () =
  let factorial_ast : program =
    [ Str_value
        ( Recursive
        , [ { pat = Pat_var "fact"
            ; expr =
                Exp_fun
                  ( [ Pat_var "n" ]
                  , Exp_if
                      ( Exp_binop (Eq, Exp_var "n", Exp_constant (Const_integer 0))
                      , Exp_constant (Const_integer 1)
                      , Some
                          (Exp_binop
                             ( Mul
                             , Exp_var "n"
                             , Exp_apply
                                 ( Exp_var "fact"
                                 , Exp_binop
                                     (Sub, Exp_var "n", Exp_constant (Const_integer 1)) )
                             )) ) )
            }
          ] )
    ]
  in
  print_endline (show_program factorial_ast)
;;