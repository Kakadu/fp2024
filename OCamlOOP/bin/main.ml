(** Copyright 2024-2025, Sultanov Muhammet and Kudrya Alexander *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_oop_lib.Ast

let () =
  let fact_ast : structure =
    [ Str_value
        ( Rec
        , [ { pat = Pat_var "factorial"
            ; exp =
                Exp_function
                  ( [ Pat_var "n" ]
                  , Exp_ifthenelse
                      ( Exp_apply (Exp_ident "<=", [ Exp_ident "n"; Exp_constant (Int 1) ])
                      , Exp_constant (Int 1)
                      , Some
                          (Exp_apply
                             ( Exp_ident "*"
                             , [ Exp_ident "n"
                               ; Exp_apply
                                   ( Exp_ident "factorial"
                                   , [ Exp_apply
                                         ( Exp_ident "-"
                                         , [ Exp_ident "n"; Exp_constant (Int 1) ] )
                                     ] )
                               ] )) ) )
            }
          ] )
    ]
  in
  print_endline (show_structure fact_ast)
;;
