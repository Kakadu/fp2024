(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Ast
open Ocaml_printf_lib.Parser

let () =
  let fact_ast : structure =
    [ Struct_value
        ( Recursive
        , [ { pat = Pat_var "factorial"
            ; exp =
                Exp_fun
                  ( [ Pat_var "n" ]
                  , Exp_ifthenelse
                      ( Exp_apply
                          ( Exp_ident "<="
                          , [ Exp_ident "n"; Exp_constant (Const_integer 1) ] )
                      , Exp_constant (Const_integer 1)
                      , Some
                          (Exp_apply
                             ( Exp_ident "*"
                             , [ Exp_ident "n"
                               ; Exp_apply
                                   ( Exp_ident "factorial"
                                   , [ Exp_apply
                                         ( Exp_ident "-"
                                         , [ Exp_ident "n"
                                           ; Exp_constant (Const_integer 1)
                                           ] )
                                     ] )
                               ] )) ) )
            }
          ] )
    ]
  in
  print_endline (show_structure fact_ast)
;;

let () =
  let parser : structure =
    parse "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)"
  in
  print_endline (show_structure parser)
;;
