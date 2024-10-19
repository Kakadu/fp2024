(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Ast
open Ocamladt_lib.Parser

(* let () =
  let factorial_ast : program =
    [ Str_value
        ( Recursive
        , [ { pat = Pat_var "fact"
            ; expr =
                Exp_fun
                  ( Pat_var "n"
                  , []
                  , Exp_if
                      ( Exp_apply
                          ( Exp_ident "="
                          , Exp_tuple (Exp_ident "n", Exp_constant (Const_integer 0), []) )
                      , Exp_constant (Const_integer 1)
                      , Some
                          (Exp_apply
                             ( Exp_apply
                                 ( Exp_ident "*"
                                 , Exp_tuple
                                     ( Exp_ident "n"
                                     , Exp_apply
                                         ( Exp_ident "fact"
                                         , Exp_apply
                                             ( Exp_ident "-"
                                             , Exp_tuple
                                                 ( Exp_ident "n"
                                                 , Exp_constant (Const_integer 1)
                                                 , [] ) ) )
                                     , [] ) )
                             , Exp_constant (Const_integer 1) )) ) )
            }
          ] )
    ]
  in
  print_endline (show_program factorial_ast) *)

  (* let () = print_endline( ((parse_fact "let rec fact n = if n = 0 then 1 else n * fact (n - 1);;") |> fun _-> "hhh")) *)
