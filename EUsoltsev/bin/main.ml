(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EUsoltsev_lib.Ast

let () =
  let factorial : program =
    [ ExpLet
        ( true
        , PatVariable "factorial"
        , ExpLambda
            ( [ PatVariable "n" ]
            , ExpBranch
                ( ExpBinOper (LowerThan, ExpIdent "n", ExpConst (ConstInt 2))
                , ExpConst (ConstInt 1)
                , Some
                    (ExpBinOper
                       ( Multiply
                       , ExpIdent "n"
                       , ExpFunction
                           ( ExpIdent "factorial"
                           , ExpBinOper (Minus, ExpIdent "n", ExpConst (ConstInt 1)) ) ))
                ) )
        , None )
    ]
  in
  print_endline (show_program factorial)
;;
