(** Copyright 2024-2025, Azamat Ishbaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EIshbaev_lib.Ast

let () =
  let fact : structure =
    [ ExprLet
        ( Rec
        , [ ( PatVar "fact"
            , ExprFunc
                ( "x"
                , ExprCond
                    ( ExprBinop (Lesq, ExprVar "x", ExprConst (ConstInt 1))
                    , ExprConst (ConstInt 1)
                    , ExprBinop
                        ( Mul
                        , ExprVar "x"
                        , ExprApp
                            ( ExprVar "fact"
                            , ExprBinop (Sub, ExprVar "x", ExprConst (ConstInt 1)) ) ) )
                ) )
          ]
        , ExprVar "n" )
    ]
  in
  print_endline (show_structure fact)
;;
