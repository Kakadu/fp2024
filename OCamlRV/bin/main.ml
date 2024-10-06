(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast

(* Factorial example *)
(* let rec fact n = if n <= 1 then 1 else n * fact (n - 1);; *)

let () =
  let fact_ast : structure =
    [ SValue
        ( Rec
        , [ ( PLiteral (StringLiteral "fact")
            , ExprFun
                ( PLiteral (StringLiteral "n")
                , []
                , ExprIf
                    ( ExprBinOperation (Lte, ExprVariable "n", ExprLiteral (IntLiteral 1))
                    , ExprLiteral (IntLiteral 1)
                    , Some
                        (ExprBinOperation
                           ( Mul
                           , ExprVariable "n"
                           , ExprApply
                               ( ExprVariable "fact"
                               , ExprBinOperation
                                   (Sub, ExprVariable "n", ExprLiteral (IntLiteral 1)) )
                           )) ) ) )
          ] )
    ]
  in
  print_endline (show_structure fact_ast)
;;
