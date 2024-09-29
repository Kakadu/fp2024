(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast

(* Factorial example *)
(* let rec fact n = if n <= 1 then 1 else n * fact (n - 1);; *)

let () =
  let fact_ast : program =
    [ ExprDefinition
        ( Global
        , Rec
        , "fact"
        , [ "n" ]
        , [ ExprIf
              ( ExprBinOperation (Lte, ExprVariable "n", ExprLiteral (IntLiteral 1))
              , ExprLiteral (IntLiteral 1)
              , ExprBinOperation
                  ( Mul
                  , ExprVariable "n"
                  , ExprApply
                      ( ExprFun "fact"
                      , ExprBinOperation
                          (Sub, ExprVariable "n", ExprLiteral (IntLiteral 1)) ) ) )
          ] )
    ]
  in
  print_endline (show_program fact_ast)
;;
