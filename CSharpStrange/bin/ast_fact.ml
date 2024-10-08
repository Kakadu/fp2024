(** Copyright 2024, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Ast

let () =
  let fact_ast =
    Program
      (Class
         ( [ Public ]
         , Id "Program"
         , [ Method ([ Public; Static ], TypeBase TypeVoid, Id "Main", [], StmtsBlock [])
           ; Method
               ( [ Public ]
               , TypeBase TypeInt
               , Id "Factorial"
               , [ TypeBase TypeInt, "n" ]
               , StmtsBlock
                   [ If
                       ( BinOp (Equal, IdExpr (Id "n"), Value (ValInt 0))
                       , Return (Value (ValInt 1))
                       , Some
                           (Return
                              (BinOp
                                 ( Mul
                                 , IdExpr (Id "n")
                                 , FuncCall
                                     ( Id "Factorial"
                                     , [ BinOp (Sub, IdExpr (Id "n"), Value (ValInt 1)) ]
                                     ) ))) )
                   ] )
           ] ))
  in
  print_endline (show_program fact_ast)
;;
