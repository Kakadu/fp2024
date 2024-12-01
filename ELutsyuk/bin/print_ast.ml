(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Ast
(* let rec fact n =
   if n <= 1 then 1 else n * fact (n-1)
   ;; *)

let () =
  let fact : program =
    [ Binding
        { is_rec = Rec
        ; pat = PVar "fact"
        ; expr =
            Fun
              ( PVar "n"
              , Branch
                  ( BinaryOp (LtEq, Var "n", Lit (Int 1))
                  , Lit (Int 1)
                  , Some
                      (BinaryOp
                         ( Mult
                         , Var "n"
                         , App (Var "fact", BinaryOp (Sub, Var "n", Lit (Int 1))) )) ) )
        }
    ]
  in
  print_endline (show_program fact)
;;
