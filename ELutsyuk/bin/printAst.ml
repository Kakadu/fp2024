(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Ast
(* let rec fact n =
   if n <= 1 then 1 else n * fact (n-1)
   ;; *)

let () =
  let fact : program =
    [ Binding
        { is_rec = Rec
        ; pat = PatVar "fact"
        ; expr =
            Fun
              ( PatVar "n"
              , Branch
                  ( BinOp (Le, Var "n", Const (Int 1))
                  , Const (Int 1)
                  , BinOp
                      (Mul, Var "n", App (Var "fact", BinOp (Sub, Var "n", Const (Int 1))))
                  ) )
        }
    ]
  in
  print_endline (show_program fact)
;;
