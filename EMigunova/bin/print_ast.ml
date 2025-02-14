(** Copyright 2024, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(**    let rec factorial n  =  if n <= 1 then 1 else n * factorial (n-1)     *)

open EMigunova_lib.Ast

let () =
  let ast_factorial : structure =
    Structure_items
      [ Let_binding
          ( Recursive
          , Let_fun ("fuctorial", [ Pattern_var "n" ])
          , Expr_if_then_else
              ( Expr_binary_op (LessEqual, Expr_var "n", Expr_const (Const_int 1))
              , Expr_const (Const_int 1)
              , Expr_binary_op
                  ( Mul
                  , Expr_var "n"
                  , Expr_application
                      ( "factorial"
                      , [ Expr_binary_op (Sub, Expr_var "n", Expr_const (Const_int 1)) ]
                      ) ) ) )
      ]
  in
  print_endline (show_structure ast_factorial)
;;
