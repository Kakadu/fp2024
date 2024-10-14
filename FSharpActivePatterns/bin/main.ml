(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.Ast
open FSharpActivePatterns.PrintAst

let () =
  let factorial =
    Function_def
      ( Rec
      , Some "factorial"
      , [ Variable (Ident "n") ]
      , If_then_else
          ( Bin_expr
              ( Logical_or
              , Bin_expr (Binary_equal, Variable (Ident "n"), Const (Int_lt 0))
              , Bin_expr (Binary_equal, Variable (Ident "n"), Const (Int_lt 1)) )
          , Const (Int_lt 1)
          , Some
              (Bin_expr
                 ( Binary_multiply
                 , Variable (Ident "n")
                 , Function_call
                     ( Variable (Ident "factorial")
                     , [ Bin_expr (Binary_subtract, Variable (Ident "n"), Const (Int_lt 1))
                       ] ) )) ) )
  in
  let program =
    [ Statement (Let (Ident "a", Const (Int_lt 10)))
    ; Expr factorial
    ; Expr (Function_call (Variable (Ident "factorial"), [ Variable (Ident "a") ]))
    ]
  in
  List.iter print_construction program
;;
