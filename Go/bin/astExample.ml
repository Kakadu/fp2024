(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

let () =
  let factorial_ast : func_decl =
    ( "factorial"
    , (* function identificator *)
      { args = [ "n", Type_int ] (* arguments *)
      ; (* return types *)
        return_types = Some (Only_types [ Type_int ])
      ; (* function body *)
        body =
          [ Stmt_if
              (* if statement *)
              ( (* initialization *)
                None
              , (* condition *)
                Expr_bin_oper (Bin_equal, Expr_ident "n", Expr_const (Const_int 0))
              , (* "if" branch *)
                [ Stmt_return (Some (Expr_const (Const_int 1))) ]
              , (* "else" branch *)
                Some
                  [ Stmt_return
                      (Some
                         (Expr_bin_oper
                            ( Bin_multiply
                            , Expr_ident "n"
                            , Expr_call
                                ( Expr_ident "factorial"
                                , [ Expr_bin_oper
                                      ( Bin_subtract
                                      , Expr_ident "n"
                                      , Expr_const (Const_int 1) )
                                  ] ) )))
                  ] )
          ]
      } )
  in
  print_endline (show_func_decl factorial_ast)
;;
