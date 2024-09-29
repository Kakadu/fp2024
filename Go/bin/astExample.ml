(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

let () =
  let factorial_ast : func_decl =
    ( "factorial" (* function identificator *)
    , Some [ "n", Some Type_int ] (* arguments *)
    , Some [ Type_int ] (* return type *)
    , Stmt_block
        (* body *)
        [ Stmt_if
            (* if statement *)
            ( None (* initialization *)
            , (* condition *)
              Expr_bin_oper (Bin_equal (Expr_ident "n", Expr_const (Const_int 1)))
            , (* if body *)
              Stmt_return (Some (Expr_const (Const_int 1)))
            , (* else body *)
              Some
                (Stmt_return
                   (Some
                      (Expr_bin_oper
                         (Bin_multiply
                            ( Expr_ident "n"
                            , Expr_call
                                (* function call *)
                                ( Expr_ident "factorial" (* function identificator *)
                                , (* function arguments *)
                                  Some
                                    [ Expr_bin_oper
                                        (Bin_subtract
                                           (Expr_ident "n", Expr_const (Const_int 1)))
                                    ] ) ))))) )
        ] )
  in
  print_endline (show_func_decl factorial_ast)
;;
