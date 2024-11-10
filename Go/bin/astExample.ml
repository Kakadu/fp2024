(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

let () =
  let factorial_ast : func_decl =
    ( "factorial"
    , (* function identificator *)
      { args = [ "n", Type_int ] (* arguments *)
      ; (* return types *)
        returns = Some (Only_types [ Type_int ])
      ; (* function body *)
        body =
          [ Stmt_if
              (* if statement *)
              { (* initialization *)
                init = None
              ; (* condition *)
                cond = Expr_bin_oper (Bin_equal, Expr_ident "n", Expr_const (Const_int 0))
              ; (* "if" body *)
                if_body = [ Stmt_return [ Expr_const (Const_int 1) ] ]
              ; (* "else" body *)
                else_body =
                  Some
                    (Stmt_block
                       [ Stmt_return
                           [ Expr_bin_oper
                               ( Bin_multiply
                               , Expr_ident "n"
                               , Expr_call
                                   ( Expr_ident "factorial"
                                   , [ Expr_bin_oper
                                         ( Bin_subtract
                                         , Expr_ident "n"
                                         , Expr_const (Const_int 1) )
                                     ] ) )
                           ]
                       ])
              }
          ]
      } )
  in
  print_endline (show_func_decl factorial_ast)
;;
