(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Ast
open C_sharp_strange_lib.Parser
open Angstrom
open Printf

let fact_ast =
  Program
    (Class
       ( [ MPublic ]
       , Id "Program"
       , [ Method ([ MPublic; MStatic ], TypeVoid, Id "Main", Params [], SBlock [])
         ; Method
             ( [ MPublic ]
             , TypeBase TypeInt
             , Id "Factorial"
             , Params [ Var (TypeVar (TypeBase TypeInt), Id "n") ]
             , SBlock
                 [ SIf
                     ( EBinOp (OpEqual, EId (Id "n"), EValue (ValInt 0))
                     , SReturn (Some (EValue (ValInt 1)))
                     , Some
                         (SReturn
                            (Some
                               (EBinOp
                                  ( OpMul
                                  , EId (Id "n")
                                  , EFuncCall
                                      ( EId (Id "Factorial")
                                      , [ EBinOp (OpSub, EId (Id "n"), EValue (ValInt 1))
                                        ] ) )))) )
                 ] )
         ] ))
;;

let () = print_endline (show_program fact_ast) (* AST print test *)
