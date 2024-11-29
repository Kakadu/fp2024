[@@@ocaml.text "/*"]

(** Copyright 2024, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) *)
open Lib.Ast

let () =
  let fact : structure_item =
    Pstr_value
      ( Recursive
      , Ppat_var "factorial"
      , Pexpr_fun
          ( Ppat_var "n"
          , Pexpr_ifThenElse
              ( Pexpr_apply
                  ( Pexpr_ident (Id "=")
                  , [ Pexpr_ident (Id "n"); Pexpr_const (Pconst_int 5) ] )
              , Pexpr_const (Pconst_int 1)
              , Some
                  (Pexpr_apply
                     ( Pexpr_ident (Id "*")
                     , [ Pexpr_ident (Id "n")
                       ; Pexpr_apply
                           ( Pexpr_ident (Id "factorial")
                           , [ Pexpr_apply
                                 ( Pexpr_ident (Id "-")
                                 , [ Pexpr_ident (Id "n"); Pexpr_const (Pconst_int 1) ] )
                             ] )
                       ] )) ) ) )
  in
  print_endline (show_structure_item fact)
;;
