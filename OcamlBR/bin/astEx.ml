(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
(*
   open OCamlBR.Ast

   let () =
   let fact_ast : expr =
   Elet
   ( Recursive
   , "factorial"
   , Evar "n"
   , Eif_then_else
   ( Ebin_op (Eq, Evar "n", Econst (Int 0))
   , Econst (Int 1)
   , Some
   (Ebin_op
   ( Mult
   , Evar "n"
   , Efun_application
   (Evar "factorial", Ebin_op (Sub, Evar "n", Econst (Int 1))) )) ) )
   in
   print_endline (show_expr fact_ast)
   ;;
*)
