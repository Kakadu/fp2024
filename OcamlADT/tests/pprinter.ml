(** Copyright 2024, Rodion Suvorov, Mikhail Gavrilenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Ast
open Ocamladt_lib.Pprinter
open Format

let example_program =
  let open Expression in
  let expr =
    Exp_apply
      ( Exp_ident "*"
      , Exp_tuple
          ( Exp_apply
              ( Exp_ident "-"
              , Exp_tuple
                  (Exp_constant (Const_integer 5), Exp_constant (Const_integer 1), []) )
          , Exp_constant (Const_integer 5)
          , [] ) )
  in
  [ Structure.Str_eval expr ]
;;

let () =
  let printed_program = asprintf "%a" pprint_program example_program in
  printf "Generated program:\n%s\n" printed_program
;;

(* to add. *)
