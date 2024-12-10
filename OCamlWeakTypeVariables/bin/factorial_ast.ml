[@@@ocaml.text "/*"]

(** Copyright 2024, Damir Yunosov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) *)
open Lib.Ast

let () =
  let fact : expr =
    Binding
      ( "factorial"
      , Recursive
      , [ "n" ]
      , Op_tern
          ( IfThenElse
          , Op_bin (Equal, Variable "n", Const (Int 0))
          , Const (Int 1)
          , Op_bin
              ( Mul
              , Variable "n"
              , Function ("factorial", [ Op_bin (Minus, Variable "n", Const (Int 1)) ]) )
          ) )
  in
  print_endline (show_expr fact)
;;
