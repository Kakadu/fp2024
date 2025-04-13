(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Parser
open Interpreter.Interpret
open Interpreter.IntAuxilary

let test_interpret str =
  let open Stdlib.Format in
  match parse str with
  | Ok parsed ->
    (match Res.run (interpret parsed) with
     | Ok _ -> ()
     | Error err -> printf "Interpreter error: %a\n" Forest.ValuesTree.pp_error err)
  | Error err -> printf "Parsing error: %s\n" err
;;
