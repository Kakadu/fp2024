(** Copyright 2024, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Ast
open Angstrom

(* let r = parse "let x = 23" *)

let r = parse_string ~consume:All p_program "let [a] = [6]"

let () =
  match r with
  | Ok s -> print_endline (show_program s)
  | Error e -> print_endline e
;;
