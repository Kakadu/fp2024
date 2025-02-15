(** Copyright 2024, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open Angstrom

let r = parse_string ~consume:All parse "let x = 23"

let () =
  match r with
  | Ok s -> print_endline s
  | Error e -> print_endline e
;;
