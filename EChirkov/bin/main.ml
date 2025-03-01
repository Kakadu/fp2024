(** Copyright 2024, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Ast
open Angstrom

(* let r = parse "let x = 23" *)

let r = parse_string ~consume:All p_expression "[   asd;23   ; asdasd]"

let () =
  match r with
  | Ok s -> print_endline (show_expression s)
  | Error e -> print_endline e
;;
