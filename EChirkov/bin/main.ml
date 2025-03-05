(** Copyright 2024, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Ast
open Angstrom

(* let r = parse "let x = 23" *)

let r =
  parse_string
    ~consume:All
    p_program
    "let main =\n  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in\n  0\n"
;;

let () =
  match r with
  | Ok s -> print_endline (show_program s)
  | Error e -> print_endline e
;;
