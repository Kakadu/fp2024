(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*
   How to run?
   dune exec interpreter
*)

open OCamlRV_lib.Interpreter

let () = test_interpret "((1 + 1) * 7) / 2;;"
