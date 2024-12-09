(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml.Qchecker

let () =
  print_endline "Testing manual generator.";
  let _ : int = QChecker.run_manual () in
  ()
;;
