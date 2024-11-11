(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlBR.Qcheck

let () =
  print_endline "Testing manual generator.";
  let _ : int = run_manual () in
  ()
;;
