(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Qchecker

let () =
  let _ : int = run_gen ~show_passed:false ~show_shrinker:false ~count:10 in
  ()
;;
