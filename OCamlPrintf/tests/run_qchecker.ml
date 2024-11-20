(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib.Qchecker

let () =
  let _ : int = TestQCheckManual.run_gen_manual 1 in
  ();
  let _ : int = TestQCheckAuto.run_gen_auto 1 in
  ()
;;
