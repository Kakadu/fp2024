(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_lib
open Launcher

let () = run (Stdio.In_channel.input_all stdin)
