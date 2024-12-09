(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Ast

val arbitrary : program QCheck.arbitrary
val test_round_trip2 : QCheck2.Test.t
