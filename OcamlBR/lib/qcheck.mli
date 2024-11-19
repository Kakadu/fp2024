(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)
(*
(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open QCheck

(* val run_manual : unit -> int *)
val run_auto : int -> int
val test_expr_generation : Test.t
val test_structure_generation : Test.t
val test_shrinking_expr : Test.t
*)