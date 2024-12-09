(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module TestQCheckManual : sig
  val gen_structure : Ast.structure QCheck.Gen.t
end

val run_gen
  :  ?show_passed:bool
  -> ?show_shrinker:bool
  -> ?count:int
  -> string
  -> Ast.structure QCheck.Gen.t
  -> int
