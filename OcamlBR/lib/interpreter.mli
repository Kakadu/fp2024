(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Interpreter : sig
  val eval_structure : Ast.structure -> (Values.environment, Values.error) result
end

val pp_env
  :  (string, Typedtree.scheme, Base.String.comparator_witness) Base.Map.t
  -> (string, Values.value, Base.String.comparator_witness) Base.Map.t
  -> unit

val print_key : string -> bool
