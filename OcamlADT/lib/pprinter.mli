(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val pprint_constant : Format.formatter -> Constant.t -> unit
val pprint_type : Format.formatter -> TypeExpr.t -> unit
val pprint_pattern : Format.formatter -> Pattern.t -> unit
val pprint_rec : Format.formatter -> Expression.rec_flag -> unit
val pprint_expression : Format.formatter -> int -> Expression.t -> unit

val pprint_value_binding
  :  Format.formatter
  -> int
  -> Expression.t Expression.value_binding
  -> unit

val pprint_case : Format.formatter -> int -> Expression.t Expression.case -> unit
val pprint_structure_item : Format.formatter -> int -> Structure.structure_item -> unit
val pprint_program : Format.formatter -> Structure.structure_item list -> unit
val pp : (Format.formatter -> 'a -> unit) -> 'a Angstrom.t -> string -> unit
