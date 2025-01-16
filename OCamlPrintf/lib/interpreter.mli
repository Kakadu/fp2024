(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Division_by_zero
  | `No_variable of string
  | `Not_implemented
  | `Type_error
  ]

val pp_error : Format.formatter -> error -> unit

type value =
  | Val_integer of int
  | Val_char of char
  | Val_string of string
  | Val_tuple of value list
  | Val_construct of string * value option
  | Val_fun of string option * Ast.pattern list * Ast.Expression.t * env

and env = (string, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit

module Inter : sig
  val eval_structure
    :  Ast.structure_item list
    -> ( (string, value, Base.String.comparator_witness) Base.Map.t * value list
         , error )
         result
end
