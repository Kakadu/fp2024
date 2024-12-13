(** Copyright 2024, Rodion Suvorov, Mikhail Gavrilenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib.Ast
open QCheck.Iter

module ShrinkQCheck : sig
  val filter : ('a -> bool) -> 'a t -> 'a t

  val shrink_list1
    :  shrink_head:('a -> 'a t)
    -> shrink_tail:'b QCheck.Shrink.t
    -> 'a * 'b list
    -> ('a * 'b list) t

  val shrink_list2
    :  shrink_first:('a -> 'a t)
    -> shrink_second:('b -> 'b t)
    -> shrink_tail:'c QCheck.Shrink.t
    -> 'a * 'b * 'c list
    -> ('a * 'b * 'c list) t

  val shrink_pattern : Pattern.t QCheck.Shrink.t
  val shrink_expression : Expression.t QCheck.Shrink.t
  val shrink_value_binding : Expression.t Expression.value_binding QCheck.Shrink.t
  val shrink_case : Expression.t Expression.case QCheck.Shrink.t
  val shrink_structure_item : Structure.structure_item -> Structure.structure_item t
  val shrink_structure : Structure.structure_item list QCheck.Shrink.t
end
