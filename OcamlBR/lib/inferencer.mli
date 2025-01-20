(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module TypeEnv : sig
  type t

  val pp : Format.formatter -> t -> unit
end

module Infer : sig
  val w : Ast.expr -> (Typedtree.ty, Typedtree.error) result

  val infer_program
    :  Ast.structure
    -> ( (string, Typedtree.scheme, Base.String.comparator_witness) Base.Map.t
         , Typedtree.error )
         result
end
