open Ast.TypeExpr

type binder = int [@@deriving show { with_path = false }]

module VarSet : sig
  include Set.S with type elt = String.t

  val pp : Format.formatter -> t -> unit
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type scheme = Forall of binder_set * t [@@deriving show { with_path = false }]

val pprint_type_tuple : Format.formatter -> t list -> unit
val pprint_type : Format.formatter -> t -> unit
val pprint_type_list_with_parens : Format.formatter -> t list -> unit
val pprint_type_with_parens_if_tuple : Format.formatter -> t -> unit

type error =
  [ `Occurs_check of string * t
  | `Unification_failed of t * t
  | `Wrong_exp
  | `Wrong_type
  | `Wrong_Const
  | `Wrong_stritem
  | `Unbound_adt_type of string
  | `Unbound_variable of string
  | `Pattern_matching_failed
  | `Arity_mismatch
  | `Undeclared_type of string
  | `Not_supported
  | `Wrong_rec
  ]

val pp_inf_err : Format.formatter -> error -> unit
