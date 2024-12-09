
type binder = int

type typ =
  | Primitive of string
  | Type_var of binder
  | Arrow of typ * typ
  | Type_list of typ
  | Type_tuple of typ * typ * typ list
  | TOption of typ

val arrow_of_types: typ list -> typ -> typ

module VarSet : sig
    include module type of Stdlib.Set.Make (Int)
  
    val pp: Format.formatter -> t -> unit
end

type binder_set = VarSet.t
type scheme = S of binder_set * typ [@@deriving show { with_path = false }]
val int_typ: typ
val bool_typ: typ
val string_typ: typ
val unit_typ: typ