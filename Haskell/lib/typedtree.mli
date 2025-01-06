(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

type binder = int [@@deriving show { with_path = false }]

module VarSet : sig
  type t

  val add : int -> t -> t
  val empty : t
  val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a
  val diff : t -> t -> t
  val union : t -> t -> t
  val pp : Format.formatter -> t -> unit
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

(** hierarchy: https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1270011 *)
type ty =
  | Ty_prim of string
  | Ty_maybe of ty
  | Ty_var of binder
  | Ty_arrow of ty * ty
  | Ty_list of ty
  | Ty_tuple of ty * ty * ty list
  | Ty_tree of ty
  | Ty_ord of binder (** i.e. [Ord a]; e.g. [(>) :: Ord a -> Ord a -> Bool] *)
  | Ty_enum of binder (** i.e. [Enum a]; e.g. [ (\x -> [x..]) :: Enum a -> [Enum a]] *)
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]
