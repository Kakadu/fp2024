(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s = iter (Format.fprintf ppf "t%d. ") s
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | Ty_prim of string
  | Ty_maybe of ty
  | Ty_var of binder
  | Ty_arrow of ty * ty
  | Ty_list of ty
  | Ty_tuple of ty * ty * ty list
  | Ty_tree of ty
  | Ty_ord of binder
  | Ty_enum of binder
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]
