(** Copyright 2024-2027, Ilia Suponev, Dmitri Chirkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp fmt s =
    Format.fprintf fmt "[ ";
    iter (Format.fprintf fmt "%d; ") s;
    Format.fprintf fmt "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]
type scheme = S of binder_set * core_type [@@deriving show { with_path = false }]

let ty_int = TypeIdentifier "int"
let ty_bool = TypeIdentifier "bool"
let ty_unit = TypeIdentifier "unit"
let ty_arrow (l, r) = ArrowType (l, r)
let ty_var v = TypeVariable v
let ty_tuple (t1, t2, tl) = TupleType (t1, t2, tl)
let ty_constructor (n, t) = TypeConstructor (n, t)

let rec pp_ty fmt = function
  | TypeIdentifier s -> Format.fprintf fmt "%s" s
  | TypeVariable v -> Format.fprintf fmt "'%d" v
  | ArrowType (l, r) -> Format.fprintf fmt "(%a -> %a)" pp_ty l pp_ty r
  | TupleType (t1, t2, tl) ->
    Format.fprintf
      fmt
      "(%a * %a%a)"
      pp_ty
      t1
      pp_ty
      t2
      (Format.pp_print_list ~pp_sep:(fun _ _ -> Format.fprintf fmt " * ") pp_ty)
      tl
  | TypeConstructor (n, t) ->
    Format.fprintf
      fmt
      "%a %s"
      (fun ppf ty ->
        match ty with
        | ArrowType _ -> Format.fprintf ppf "(%a)" pp_ty ty
        | _ -> Format.fprintf ppf "%a" pp_ty ty)
      t
      n
;;
