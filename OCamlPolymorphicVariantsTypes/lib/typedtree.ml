(** Copyright 2024-2027, Ilia Suponev, Dmitri Chirkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp fmt s =
    Format.fprintf fmt "[ ";
    iter (Format.fprintf fmt "%d; ") s;
    Format.fprintf fmt "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TPrim of string
  | TVar of binder
  | TArrow of ty * ty
  | TTuple of ty * ty * ty list
  | TList of ty
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let ty_int = TPrim "int"
let ty_bool = TPrim "bool"
let ty_string = TPrim "string"
let ty_unit = TPrim "unit"
let ty_arrow l r = TArrow (l, r)
let ty_var v = TVar v
let ty_tuple (t1, t2, tl) = TTuple (t1, t2, tl)
let ty_list t = TList t

let rec pp_ty fmt = function
  | TPrim s -> Format.fprintf fmt "%s" s
  | TVar v -> Format.fprintf fmt "'%d" v
  | TArrow (l, r) -> Format.fprintf fmt "(%a -> %a)" pp_ty l pp_ty r
  | TTuple (t1, t2, tl) ->
    Format.fprintf
      fmt
      "(%a * %a%a)"
      pp_ty
      t1
      pp_ty
      t2
      (Format.pp_print_list ~pp_sep:(fun _ _ -> Format.fprintf fmt " * ") pp_ty)
      tl
  | TList t ->
    Format.fprintf
      fmt
      "%a list"
      (fun ppf ty ->
        match ty with
        | TArrow _ -> Format.fprintf ppf "(%a)" pp_ty ty
        | _ -> Format.fprintf ppf "%a" pp_ty ty)
      t
;;
