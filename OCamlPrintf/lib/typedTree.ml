(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type ty =
  | TUnit
  | TPrim of string
  | TVar of binder
  | TList of ty
  | TTuple of ty * ty * ty list
  | TArrow of ty * ty
[@@deriving show { with_path = false }]

type scheme = Scheme of VarSet.t * ty [@@deriving show { with_path = false }]

let var_type x = TVar x
let tuple_type first second list = TTuple (first, second, list)
let arrow_type l r = TArrow (l, r)
let ( @-> ) = arrow_type

open Format

let is_type_arrow = function
  | TArrow (_, _) -> true
  | _ -> false
;;

let is_type_list = function
  | TList _ -> true
  | _ -> false
;;

let rec pp_type ppf = function
  | TUnit -> fprintf ppf "_"
  | TPrim str -> fprintf ppf "%s" str
  | TVar binder -> fprintf ppf "'%d" binder
  | TList type' ->
    (match type' with
     | type' when is_type_arrow type' || is_type_list type' ->
       fprintf ppf "(%a) list" pp_type type'
     | type' -> fprintf ppf "%a list" pp_type type')
  | TTuple (first_type, second_type, type_list) ->
    let pp_with_condition_on_arrow type' =
      match type' with
      | type' when is_type_arrow type' -> fprintf ppf "(%a)" pp_type type'
      | _ -> fprintf ppf "%a" pp_type type'
    in
    fprintf ppf "(";
    pp_with_condition_on_arrow first_type;
    List.iter
      (fun type' ->
        fprintf ppf " * ";
        pp_with_condition_on_arrow type')
      (second_type :: type_list);
    fprintf ppf ")"
  | TArrow (first_type, second_type) ->
    (match first_type with
     | first_type when is_type_arrow first_type ->
       fprintf ppf "(%a) -> %a" pp_type first_type pp_type second_type
     | first_type -> fprintf ppf "%a -> %a" pp_type first_type pp_type second_type)
;;
