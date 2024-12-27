(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving show { with_path = false }, qcheck]

type typ =
  | Primitive of (string[@gen gen_primitive])
  | Type_var of binder
  | Arrow of typ * typ
  | Type_list of typ
  | Type_tuple of typ * typ * typ list
  | TOption of typ

let gen_typ_primitive =
  QCheck.Gen.(oneofl [ "string"; "int"; "unit"; "bool" ] >|= fun t -> Primitive t)
;;

let arrow_of_types first_types last_type =
  let open Base in
  List.fold_right first_types ~init:last_type ~f:(fun left right -> Arrow (left, right))
;;

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp fmt s =
    Format.fprintf fmt "[ ";
    iter (Format.fprintf fmt "%d; ") s;
    Format.fprintf fmt "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

(* binder_set here -- list of all type vars in context (?) *)
type scheme = Scheme of binder_set * typ

let int_typ = Primitive "int"
let bool_typ = Primitive "bool"
let string_typ = Primitive "string"
let unit_typ = Primitive "unit"
