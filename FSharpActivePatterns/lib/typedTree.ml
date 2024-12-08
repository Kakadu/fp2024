(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving show { with_path = false }]

type typ =
  | Primary of string
  | Type_var of binder
  | Arrow of typ * typ
[@@deriving show { with_path = false }]

let arrow_of_types first_types last =
  let open Base in
  List.fold_right first_types ~init:last ~f:(fun left right -> Arrow (left, right))
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
type scheme = S of binder_set * typ [@@deriving show { with_path = false }]

let int_typ = Primary "int"
let bool_typ = Primary "bool"
let string_typ = Primary "string"
let unit_typ = Primary "unit"
