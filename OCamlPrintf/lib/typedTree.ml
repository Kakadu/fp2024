(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSet = struct
  include Stdlib.Set.Make (String)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%s; ") s;
    Format.fprintf ppf "]"
  ;;
end

type scheme = Scheme of VarSet.t * Ast.core_type [@@deriving show { with_path = false }]

let var_type x = Ast.Type_name x
let tuple_type first second list = Ast.Type_tuple (first, second, list)
let arrow_type l r = Ast.Type_arrow (l, r)
let ( @-> ) = arrow_type

let is_type_arrow = function
  | Ast.Type_arrow _ -> true
  | _ -> false
;;

let is_type_list = function
  | Ast.Type_list _ -> true
  | _ -> false
;;
