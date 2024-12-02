(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Format
open Typedtree

let rec pp_ty fmt ty =
  let rec helper fmt = function
    | Ty_unit -> fprintf fmt "()"
    | Ty_prim s -> pp_print_string fmt s
    | Ty_var b -> fprintf fmt "t%d" b
    | Ty_arrow (ty1, ty2) ->
      (match ty1 with
       | Ty_arrow (_, _) -> fprintf fmt "(%a) -> %a" helper ty1 helper ty2
       | _ -> fprintf fmt "%a -> %a" helper ty1 helper ty2)
    | Ty_list ty -> fprintf fmt "[%a]" helper ty
    | Ty_tuple ty_list ->
      fprintf fmt "(";
      fprintf
        fmt
        "%a"
        (pp_print_list
           ~pp_sep:(fun fmt _ -> fprintf fmt ", ")
           (fun fmt ty -> fprintf fmt "%a" helper ty))
        ty_list;
      fprintf fmt ")"
    | Ty_tree ty -> fprintf fmt "{%a}" helper ty
  in
  helper fmt ty
;;
