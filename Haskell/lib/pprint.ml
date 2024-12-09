(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Format
open Typedtree

let pp_ty =
  let rec helper fmt = function
    | Ty_maybe ty -> fprintf fmt "Maybe %a" helper ty
    | Ty_prim s -> pp_print_string fmt s
    | Ty_var b -> fprintf fmt "t%d" b
    | Ty_arrow (ty1, ty2) ->
      (match ty1 with
       | Ty_arrow (_, _) -> fprintf fmt "(%a) -> %a" helper ty1 helper ty2
       | _ -> fprintf fmt "%a -> %a" helper ty1 helper ty2)
    | Ty_list ty -> fprintf fmt "[%a]" helper ty
    | Ty_tuple (ty1, ty2, ty_list) ->
      fprintf
        fmt
        "(%a, %a%a)"
        helper
        ty1
        helper
        ty2
        (pp_print_list (fun fmt ty -> fprintf fmt ", %a" helper ty))
        ty_list
    | Ty_tree ty -> fprintf fmt "{%a}" helper ty
    | Ty_ord ty -> fprintf fmt "Ord t%d" ty
    | Ty_enum ty -> fprintf fmt "Enum t%d" ty
  in
  helper
;;

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_ty l pp_ty r
;;
