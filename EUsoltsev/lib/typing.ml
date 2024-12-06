open Format

type binder = int [@@deriving show { with_path = false }]

module VarSet = Stdlib.Set.Make (Int)

type ty =
  | TyVar of binder
  | TyPrim of string
  | TyArrow of ty * ty
  | TyTuple of ty list
  | TyList of ty
  | TyOption of ty
[@@deriving show { with_path = false }]

type scheme = Scheme of VarSet.t * ty

type error =
  | OccursCheck of int * ty
  | NoVariable of string
  | UnificationFailed of ty * ty
  | SeveralBounds of string
  | NotImplemented

let rec pp_ty fmt = function
  | TyPrim x -> fprintf fmt "%s" x
  | TyVar x -> fprintf fmt "%s" @@ Char.escaped (Char.chr (x + 97))
  | TyArrow (l, r) ->
    (match l, r with
     | TyArrow _, _ -> fprintf fmt "(%a) -> %a" pp_ty l pp_ty r
     | _, _ -> fprintf fmt "%a -> %a" pp_ty l pp_ty r)
  | TyTuple elems ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") pp_ty)
      elems
  | TyList ty ->
    (match ty with
     | TyArrow _ | TyTuple _ -> fprintf fmt "(%a) list" pp_ty ty
     | _ -> fprintf fmt "%a list" pp_ty ty)
  | TyOption ty ->
    (match ty with
     | TyArrow _ | TyTuple _ -> fprintf fmt "(%a) option" pp_ty ty
     | _ -> fprintf fmt "%a option" pp_ty ty)
  | _ -> fprintf fmt "not implemented"
;;

let pp_error fmt = function
  | OccursCheck (id, ty) ->
    fprintf
      fmt
      "Error: Occurs check failed. Type variable '%d occurs inside %a."
      id
      pp_ty
      ty
  | NoVariable name -> fprintf fmt "Error: Unbound variable '%s'." name
  | UnificationFailed (ty1, ty2) ->
    fprintf fmt "Error: Failed to unify types: %a and %a." pp_ty ty1 pp_ty ty2
  | SeveralBounds name -> fprintf fmt "Error: Multiple bounds for variable '%s'." name
  | NotImplemented -> fprintf fmt "Error: This feature is not implemented yet."
;;
