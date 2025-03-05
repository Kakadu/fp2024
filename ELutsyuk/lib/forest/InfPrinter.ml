(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open TypesTree
open Format

let rec pp_typ ppf = function
  | TypConst TInt -> fprintf ppf "int"
  | TypConst TBool -> fprintf ppf "bool"
  | TypConst TStr -> fprintf ppf "string"
  | TypConst TUnit -> fprintf ppf "unit"
  | TypVar ty -> fprintf ppf "'%d" ty
  | TypArrow (ty1, ty2) ->
    (match ty1 with
     | TypArrow _ -> fprintf ppf "(%a) -> %a" pp_typ ty1 pp_typ ty2
     | _ -> fprintf ppf "%a -> %a" pp_typ ty1 pp_typ ty2)
  | TypList ty -> fprintf ppf "%a list" pp_typ ty
  | TypTuple ty ->
    fprintf ppf "(%a)" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") pp_typ) ty
  | TypOption ty -> fprintf ppf "%a option" pp_typ ty
;;

let pp_error ppf = function
  | OccursCheckFailed (id, ty) ->
    fprintf ppf "Occurs check failed: variable '%d appears in %a." id pp_typ ty
  | UnificationFailed (t1, t2) ->
    fprintf ppf "Unification failed: cannot unify %a and %a" pp_typ t1 pp_typ t2
  | UnboundVariable name -> fprintf ppf "Unbound variable: %s" name
  | InvalidRecursivePattern -> fprintf ppf "Invalid recursive pattern"
;;
