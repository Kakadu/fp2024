(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Stdlib.Format

type var = int [@@deriving show { with_path = false }]

type constant =
  | TInt
  | TStr
  | TBool
  | TUnit
[@@deriving show { with_path = false }]

type typ =
  | TypConst of constant
  | TypVar of var
  | TypArrow of typ * typ
  | TypTuple of typ list
  | TypList of typ
  | TypOption of typ

type error =
  | OccursCheckFailed of int * typ
  | UnificationFailed of typ * typ
  | UnboundVariable of string
  | InvalidRecursivePattern
  | UnexpectedFuncType of typ
  | IllegalLHS
  | IllegalRHS

(* Type constructors *)
let int_typ = TypConst TInt
let bool_typ = TypConst TBool
let string_typ = TypConst TStr
let unit_typ = TypConst TUnit
let constant_typ ty = TypConst ty
let var_typ name = TypVar name
let arrow_typ ty1 ty2 = TypArrow (ty1, ty2)
let tup_typ ty = TypTuple ty
let list_typ ty = TypList ty
let option_typ ty = TypOption ty

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
    fprintf
      ppf
      "occurs check failed: variable '%d' cannot appear in its own type %a"
      id
      pp_typ
      ty
  | UnificationFailed (t1, t2) ->
    fprintf
      ppf
      "type unification failed: cannot unify types %a and %a"
      pp_typ
      t1
      pp_typ
      t2
  | UnboundVariable name ->
    fprintf ppf "unbound variable: '%s' is not defined in the current scope" name
  | InvalidRecursivePattern ->
    fprintf ppf "invalid recursive pattern: recursive patterns can only be variables"
  | UnexpectedFuncType ty ->
    fprintf ppf "unexpected function type: expected a function type but got %a" pp_typ ty
  | IllegalLHS ->
    fprintf
      ppf
      "illegal left-hand side: only variables can appear on the left-hand side of a \
       'let' binding"
  | IllegalRHS ->
    fprintf
      ppf
      "illegal right-hand side: the right-hand side of a 'let' binding must be an \
       expression, not a pattern"
;;
