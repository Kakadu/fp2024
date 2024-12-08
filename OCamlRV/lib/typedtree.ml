(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type binder = int

type ty =
  | TPrimitive of string (** int, string, bool, unit *)
  | TVar of binder (** Type variable *)
  | TArrow of ty * ty (** Function type *)
  | TTuple of ty list (** Tuple type *)
  | TList of ty (** List type *)

let arrow l r = TArrow (l, r)
let int_type = TPrimitive "int"
let bool_type = TPrimitive "bool"
let string_type = TPrimitive "string"
let unit_type = TPrimitive "unit"
let tuple_type t = TTuple t
let list_type t = TList t

let rec pp_type ppf =
  let open Format in
  function
  | TVar n -> fprintf ppf "'%d" n
  | TPrimitive s -> fprintf ppf "%s" s
  | TArrow (l, r) ->
    (match l with
     | TArrow _ -> fprintf ppf "(%a) -> %a" pp_type l pp_type r
     | _ -> fprintf ppf "%a -> %a" pp_type l pp_type r)
  | TTuple tl ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf ppf " * ")
         (fun ppf ty ->
           match ty with
           | TArrow _ -> fprintf ppf "(%a)" pp_type ty
           | _ -> fprintf ppf "%a" pp_type ty))
      tl
  | TList t -> fprintf ppf "%a list" pp_type t
;;

type error =
  [ `Occurs_check
  | `No_variable of identifier
  | `Unification_failed of ty * ty
  | `Pattern_matching_error
  | `Not_implemented
  ]

let pp_error ppf : error -> _ =
  let open Stdlib.Format in
  function
  | `Occurs_check -> fprintf ppf "Occurs check failed"
  | `No_variable s -> fprintf ppf "Unbound variable '%s'" s
  | `Unification_failed (l, r) ->
    fprintf ppf "Unification failed on %a and %a" pp_type l pp_type r
  | `Pattern_matching_error -> fprintf ppf "Pattern matching error"
  | `Not_implemented -> fprintf ppf "Not implemented"
;;
