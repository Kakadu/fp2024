(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type builtin =
  | BInt of (int -> unit)
  | BString of (string -> unit)

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VTuple of value * value * value list
  | VFun of rec_flag * pattern * pattern list * expr * environment
  | VOption of value option
  | VBuiltin of builtin
  | VFunction of case * case list
(* | VRecord of string * (label * value) * (label * value) list *)

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | VInt x -> fprintf ppf "%d" x
  | VBool b -> fprintf ppf "%b" b
  | VString s -> fprintf ppf "%S" s
  | VUnit -> fprintf ppf "()"
  | VList vl ->
    fprintf
      ppf
      "[%a]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_value)
      vl
  | VTuple (v1, v2, vl) ->
    fprintf
      ppf
      "(%a, %a%a)"
      pp_value
      v1
      pp_value
      v2
      (fun ppf -> function
        | [] -> ()
        | rest ->
          fprintf
            ppf
            ", %a"
            (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
            rest)
      vl
  | VFun _ | VFunction _ -> fprintf ppf "<fun>"
  | VOption v ->
    (match v with
     | Some v -> fprintf ppf "Some %a" pp_value v
     | None -> fprintf ppf "None")
  | VBuiltin _ -> fprintf ppf "<builtin>"
;;

type error =
  [ `Division_by_zero
  | `Unbound_variable of string
  | `Pattern_matching_failure
  | `Type_error
  | `Ill_left_hand_side of string
  ]

let pp_error ppf : error -> unit = function
  | `Division_by_zero -> Format.fprintf ppf {|Division by zero|}
  | `Unbound_variable s -> Format.fprintf ppf {|Unbound variable: %s|} s
  | `Pattern_matching_failure -> Format.fprintf ppf {|Pattern-matching failure|}
  | `Type_error -> Format.fprintf ppf {|Type error|}
  | `Ill_left_hand_side s -> Format.fprintf ppf {|Ill left-hand side %s|} s
;;
