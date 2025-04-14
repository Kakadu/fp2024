(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Base
open Stdlib.Format
open Ast

type value =
  | ValInt of int
  | ValStr of string
  | ValBool of bool
  | ValUnit
  | ValList of value list
  | ValTup of value * value * value list
  | ValFun of rec_state * pat * pat list * expr * env
  | ValBuiltIn of string
  | ValOption of value option

and env = (id, value, String.comparator_witness) Map.t

type error =
  | NoVariable of string
  | TypeError
  | PatternMatchingFail

let rec pp_value ppf = function
  | ValInt v -> fprintf ppf "%i" v
  | ValBool v -> fprintf ppf "'%b'" v
  | ValStr v -> fprintf ppf "%S" v
  | ValUnit -> fprintf ppf "()"
  | ValList v ->
    fprintf ppf "[%a]" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_value) v
  | ValTup (v1, v2, vs) ->
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
      vs
  | ValFun _ | ValBuiltIn _ -> fprintf ppf "<fun>"
  | ValOption v ->
    (match v with
     | Some v -> fprintf ppf "Some %a" pp_value v
     | None -> fprintf ppf "None")
;;

let pp_error ppf = function
  | NoVariable str -> fprintf ppf "Variable '%S' is not defined in the current scope" str
  | TypeError ->
    fprintf ppf "type error: the types do not match or an invalid type was encountered"
  | PatternMatchingFail ->
    fprintf
      ppf
      "pattern matching failed: no matching pattern was found for the given input"
;;
