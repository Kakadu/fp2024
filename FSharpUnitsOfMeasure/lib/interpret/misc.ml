(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

type error =
  | Division_by_zero
  | Type_mismatch
  | Unbound_identificator of string
  | Unsupported_operation of string
  | Match_failure
  | Not_implemented

type builtin_fun =
  | Print_int of (int -> unit)
  | Print_float of (float -> unit)
  | Print_string of (string -> unit)
  | Print_char of (char -> unit)
  | Print_bool of (bool -> unit)
  | Print_endline of (string -> unit)

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VChar of char
  | VString of string
  | VFun of rec_flag * pattern * expression * environment
  | VList of value list
  | VTuple of value * value * value list
  | VOption of value option
  | VFunction of rule * rule list
  | VBuiltin_fun of builtin_fun
  | VUnit

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

let pp_error ppf : error -> unit =
  let open Format in
  function
  | Division_by_zero -> fprintf ppf "Division by zero"
  | Type_mismatch -> fprintf ppf "Type mismatch"
  | Unbound_identificator s -> fprintf ppf "Unbound identificator: %s" s
  | Unsupported_operation s -> fprintf ppf "Unsupported operation: %s" s
  | Match_failure -> fprintf ppf "Match failure"
  | Not_implemented -> fprintf ppf "Not implemented"
;;

let rec pp_value ppf =
  let open Format in
  function
  | VInt i -> fprintf ppf "%d" i
  | VFloat f -> fprintf ppf "%F" f
  | VBool b -> fprintf ppf "%b" b
  | VChar c -> fprintf ppf "%C" c
  | VString s -> fprintf ppf "%S" s
  | VFun _ | VFunction _ | VBuiltin_fun _ -> fprintf ppf "<fun>"
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
  | VOption (Some x) -> fprintf ppf "Some %a" pp_value x
  | VOption None -> fprintf ppf "None"
  | VUnit -> fprintf ppf "()"
;;
