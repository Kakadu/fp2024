(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Stdlib.Format
open Ast

type value =
  | ValInt of int
  | ValStr of string
  | ValBool of bool
  | ValFun of pat * pat list * expr
  | ValTup of value list
  | ValBuiltIn of string
  | ValOption of id * value option

type error =
  | NoVariable of string
  | TypeError

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | ValInt v -> fprintf ppf "%i" v
  | ValBool v -> fprintf ppf "'%b'" v
  | ValStr v -> fprintf ppf "%S" v
  | ValTup v ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " * ") pp_value)
      v
  | ValFun _ | ValBuiltIn _ -> fprintf ppf "<fun>"
  | ValOption (tag, None) -> fprintf ppf "%S" tag
  | ValOption ("Some", Some v) -> fprintf ppf "Some %a" pp_value v
  | ValOption (tag, Some v) -> fprintf ppf "[%s] %a" tag pp_value v
;;

let pp_error ppf = function
  | NoVariable str -> fprintf ppf "No variable with name %S" str
  | TypeError -> fprintf ppf "Type error"
;;
