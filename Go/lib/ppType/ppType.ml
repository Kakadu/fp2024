(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

let sep_by_comma list print =
  let rec helper acc = function
    | fst :: snd :: tl ->
      let acc = String.concat "" [ acc; print fst; ", " ] in
      helper acc (snd :: tl)
    | fst :: _ -> acc ^ print fst
    | [] -> acc
  in
  helper "" list
;;

let rec print_type = function
  | Type_int -> "int"
  | Type_string -> "string"
  | Type_bool -> "bool"
  | Type_array (size, type') -> asprintf "[%i]%s" size (print_type type')
  | Type_func (arg_types, return_types) ->
    let print_returns =
      match return_types with
      | _ :: _ :: _ -> asprintf " (%s)" (sep_by_comma return_types print_type)
      | type' :: _ -> " " ^ print_type type'
      | [] -> ""
    in
    asprintf "func(%s)%s" (sep_by_comma arg_types print_type) print_returns
  | Type_chan t -> asprintf "chan %s" (print_type t)
;;
