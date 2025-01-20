(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypedTree
open Format

let rec pp_typ fmt = function
  | Primitive s -> fprintf fmt "%s" s
  | Type_var var -> fprintf fmt "'_%d" var
  | Arrow (fst, snd) ->
    (match fst with
     | Arrow _ -> fprintf fmt "(%a) -> %a" pp_typ fst pp_typ snd
     | _ -> fprintf fmt "%a -> %a" pp_typ fst pp_typ snd)
  | Type_list typ -> fprintf fmt "%a list" pp_typ typ
  | Type_tuple (first, second, rest) ->
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt " * ")
      (fun fmt typ ->
        match typ with
        | Arrow _ -> fprintf fmt "(%a)" pp_typ typ
        | _ -> pp_typ fmt typ)
      fmt
      (first :: second :: rest)
  | TOption t ->
    (match t with
     | Type_tuple _ | Arrow _ -> fprintf fmt "(%a) option" pp_typ t
     | t -> fprintf fmt "%a option" pp_typ t)
;;
