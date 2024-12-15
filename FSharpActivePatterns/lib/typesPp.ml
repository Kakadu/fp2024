(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypedTree
open Format

let pp_typ fmt typ =
  let rec helper fmt = function
    | Primitive s -> fprintf fmt "%S" s
    | Type_var var -> fprintf fmt "'_%d" var
    | Arrow (fst, snd) ->
      (match fst with
       | Arrow _ -> fprintf fmt "(%a) -> %a" helper fst helper snd
       | _ -> fprintf fmt "%a -> %a" helper fst helper snd)
    | Type_list typ -> fprintf fmt "%a list" helper typ
    | Type_tuple (first, second, rest) ->
      fprintf fmt "(";
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        helper
        fmt
        (first :: second :: rest);
      fprintf fmt ")"
    | TOption t -> fprintf fmt "(%a) option" helper t
  in
  helper fmt typ;
  fprintf fmt "\n"
;;
