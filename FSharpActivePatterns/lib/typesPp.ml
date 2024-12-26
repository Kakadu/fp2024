(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypedTree
open Format

let pp_typ fmt typ =
  let rec helper fmt = function
    | Primitive s -> fprintf fmt "%s" s
    | Type_var var -> fprintf fmt "'_%d" var
    | Arrow (fst, snd) ->
      (match fst with
       | Arrow _ -> fprintf fmt "(%a) -> %a" helper fst helper snd
       | _ -> fprintf fmt "%a -> %a" helper fst helper snd)
    | Type_list typ -> fprintf fmt "%a list" helper typ
    | Type_tuple (first, second, rest) ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " * ")
        (fun fmt typ ->
           match typ with
           | Arrow _ -> fprintf fmt "(%a)" helper typ
           | _ -> helper fmt typ)
        fmt
        (first :: second :: rest);
    | TOption t -> 
      match t with
      | Type_tuple _ | Arrow _ -> 
        fprintf fmt "(%a) option" helper t;
      | t -> fprintf fmt "%a option" helper t 
  in
  helper fmt typ;
  fprintf fmt "\n"
;;
