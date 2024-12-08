(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypedTree
open Format

let pp_typ fmt typ =
  let rec helper fmt = function
    | Primary s -> fprintf fmt "%S" s
    | Type_var var -> fprintf fmt "'_%d" var
    | Arrow (fst, snd) -> fprintf fmt "(%a) -> %a" helper fst helper snd
  in
  helper fmt typ;
  fprintf fmt "\n"
;;
