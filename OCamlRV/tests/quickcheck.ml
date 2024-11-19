(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast
open OCamlRV_lib.Parser
open OCamlRV_lib.Pprintast

let arbitrary_structure =
  QCheck.make gen_structure ~print:(Format.asprintf "%a" pp_structure_item_list)
;;

let prop_round_trip =
  QCheck.Test.make ~count:10 arbitrary_structure (fun s ->
    Result.ok s = parse (Format.asprintf "%a" pp_structure_item_list s))
;;

QCheck_base_runner.set_seed 12345;;
QCheck_base_runner.run_tests [ prop_round_trip ]
