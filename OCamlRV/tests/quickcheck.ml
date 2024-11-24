(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast
open OCamlRV_lib.Parser
open OCamlRV_lib.Pprintast

let arbitrary_structure =
  QCheck.make gen_structure ~print:(Format.asprintf "%a" pp_structure_item_list)
;;

let run n =
  QCheck_base_runner.run_tests
    [ QCheck.Test.make ~count:n arbitrary_structure (fun s ->
        (* Stdlib.Format.printf "%s\n" (show_structure s); *)
        Result.ok s = parse (Format.asprintf "%a" pp_structure_item_list s))
    ]
;;

let run_tests n =
  let _ = run n in
  ()
;;

let () =
  Arg.parse
    [ "-seed", Arg.Int QCheck_base_runner.set_seed, " Set seed"
    ; "-gen", Arg.Int run_tests, " Number of runs"
    ]
    (fun _ -> assert false)
    "help"
;;
