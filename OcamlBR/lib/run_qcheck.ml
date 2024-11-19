(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlBR.Qcheck

let run_tests n =
  let _ = run_auto n in
  ()
;;

let () =
  Arg.parse
    [ "-seed", Arg.Int QCheck_base_runner.set_seed, " Set seed"
    ; "-stop", Arg.Unit (fun _ -> exit 0), " Exit"
    ; "-gen", Arg.Int run_tests, " Number of runs"
    ]
    (fun _ -> assert false)
    "help"
;;
