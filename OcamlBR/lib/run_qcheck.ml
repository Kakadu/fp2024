(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)
(*
(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open OCamlBR.Pr_printer

open OCamlBR.Ast

(* open OCamlBR.Qcheck

   let () =
   print_endline "Testing manual generator.";
   let _ = run_manual () in
   ()
   ;; *)
open OCamlBR.Qcheck

(* let () =
   let _ = run_auto () in
   ()
   ;; *)

(* let () = ignore (QCheck_base_runner.run_tests ~verbose:true [ run_auto ]) *)
let generate n =
  List.iter
    Format.(fprintf std_formatter "%a\n" prpr_structure)
    (QCheck.Gen.generate ~n gen_structure)
;;

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

(* QCheck_runner.run_tests [run_auto] *)
let () = ignore (QCheck_base_runner.run_tests ~verbose:true [ test_structure_generation ])
(* let () = ignore (QCheck_runner.run_tests ~verbose:true [ test_structure_generation ]) *)
*)