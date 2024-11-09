(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast
open OCamlRV_lib.Parser
open OCamlRV_lib.Pprintast

let parse_to_structure input =
  match parse input with
  | Ok structure -> structure
  | Error _ -> []
;;

let pprint_to_string input =
  Format.fprintf Format.str_formatter "%a" pp_structure_item_list input;
  Format.flush_str_formatter ()
;;

let literal c = ExprLiteral (IntLiteral c)
let binop op a b = ExprBinOperation (op, a, b)
let digit = QCheck.Gen.int_range 0 9

let expr_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
      if n <= 1
      then map literal digit
      else
        frequency
          [ 1, map2 (binop Add) (self (n / 2)) (self (n / 2))
          ; 1, map2 (binop Sub) (self (n / 2)) (self (n / 2))
          ; 1, map2 (binop Mul) (self (n / 2)) (self (n / 2))
          ; 1, map2 (binop Div) (self (n / 2)) (self (n / 2))
          ]))
;;

let gen_structure_item = QCheck.Gen.map (fun expr -> [ SEval expr ]) expr_gen

let arbitrary_lam =
  QCheck.make gen_structure_item ~print:(Format.asprintf "%a" pp_structure_item_list)
;;

let prop_round_trip =
  QCheck.Test.make ~count:10 arbitrary_lam (fun s ->
    try
      (* Format.printf "%s\n" (show_structure s); *)
      let pprinted_structure = pprint_to_string s in
      let parsed_structure = parse_to_structure pprinted_structure in
      s = parsed_structure
    with
    | _ -> true)
;;

let () =
  QCheck_runner.set_seed 12345;
  QCheck_runner.run_tests_main [ prop_round_trip ]
;;
