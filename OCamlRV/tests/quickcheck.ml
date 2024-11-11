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

let gen_literal =
  let open QCheck.Gen in
  oneof
    [ map (fun i -> IntLiteral i) int
    ; map (fun b -> BoolLiteral b) bool
    ; map (fun s -> StringLiteral s) string_printable
    ; return UnitLiteral
    ; return NilLiteral
    ]
;;

let gen_char =
  let open QCheck.Gen in
  map Char.chr (int_range (Char.code 'a') (Char.code 'h'))
;;

let gen_id =
  let open QCheck.Gen in
  string_size (int_range 1 3) ~gen:gen_char
;;

let gen_pattern =
  let open QCheck.Gen in
  oneof
    [ return PAny; map (fun l -> PLiteral l) gen_literal; map (fun l -> PVar l) gen_id ]
;;

let gen_bin_op =
  QCheck.Gen.oneofl [ Add; Sub; Mul; Div; Lt; Gt; Eq; Neq; Lte; Gte; And; Or ]
;;

let expr_gen =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
      if n <= 1
      then
        oneof
          [ map (fun c -> ExprLiteral c) gen_literal
          ; map (fun v -> ExprVariable v) gen_id
          ]
      else
        frequency
          [ 1, map (fun c -> ExprLiteral c) gen_literal
          ; 1, map (fun v -> ExprVariable v) gen_id
          ; ( 1
            , map3
                (fun e1 e2 e3 -> ExprIf (e1, e2, Some e3))
                (self (n / 2))
                (self (n / 2))
                (self (n / 2)) )
          ; ( 1
            , map3
                (fun op e1 e2 -> ExprBinOperation (op, e1, e2))
                gen_bin_op
                (self (n / 2))
                (self (n / 2)) )
          ; 1, map2 (fun e1 e2 -> ExprApply (e1, e2)) (self (n / 2)) (self (n / 2))
          ; 1, map2 (fun p e -> ExprFun (p, e)) gen_pattern (self (n / 2))
          ; ( 1
            , map3
                (fun rf bl e -> ExprLet (rf, bl, e))
                (frequency [ 1, return NonRec; 1, return Rec ])
                (list_size
                   (int_range 1 3)
                   (map2 (fun p e -> p, e) gen_pattern (self (n / 2))))
                (self (n / 2)) )
          ; 1, map (fun el -> ExprTuple el) (list_size (int_range 1 10) (self 0))
          ]))
;;

let gen_structure_item = QCheck.Gen.map (fun e -> [ SEval e ]) expr_gen

let arbitrary =
  QCheck.make gen_structure_item ~print:(Format.asprintf "%a" pp_structure_item_list)
;;

let prop_round_trip =
  QCheck.Test.make ~count:10 arbitrary (fun s ->
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
