(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


open OCamlRV_lib.Parser
open OCamlRV_lib.Ast
(* Factorial example *)
(* let rec fact n = if n <= 1 then 1 else n * fact (n - 1);; *)

(* let () =
  let fact_ast : structure =
    [ SValue
        ( Rec
        , [ ( PLiteral (StringLiteral "fact")
            , ExprFun
                ( PLiteral (StringLiteral "n")
                , []
                , ExprIf
                    ( ExprBinOperation (Lte, ExprVariable "n", ExprLiteral (IntLiteral 1))
                    , ExprLiteral (IntLiteral 1)
                    , Some
                        (ExprBinOperation
                           ( Mul
                           , ExprVariable "n"
                           , ExprApply
                               ( ExprVariable "fact"
                               , ExprBinOperation
                                   (Sub, ExprVariable "n", ExprLiteral (IntLiteral 1)) )
                           )) ) ) )
          ] )
    ]
  in
  print_endline (show_structure fact_ast);
  
;; *)

(* Тестовый запуск *)

(* Тесты *)
let () =
  let test1 = "if x then y else z" in
  let test2 = "if 5 > 3 then true else false" in
  let test3 = "if a then b else c" in
  let test4 = "if x then 1 + 2 else 3" in
  let test5 = "if true then false else true" in

  let _ = test_parse_pif test1 in
  let _ = test_parse_pif test2 in
  let _ = test_parse_pif test3 in
  let _ = test_parse_pif test4 in
  let _ = test_parse_pif test5 in

  let test6 = "- 5" in
  let test7 = "- x" in
  let test8 = "+ 5" in
  let test9 = "+ x" in
  let test10 = "notx" in

  let _ = test_parse_unop test6 in
  let _ = test_parse_unop test7 in
  let _ = test_parse_unop test8 in
  let _ = test_parse_unop test9 in
  let _ = test_parse_unop test10 in

  let test11 = " -5" in
  let test12 = "2134324" in
  let test13 = "-525" in
  let test14 = " true" in
  let test15 = "false" in

  let _ = test_parse_pnumber test11 in
  let _ = test_parse_pnumber test12 in
  let _ = test_parse_pnumber test13 in
  let _ = test_parse_pnumber test14 in
  let _ = test_parse_pnumber test15 in



  let test16 = "5 + 5 " in
  let test17 = "5+5" in
  let test18 = "2 - 3" in
  let test19 = " 2 -2 -2" in
  let test20 = "4 * 4" in

  let _ = test_parse_binop_expr test16 in
  let _ = test_parse_binop_expr test17 in
  let _ = test_parse_binop_expr test18 in
  let _ = test_parse_binop_expr test19 in
  let _ = test_parse_binop_expr test20 in
  ()

(* let b =
  test_parse "let f = 5" [ ] in
  Format.printf (if b then "ok\n" else "not ok\n"); *)

