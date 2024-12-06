(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


open EUsoltsev_lib
open Ast
open Inferencer
open Inferencer.PP

open Format
open Ast
open Typing
open Parser

(* let parse_test input =
  match parse input with
  | Ok ast -> printf "%s\n" (show_program ast)
  | Error fail -> printf "Ошибка: %s\n" fail
;;
let () = parse_test "let (x, y) = (y + 10, x + 10)"

let expr = ExpLambda
    ( [PatTuple (PatVariable "x", PatVariable "y", [])],
      ExpTuple
        ( ExpBinOper (Plus, ExpIdent "x", ExpConst (ConstInt 10)),
          ExpBinOper (Plus, ExpIdent "y", ExpConst (ConstInt 10)),
          [] ) ) ;;
PP.print_result expr ;;



let pp_infer e =
  match run_inference e with
  | Ok ty -> Stdlib.Format.printf "%a" pp_ty ty
  | Error err -> Stdlib.Format.printf "%a" pp_error err
;;


let pp_parse_expr_and_infer input =
  match Parser.parse_string_expr input with
  | Ok e -> pp_infer e
  | Error _ -> Stdlib.print_endline "Failed to parse"
;;

let () = pp_parse_expr_and_infer "fun x -> x * 666" *)







let () =
  let factorial : program =
    [ ExpLet
        ( true
        , PatVariable "factorial"
        , ExpLambda
            ( [ PatVariable "n" ]
            , ExpBranch
                ( ExpBinOper (LowerThan, ExpIdent "n", ExpConst (ConstInt 2))
                , ExpConst (ConstInt 1)
                , Some
                    (ExpBinOper
                       ( Multiply
                       , ExpIdent "n"
                       , ExpFunction
                           ( ExpIdent "factorial"
                           , ExpBinOper (Minus, ExpIdent "n", ExpConst (ConstInt 1)) ) ))
                ) )
        , None )
    ]
  in
  print_endline (show_program factorial)
;;


  
;;
