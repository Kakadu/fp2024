(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Parser
open Ast
open Pretty_printer

(*let parse str =
  match parse_expr str with
  | Ok str -> show_structure str
  | _ -> Stdlib.print_endline "Parsing failed"
;;*)
(*
let parse str =
  match parse_expr str with
  | Ok ast -> Stdlib.print_endline (pp_structure ast)
  | _ -> Stdlib.print_endline "Parsing failed"
;;
*)
(*
let parse str =
  match parse_expr str with
  | Ok structure -> 
    Format.asprintf "%a" pp_structure structure
  | Error _ -> 
      Format.asprintf "Parsing failed" *)



let parse str =
  match parse_expr str with
  | Ok structure -> Stdlib.print_endline( Format.asprintf "%a" pp_structure structure)
  | Error _ -> Stdlib.print_endline "Parsing failed"

(*let parse_and_print str =
  Stdlib.print_endline (parse str)*)


let%expect_test _ =
  parse "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  [%expect
    {|
    let rec factorial n =
      if n = 0 then 1
      else n * factorial (n - 1)
    in factorial 5
    |}]
;;
(*
let%expect_test _ =
  parse "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1) in factorial 5";
  [%expect
    {|
    let rec factorial n =
      if n = 0 then 1
      else n * factorial (n - 1)
    in factorial 5
    |}]
;;

let%expect_test _ =
  parse "1234 + 676 - 9002 * (52 / 2)";
  [%expect
    {|
    1234 + 676 - 9002 * (52 / 2)
    4905-03495340-95340-59
    |}]
;;

let%expect_test _ =
  parse "if 1234 + 1 = 1235 then let x = 4";
  [%expect
    {|
    if 1234 + 1 = 1235 then
      let x = 4
    |}]
;;

let%expect_test _ =
  parse "let rec 5 = ()";
  [%expect {| 
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "39482309482390842309482438208 + 2";
  [%expect {| 
  Parsing failed
  |}]
;;

let%expect_test _ =
  parse "let x = 5 in let y = 3 in let n = x + y;; if 13 > 12 then let a = 2";
  [%expect {|
    let x = 5 in
    let y = 3 in
    let n = x + y
    in if 13 > 12 then
      let a = 2
  |}]
;;

let%expect_test _ =
  parse "let x = 5 ;; if 13 > 12 then let a = 2";
  [%expect {|
    let x = 5
    in if 13 > 12 then
      let a = 2
  |}]
;;


let%expect_test _ =
  parse "let rec sum n = if n = 0 then 0 else n + sum (n - 1) in sum 10";
  [%expect {|
    let rec sum n =
      if n = 0 then 0
      else n + sum (n - 1)
    in sum 10
  |}]
;;


let%expect_test _ =
  parse "let rec fib n = if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2) in fib 6";
  [%expect {|
    let rec fib n =
      if n = 0 then 0
      else if n = 1 then 1
      else fib (n - 1) + fib (n - 2)
    in fib 6
  |}]
;;
*)