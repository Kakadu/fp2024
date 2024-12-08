(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Inferencer.Infer
(* let infer_program_test str =
   let parsed = Result.get_ok (Parser.parse_expr str) in
   match infer_program parsed with
   | Ok env -> Format.printf "%a" Inferencer.TypeEnv.pp env
   | Error err -> Format.printf "%a" Typedtree.pp_error err
   ;; *)

let%expect_test _ =
  let _ = infer_program_test {|let f x g = g x in f|} in
  [%expect {| 'a -> ('a -> 'b) -> 'b |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x = x + 2 |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| 1 + 3 |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let a = if true then 2 else 1 |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| fun x y -> x + y |} in
  [%expect {|  |}]
;;
