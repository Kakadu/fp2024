(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
let infer_program_test str =
  let parsed = Result.get_ok (Parser.parse_expr str) in
  match Inferencer.Infer.infer_program parsed with
  | Ok env -> Format.printf "%a" Inferencer.TypeEnv.pp env
  | Error err -> Format.printf "%a" Typedtree.pp_error err
;;

let%expect_test _ =
  let _ = infer_program_test {|let f x g = g x in f|} in
  [%expect {| 'a -> ('a -> 'b) -> 'b |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let x = 2 in x - 9 |} in
  [%expect {|  |}]
;;
