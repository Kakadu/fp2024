(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open MiniML.Parser

let%expect_test "normal identifier" =
  let result = parse_string ~consume:Consume.Prefix parse_id "meow" in
  (match result with
   | Ok res -> print_endline res
   | Error err -> Printf.printf "Error: %s\n" err);
  [%expect {| meow |}]
;;

let%expect_test "keyword identifier" =
  let result = parse_string ~consume:Consume.Prefix parse_id "while" in
  (match result with
   | Ok res -> print_endline res
   | Error err -> Printf.printf "Error%s\n" err);
  [%expect {| Error: Identifier must not match the keyword. |}]
;;
