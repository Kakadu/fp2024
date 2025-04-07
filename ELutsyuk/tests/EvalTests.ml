(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

let test_interpret str =
  let open Stdlib.Format in
  match Parser.Parse.parse_program str with
  | Ok parsed ->
    (match Interpreter.interpret_program parsed with
     | Ok _ -> ()
     | Error err ->
       fprintf std_formatter "Interpretation error: %a\n" Forest.ValuesTree.pp_error err)
  | Error err -> printf "Parsing error: %s\n" err
;;

let%expect_test "interpret_" =
  test_interpret {| 
    let f x = x
    let a = f 5|};
  [%expect {|
    {
    val a : int -> bool = <fun>
    val b : bool = false
    } |}]
;;
