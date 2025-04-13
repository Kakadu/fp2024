(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Parse.Structure
open Stdlib.Format
open Interp.Interpret
open Interp.Misc
open Checks

(* Will change <type> with inferenced type when inferencer is ready *)
let pp_env env =
  printf "\n";
  Base.Map.iteri
    ~f:(fun ~key ~data ->
      match Base.Map.find env key with
      | Some _ ->
        if not (is_builtin_fun key)
        then
          if is_builtin_op key
          then printf "val ( %s ) : %s = %a\n" key "<type>" pp_value data
          else printf "val %s : %s = %a\n" key "<type>" pp_value data
      | None -> ())
    env;
  printf "\n"
;;

let test_interpret s =
  let open Format in
  match Angstrom.parse_string ~consume:Angstrom.Consume.All pprog s with
  | Ok parsed ->
    (match eval parsed with
     | Ok (env, _) -> pp_env env
     | Error e -> printf "Interpreter error: %a\n" pp_error e)
  | Error e -> printf "Parse error: %s\n" e
;;

let _ = test_interpret

let%expect_test _ =
  let _ = test_interpret {|
    1
  |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = 1 in
    a
  |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = 1
  |} in
  [%expect {|
    val a : <type> = 1 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = 1 + 1
  |} in
  [%expect {|
    val a : <type> = 2 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = -1
  |} in
  [%expect {|
    val a : <type> = -1 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = -(-(-1))
  |} in
  [%expect {|
    val a : <type> = -1 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let (+) a b = a + b
  |} in
  [%expect {|
    val ( + ) : <type> = <fun> |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
  let a = [2; 1];;
  let b = 3;;
  let c = b :: a;;
  |} in
  [%expect
    {|
    val a : <type> = [2; 1]
    val b : <type> = 3
    val c : <type> = [3; 2; 1] |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    1
  |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = 1 in
    a
  |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = 1
  |} in
  [%expect {|
    val a : <type> = 1 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = 1 + 1
  |} in
  [%expect {|
    val a : <type> = 2 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = -1
  |} in
  [%expect {|
    val a : <type> = -1 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let a = -(-(-1))
  |} in
  [%expect {|
    val a : <type> = -1 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let (a, b) = (4, 3)
  |} in
  [%expect {|
    val a : <type> = 4
    val b : <type> = 3 |}]
;;

let%expect_test _ =
  let _ = test_interpret {|
    let (0, b) = (4, 3)
  |} in
  [%expect {| Interpreter error: Match failure |}]
;;

let%expect_test _ =
  let _ =
    test_interpret
      {|
      let f = true;;
      let g =
        match f with
        | true -> false
        | false -> true;;
      let n = g;;
  |}
  in
  [%expect
    {|
    val f : <type> = true
    val g : <type> = false
    val n : <type> = false |}]
;;

let%expect_test _ =
  let _ =
    test_interpret
      {|
      let rec is_even n =
        if n = 0 then true else is_odd (n - 1)
      and is_odd n =
        if n = 0 then false else is_even (n - 1);;

      let a  = is_even 4
  |}
  in
  [%expect
    {|
    val a : <type> = true
    val is_even : <type> = <fun>
    val is_odd : <type> = <fun> |}]
;;

let%expect_test _ =
  let _ =
    test_interpret
      {|
      let a = 7;;
      let b = (fun x -> [ x ]) a;;

      let c = a :: b;;

      let d = if false && true then c else b
  |}
  in
  [%expect
    {|
    val a : <type> = 7
    val b : <type> = [7]
    val c : <type> = [7; 7]
    val d : <type> = [7] |}]
;;

let%expect_test _ =
  let _ =
    test_interpret
      {|
      let rec factorial n =
        if n <= 1 then 1
        else n * factorial (n-1)

      let a = factorial 4
      let b = factorial 3
  |}
  in
  [%expect
    {|
    val a : <type> = 24
    val b : <type> = 6
    val factorial : <type> = <fun> |}]
;;
