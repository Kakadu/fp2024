(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Parse.Structure
open Angstrom
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
     | Ok env -> pp_env env
     | Error e -> printf "Interpreter error: %a\n" pp_error e)
  | Error e -> printf "Parse error: %s\n" e
;;

(************************** manytests **************************)

let%expect_test "001.ml" =
  let _ = test_interpret {|
      let rec fac n = if n<=1 then 1 else n * fac (n-1)
  |} in
  [%expect
    {|
    val fac : <type> = <fun> |}]
;;

let%expect_test "002if.ml" =
  let _ = test_interpret {|
      let main = if true then 1 else false
  |} in
  [%expect
    {|
    val main : <type> = 1 |}]
;;

let%expect_test "003occurs.ml" =
  let _ =
    test_interpret
      {|
      let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f))
  |}
  in
  [%expect
    {|
    val fix : <type> = <fun> |}]
;;

(* DOES NOT HALT*)
(* let%expect_test "004let_poly.ml\n" =
  let _ = test_interpret {|
      let _1 =
  (fun f -> (f 1, f true)) (fun x -> x)
  |} in
  [%expect
    {|
    val a : <type> = 7
    val b : <type> = [7]
    val c : <type> = [7; 7]
    val d : <type> = [7] |}]
;; *)

(* IS INCORRECT *)
(* let%expect_test "  005.ml\n" =
  let _ =
    test_interpret
      {|
     let _2 = function
  | Some f -> let _ = f "42" in f 42
  | None -> 1
  |}
  in
  [%expect
    {|
    Parse error: : end_of_input |}]
;; *)

let%expect_test "  015tuples.ml" =
  let _ = test_interpret {|
     let rec (a,b) = (a,b)
  |} in
  [%expect
    {|
    Interpreter error: Invalid syntax |}]
;;

let%expect_test "016tuples_mismatch.ml" =
  let _ = test_interpret {|
       let a, _ = 1, 2, 3
  |} in
  [%expect
    {|
    Interpreter error: Match failure |}]
;;

let%expect_test "  097fun_vs_list.ml" =
  let _ = test_interpret {|
       let [a] = (fun x -> x)
  |} in
  [%expect
    {|
    Interpreter error: Match failure |}]
;;

(* IS INCORRECT, CAN'T PARSE UNIT *)
(* let%expect_test "    097fun_vs_unit.ml" =
  let _ = test_interpret {|
         let () = (fun x -> x)
  |} in
  [%expect
    {|
    Parse error: : end_of_input |}]
;; *)

(* ??? *)
let%expect_test "  /098rec_int.ml" =
  let _ = test_interpret {|
          let rec x = x + 1
  |} in
  [%expect
    {|
    Interpreter error: Unbound identificator: x |}]
;;

let%expect_test "  099.ml" =
  let _ = test_interpret {|
  let rec x::[] = [1]
  |} in
  [%expect
    {|
    Parse error: : end_of_input |}]
;;

(************************** other **************************)

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