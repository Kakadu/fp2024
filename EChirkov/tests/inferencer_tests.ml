(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MiniML.Parser
open MiniML.Inferencer

let test_inferencer s =
  match parse s with
  | Ok s ->
    (* print_endline (show_program s); *)
    (match inference s with
     | Ok env -> MiniML.Inferencer.print_env env
     | Error e -> Format.printf "Type inference error: %a" MiniML.Inferencer.pp_error e)
  | Error _ -> print_endline "Parsing error"
;;

let%expect_test "inference factorial function" =
  test_inferencer "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1)";
  [%expect {|val factorial : (int -> int)|}]
;;

(* ========== const ========== *)

let%expect_test "inference int" =
  test_inferencer "let x = 2";
  [%expect {|val x : int|}]
;;

let%expect_test "inference bool" =
  test_inferencer "let x = true";
  [%expect {|val x : bool|}]
;;

let%expect_test "inference unit" =
  test_inferencer "let x = ()";
  [%expect {|val x : unit|}]
;;

(* ========== bop ========== *)

let%expect_test "inference bop add sub mul div" =
  test_inferencer "let x = 23 + 23 - 45 - (2 * 345) / (-98)";
  [%expect {|
    val x : int|}]
;;

let%expect_test "inference bop unit unify" =
  test_inferencer "let x = true || ()";
  [%expect {|
    Type inference error: Unification failed on unit and bool|}]
;;

let%expect_test "inference bop bool unify" =
  test_inferencer "let f x y = x && (y : int)";
  [%expect {|
    Type inference error: Unification failed on bool and int|}]
;;

(* ========== tuples ========== *)

let%expect_test "inference tuple fst" =
  test_inferencer "let f t = let (x, y) = t in x";
  [%expect {|
    val f : ((🍏 * 🍎) -> 🍏)|}]
;;

let%expect_test "inference tuple 2" =
  test_inferencer "let (x, y) = (23, 12)";
  [%expect {|
    val x : int
    val y : int|}]
;;

let%expect_test "inference tuple 3" =
  test_inferencer "let (x, y, z) = (23, 12, true)";
  [%expect {|
    val x : int
    val y : int
    val z : bool|}]
;;

let%expect_test "inference tuples" =
  test_inferencer {|
    let rec fix f x = f (fix f) x
    let map f p = let (a,b) = p in (f a, f b)
    let fixpoly l =
      fix (fun self l -> map (fun li x -> li (self l) x) l) l
    let feven p n =
      let (e, o) = p in
      if n = 0 then 1 else o (n - 1)
    let fodd p n =
      let (e, o) = p in
      if n = 0 then 0 else e (n - 1)
    let tie = fixpoly (feven, fodd)

    let rec meven n = if n = 0 then 1 else modd (n - 1)
    and modd n = if n = 0 then 1 else meven (n - 1)
    let main =
      let () = print_int (modd 1) in
      let () = print_int (meven 2) in
      let (even,odd) = tie in
      let () = print_int (odd 3) in
      let () = print_int (even 4) in
      0
  |};
  [%expect {|
    val feven : ((🍏 * ((int -> int))) -> (int -> int))
    val fix : (((🍏 -> 🍎) -> (🍏 -> 🍎)) -> (🍏 -> 🍎))
    val fixpoly : (((((((🍏 -> 🍎)) * ((🍏 -> 🍎))) -> (🍏 -> 🍎))) * (((((🍏 -> 🍎)) * ((🍏 -> 🍎))) -> (🍏 -> 🍎)))) -> (((🍏 -> 🍎)) * ((🍏 -> 🍎))))
    val fodd : ((((int -> int)) * 🍏) -> (int -> int))
    val main : int
    val map : ((🍏 -> 🍎) -> ((🍏 * 🍏) -> (🍎 * 🍎)))
    val meven : (int -> int)
    val modd : (int -> int)
    val tie : (((int -> int)) * ((int -> int))) |}]
;;


(* ========== list ========== *)

let%expect_test "inference list pat" =
  test_inferencer "let [a] = [false]";
  [%expect {|
    val a : bool|}]
;;

let%expect_test "inference list integers" =
  test_inferencer "let l = [1; 2; 3]";
  [%expect {| val l : int list |}]
;;

(* ========== vars ========== *)

let%expect_test "inference var simple" =
  test_inferencer "let a = 23 let b = a let c = b";
  [%expect {|
    val a : int
    val b : int
    val c : int|}]
;;

let%expect_test "inference var no" =
  test_inferencer "let a = 23 let b = 45 let c = d";
  [%expect {|
    Type inference error: Undefined variable 'd'|}]
;;


(* ========== fun ========== *)

let%expect_test "2+2" =
  test_inferencer
    {|
  let two = fun f -> fun x -> f (f x)
  let plus = fun m -> fun n -> fun f -> fun x -> m f (n f x) 
  let four = plus two two 
  let x = four (fun x -> x + 1) 0
  |};
  [%expect
    {|
    val four : ((🍏 -> 🍏) -> (🍏 -> 🍏))
    val plus : ((🍏 -> (🍎 -> 🍐)) -> ((🍏 -> (🍊 -> 🍎)) -> (🍏 -> (🍊 -> 🍐))))
    val two : ((🍏 -> 🍏) -> (🍏 -> 🍏))
    val x : int|}]
;;

let%expect_test "2*2" =
  test_inferencer
    {|
  let two = fun f -> fun x -> f (f x)
  let mul = fun m -> fun n -> fun f -> fun x -> m (n f) x 
  let four = mul two two 
  let x = four (fun x -> x + 1) 0
  |};
  [%expect
    {|
    val four : ((🍏 -> 🍏) -> (🍏 -> 🍏))
    val mul : ((🍏 -> (🍎 -> 🍐)) -> ((🍊 -> 🍏) -> (🍊 -> (🍎 -> 🍐))))
    val two : ((🍏 -> 🍏) -> (🍏 -> 🍏))
    val x : int|}]
;;

let%expect_test "inference id" =
  test_inferencer "let id = fun x -> x";
  [%expect {| val id : (🍏 -> 🍏) |}]
;;

let%expect_test "inference app" =
  test_inferencer "let id = fun x -> x let x = id 42";
  [%expect {|
    val id : (🍏 -> 🍏)
    val x : int |}]
;;

let%expect_test "inference higher-order" =
  test_inferencer "let apply = fun f -> fun x -> f x";
  [%expect {| val apply : ((🍏 -> 🍎) -> (🍏 -> 🍎)) |}]
;;


let%expect_test "inference pattern on tuples" =
  test_inferencer "let fst = fun p -> let (x, y) = p in x";
  [%expect {| val fst : ((🍏 * 🍎) -> 🍏) |}]
;;

let%expect_test "inference composition" =
  test_inferencer "let compose = fun f -> fun g -> fun x -> f (g x)";
  [%expect {| val compose : ((🍏 -> 🍎) -> ((🍐 -> 🍏) -> (🍐 -> 🍎))) |}]
;;

(* ========== option ========== *)

let%expect_test "inference option Some" =
  test_inferencer "let o = Some 42";
  [%expect {| val o : int option |}]
;;

let%expect_test "inference option None" =
  test_inferencer "let o = None";
  [%expect {| val o : 🍏 option |}]
;;

let%expect_test "inference wrap" =
  test_inferencer "let wrap = fun x -> Some x";
  [%expect {| val wrap : (🍏 -> 🍏 option) |}]
;;

(* ========== if ========== *)

let%expect_test "inference simple if" =
  test_inferencer "let abs = fun x -> if x < 0 then -x else x";
  [%expect {| val abs : (int -> int) |}]
;;

let%expect_test "inference nested if" =
  test_inferencer "let sign = fun x -> if x > 0 then 1 else if x < 0 then -1 else 0";
  [%expect {| val sign : (int -> int) |}]
;;

(* ========== let ========== *)

let%expect_test "inference shadowing" =
  test_inferencer "let x = let x = 42 in let x = true in x";
  [%expect {| val x : bool |}]
;;

let%expect_test "inference multiple let" =
  test_inferencer "let x = let a = 10 and b = 2 in a + b";
  [%expect {| val x : int |}]
;;

let%expect_test "inference let rec self" =
  test_inferencer "let rec x = x x";
  [%expect {| Type inference error: Cannot construct type: 🍏 appears within (🍏 -> 🍎) |}]
;;

let%expect_test "inference let rec lhs" =
  test_inferencer "let rec (x, y) = (23, 34)";
  [%expect {| Type inference error: Invalid left hand side |}]
;;

let%expect_test "inference let rec rhs" =
  test_inferencer "let rec f = f 23";
  [%expect {| Type inference error: Invalid right hand side |}]
;;

(* ========== constraint ========== *)

let%expect_test "inference constraint" =
  test_inferencer "let f x y = y (x : bool)";
  [%expect {| val f : (bool -> ((bool -> 🍏) -> 🍏)) |}]
;;

let%expect_test "inference constraint 2" =
  test_inferencer "let f = fun x -> fun y -> (x x y : int)";
  [%expect {| Type inference error: Cannot construct type: 🍏 appears within (🍏 -> 🍎) |}]
;;
