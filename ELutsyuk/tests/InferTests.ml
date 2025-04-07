(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Inferencer.Infer
open Stdlib.Format

let test_inference str =
  match Parser.Parse.parse_program str with
  | Ok parsed ->
    (match inference_program parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(Scheme (_, typ)) ->
         if String.equal key "print_int" || String.equal key "print_endline"
         then ()
         else Stdlib.Format.printf "val %s : %a\n" key Forest.TypesTree.pp_typ typ)
     | Error e -> printf "Infer error: %a\n" Forest.TypesTree.pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;

let%expect_test "inference_arithmetic" =
  test_inference "let cat = 1 + 2 * 3 / 10";
  [%expect {|
    val cat : int |}]
;;

let%expect_test "inference_fun_with_argument" =
  test_inference {| let foo x = x + 100 |};
  [%expect {|
    val foo : int -> int |}]
;;

let%expect_test "inference_rec_fun_with_argument" =
  test_inference {| let rec foo x = foo 5 - 1 |};
  [%expect {|
    val foo : int -> int |}]
;;

let%expect_test "inference_fun_with_nesting" =
  test_inference {| let add_one x = let double y = y * 2 in double (x + 1) |};
  [%expect {| val add_one : int -> int |}]
;;

let%expect_test "inference_polymorphic_fun" =
  test_inference "let identity x = x";
  [%expect {| val identity : '0 -> '0 |}]
;;

let%expect_test "inference_fun_with_tuple_argument" =
  test_inference {| let sum (x, y) = x + y|};
  [%expect {| val sum : (int * int) -> int |}]
;;

let%expect_test "inference_unbound_variable" =
  test_inference {| let foo x = x + a |};
  [%expect {|
    Infer error: Unbound variable: a |}]
;;

let%expect_test "inference_many_fun" =
  test_inference {| let a = fun x y -> fun z -> fun w -> fun c -> x + y + z + w+ c |};
  [%expect {|
val a : int -> int -> int -> int -> int -> int |}]
;;

let%expect_test "inference_unit" =
  test_inference {| let x = () |};
  [%expect {|
    val x : unit |}]
;;

let%expect_test "inference_tuple" =
  test_inference {| let meow = 5;; let x = (1, "a", meow ) |};
  [%expect {|
    val meow : int
    val x : (int * string * int) |}]
;;

let%expect_test "inference_combined_type" =
  test_inference {| let foo x = if x then [Some x; Some x] else [None; None]|};
  [%expect {|
    val foo : bool -> bool option list |}]
;;

let%expect_test "inference_001" =
  test_inference "let a = if true then 2 + 9 else 1 ";
  [%expect {|
    val a : int |}]
;;

let%expect_test "inference_002if" =
  test_inference "let main = if true then 1 else false";
  [%expect {|
    Infer error: Unification failed: cannot unify int and bool |}]
;;

let%expect_test "inference_003occurs" =
  test_inference
    "let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f))";
  [%expect {|
    Infer error: Occurs check failed: variable '1 appears in '1 -> '3. |}]
;;

let%expect_test "inference_004let_poly" =
  test_inference "let _1 =\n  (fun f -> (f 1, f true)) (fun x -> x)";
  [%expect {|
    Infer error: Unification failed: cannot unify int and bool|}]
;;

let%expect_test "inference_015tuples" =
  test_inference "let rec (a,b) = (a,b)";
  [%expect {|
    Infer error: Invalid recursive pattern |}]
;;

let%expect_test "inference_016tuples_mismatches" =
  test_inference "let a, b = 1, 2, 3";
  [%expect
    {|
    Infer error: Unification failed: cannot unify ('1 * '0) and (int * int * int) |}]
;;

let%expect_test "inference_097fun_vs_list" =
  test_inference "let [a] = (fun x -> x)";
  [%expect {| |}]
;;

let%expect_test "inference_098rec_int" =
  test_inference "let rec x = x + 1";
  [%expect {|
    val x : int |}]
;;

let%expect_test "inference_099" =
  test_inference "let rec x::[] = [1]";
  [%expect {|
    Infer error: Invalid recursive pattern |}]
;;

let%expect_test "inference_typed_0" =
  test_inference "let (a : int list) = 5";
  [%expect {| |}]
;;

let%expect_test "inference_typed_001fac" =
  test_inference
    "let rec fac n = if n<=1 then 1 else n * fac (n-1)\n\n\
     let main =\n\
    \  let () = print_int (fac 4) in\n\
    \  0\n";
  [%expect {|
    val fac : int -> int
    val main : int |}]
;;

let%expect_test "inference_typed_002fac" =
  test_inference
    "let rec fac_cps n k =\n\
    \  if n=1 then k 1 else\n\
    \  fac_cps (n-1) (fun p -> k (p*n))\n\n\
     let main =\n\
    \  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in\n\
    \  0\n";
  [%expect {|
    val fac_cps : int -> (int -> '8) -> '8
    val main : int |}]
;;

let%expect_test "inference_typed_003fib" =
  test_inference
    "let rec fib_acc a b n =\n\
    \  if n=1 then b\n\
    \  else\n\
    \    let n1 = n-1 in\n\
    \    let ab = a+b in\n\
    \    fib_acc b ab n1\n\n\
     let rec fib n =\n\
    \  if n<2\n\
    \  then n\n\
    \  else fib (n - 1) + fib (n - 2) \n\n\
     let main =\n\
    \  let () = print_int (fib_acc 0 1 4) in\n\
    \  let () = print_int (fib 4) in\n\
    \  0\n";
  [%expect
    {|
    val fib : int -> int
    val fib_acc : int -> int -> int -> int
    val main : int |}]
;;

let%expect_test "inference_typed_004manyargs" =
  test_inference
    "let wrap f = if 1 = 1 then f else f\n\n\
     let test3 a b c =\n\
    \  let a = print_int a in\n\
    \  let b = print_int b in\n\
    \  let c = print_int c in\n\
    \  0\n\n\
     let test10 a b c d e f g h i j = a + b + c + d + e + f + g + h + i + j\n\n\
     let main =\n\
    \  let rez =\n\
    \      (wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000\n\
    \         1000000000)\n\
    \  in\n\
    \  let () = print_int rez in\n\
    \  let temp2 = wrap test3 1 10 100 in\n\
    \  0\n";
  [%expect
    {|
    val main : int
    val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    val test3 : '2 -> '3 -> '4 -> int
    val wrap : '0 -> '0 |}]
;;

let%expect_test "inference_typed_005fix" =
  test_inference
    "let rec fix f x = f (fix f) x\n\n\
     let fac self n = if n<=1 then 1 else n * self (n-1)\n\n\
     let main =\n\
    \  let () = print_int (fix fac 6) in\n\
    \  0\n";
  [%expect
    {|
    val fac : (int -> int) -> int -> int
    val fix : (('2 -> '5) -> '2 -> '5) -> '2 -> '5
    val main : int |}]
;;

let%expect_test "inference_typed_006partial" =
  test_inference
    "let foo b = if b then (fun foo -> foo+2) else (fun foo -> foo*10)\n\n\
     let foo x = foo true (foo false (foo true (foo false x)))\n\
     let main =\n\
    \  let () = print_int (foo 11) in\n\
    \  0";
  [%expect {|
    val foo : int -> int
    val main : int |}]
;;

let%expect_test "inference_typed_006partial2" =
  test_inference
    "let foo a b c =\n\
    \  let () = print_int a in\n\
    \  let () = print_int b in\n\
    \  let () = print_int c in\n\
    \  a + b * c\n\n\
     let main =\n\
    \  let foo = foo 1 in\n\
    \  let foo = foo 2 in\n\
    \  let foo = foo 3 in\n\
    \  let () = print_int foo in\n\
    \  0";
  [%expect {|
    val foo : int -> int -> int -> int
    val main : int |}]
;;

let%expect_test "inference_typed_006partial3" =
  test_inference
    "let foo a =\n\
    \  let () = print_int a in fun b ->\n\
    \  let () = print_int b in fun c ->\n\
    \  print_int c\n\n\
     let main =\n\
    \  let () = foo 4 8 9 in\n\
    \  0";
  [%expect {|
    val foo : '0 -> '2 -> int -> unit
    val main : int |}]
;;

let%expect_test "inference_typed_007order" =
  test_inference
    "let _start () () a () b _c () d __ =\n\
    \  let () = print_int (a+b) in\n\
    \  let () = print_int __ in\n\
    \  a*b / _c + d\n\n\n\
     let main =\n\
    \  print_int (_start (print_int 1) (print_int 2) 3 (print_int 4) 100 1000 (print_int \
     (-1)) 10000 (-555555))";
  [%expect {|
    Infer error: Unbound variable: __ |}]
;;

let%expect_test "inference_typed_008ascription" =
  test_inference
    "let addi = fun f g x -> (f x (g x: bool) : int)\n\n\
     let main =\n\
    \  let () = print_int (addi (fun x b -> if b then x+1 else x*2) (fun _start -> \
     _start/2 = 0) 4) in\n\
    \  0";
  [%expect {|
    Infer error: Unification failed: cannot unify int and unit |}]
;;

let%expect_test "typed_009let_poly" =
  test_inference "let temp =\n  let f = fun x -> x in\n  (f 1, f true)";
  [%expect {|
    val temp : (int * bool) |}]
;;

let%expect_test "inference_typed_015tuples" =
  test_inference
    "let rec fix f x = f (fix f) x\n\
     let map f p = let (a,b) = p in (f a, f b)\n\
     let fixpoly l =\n\
    \  fix (fun self l -> map (fun li x -> li (self l) x) l) l\n\
     let feven p n =\n\
    \  let (e, o) = p in\n\
    \  if n = 0 then 1 else o (n - 1)\n\
     let fodd p n =\n\
    \  let (e, o) = p in\n\
    \  if n = 0 then 0 else e (n - 1)\n\
     let tie = fixpoly (feven, fodd)\n\n\
     let rec meven n = if n = 0 then 1 else modd (n - 1)\n\
     and modd n = if n = 0 then 1 else meven (n - 1)\n\
     let main =\n\
    \  let () = print_int (modd 1) in\n\
    \  let () = print_int (meven 2) in\n\
    \  let (even,odd) = tie in\n\
    \  let () = print_int (odd 3) in\n\
    \  let () = print_int (even 4) in\n\
    \  0\n";
  [%expect {|
    Infer error: Unbound variable: a |}]
;;
