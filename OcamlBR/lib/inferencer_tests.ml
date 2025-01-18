(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Inferencer.Infer
open Typedtree

let infer_program_test s =
  let open Stdlib.Format in
  let open Interpreter in
  match Parser.parse_expr s with
  | Ok parsed ->
    (match infer_program parsed with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
         if print_key key then printf "val %s : %a\n" key pp_ty ty)
     | Error e -> printf "Infer error: %a\n" pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;

let infer_from_file file_name =
  let file_path = "../../../../../tests/manytests/" ^ file_name in
  let input =
    let ic = open_in file_path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    content
  in
  let result = infer_program_test input in
  result
;;

let%expect_test "typed_001fac" =
  let _ = infer_from_file "typed/001fac.ml" in
  [%expect {| 
  val fac : int -> int
  val main : int|}]
;;

let%expect_test "typed_002fac" =
  let _ = infer_from_file "typed/002fac.ml" in
  [%expect {| 
  val fac_cps : int -> ((int -> int) -> int)
  val main : int|}]
;;

let%expect_test "typed_003fib" =
  let _ = infer_from_file "typed/003fib.ml" in
  [%expect
    {| 
  val fib : int -> int
  val fib_acc : int -> (int -> (int -> int))
  val main : int |}]
;;

let%expect_test "typed_004manyargs" =
  let _ = infer_from_file "typed/004manyargs.ml" in
  [%expect
    {| 
  val main : int
  val test10 : int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> (int -> int)))))))))
  val test3 : int -> (int -> (int -> int))
  val wrap : '0 -> '0 |}]
;;

let%expect_test "typed_005fix" =
  let _ = infer_from_file "typed/005fix.ml" in
  [%expect
    {| 
  val fac : (int -> int) -> (int -> int)
  val fix : ((int -> int) -> (int -> int)) -> (int -> int)
  val main : int |}]
;;

let%expect_test "typed_006partial" =
  let _ = infer_from_file "typed/006partial.ml" in
  [%expect {|
  val foo : int -> int
  val main : int |}]
;;

let%expect_test "typed_006partial2" =
  let _ = infer_from_file "typed/006partial2.ml" in
  [%expect {|
  val foo : int -> (int -> (int -> int))
  val main : int |}]
;;

let%expect_test "typed_006partial3" =
  let _ = infer_from_file "typed/006partial3.ml" in
  [%expect {| 
  val foo : int -> (int -> (int -> unit))
  val main : int |}]
;;

let%expect_test "typed_007order" =
  let _ = infer_from_file "typed/007order.ml" in
  [%expect
    {| 
  val _start : unit -> (unit -> (int -> (unit -> (int -> (int -> (unit -> (int -> (int -> int))))))))
  val main : unit |}]
;;

let%expect_test "typed_008ascription" =
  let _ = infer_from_file "typed/008ascription.ml" in
  [%expect
    {| 
  val addi : ('2 -> (bool -> int)) -> (('2 -> bool) -> ('2 -> int))
  val main : int |}]
;;

let%expect_test "typed_009let_poly" =
  let _ = infer_from_file "typed/009let_poly.ml" in
  [%expect {| val temp : (int * bool) |}]
;;

let%expect_test "typed_010sukharev" =
  let _ = infer_from_file "typed/010sukharev.ml" in
  [%expect
    {|
    val _1 : int -> (int -> ((int * '3) -> bool))
    val _2 : int
    val _3 : ((int * string)) option
    val _4 : int -> '10
    val _42 : int -> bool
    val _5 : int
    val _6 : ('23) option -> '23
    val id1 : '32 -> '32
    val id2 : '33 -> '33
    val int_of_option : (int) option -> int |}]
;;

let%expect_test "typed_015tuples" =
  let _ = infer_from_file "typed/015tuples.ml" in
  [%expect
    {|
  val feven : ('29 * int -> int) -> (int -> int)
  val fix : ((((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int)) -> (((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int))) -> (((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int))
  val fixpoly : ((int -> int * int -> int) -> (int -> int) * (int -> int * int -> int) -> (int -> int)) -> (int -> int * int -> int)
  val fodd : (int -> int * '36) -> (int -> int)
  val main : int
  val map : ('9 -> '11) -> (('9 * '9) -> ('10 * '11))
  val meven : int -> int
  val modd : int -> int
  val tie : (int -> int * int -> int) |}]
;;

let%expect_test "typed_016lists" =
  let _ = infer_from_file "typed/016lists.ml" in
  [%expect
    {|
  val append : (int * int) list -> ((int * int) list -> (int * int) list)
  val cartesian : int list -> (int list -> (int * int) list)
  val concat : (int * int) list list -> (int * int) list
  val iter : (int -> unit) -> (int list -> unit)
  val length : (int * int) list -> int
  val length_tail : '16 list -> int
  val main : int
  val map : (int -> (int * int)) -> (int list -> (int * int) list) |}]
;;

let%expect_test "do_not_type_001" =
  let _ = infer_from_file "do_not_type/001.ml" in
  [%expect {| Infer error: Undefined variable "fac" |}]
;;

let%expect_test "do_not_type_002if" =
  let _ = infer_from_file "do_not_type/002if.ml" in
  [%expect {| Infer error: Unification failed on int and bool  |}]
;;

let%expect_test "do_not_type_003occurs" =
  let _ = infer_from_file "do_not_type/003occurs.ml" in
  [%expect {| Infer error: Occurs check failed |}]
;;

let%expect_test "do_not_type_004let_poly" =
  let _ = infer_from_file "do_not_type/004let_poly.ml" in
  [%expect {| Infer error: Unification failed on int and bool |}]
;;

let%expect_test "do_not_type_015tuples" =
  let _ = infer_from_file "do_not_type/015tuples.ml" in
  [%expect {| Infer error: Ill left-hand side : only variables are allowed  |}]
;;

let%expect_test "do_not_type_016" =
  let _ = infer_from_file "do_not_type/016tuples_mismatch.ml" in
  [%expect {| Infer error: Unification failed on ('0 * '1) and (int * int * int)  |}]
;;

let%expect_test "do_not_type_097fun_vs_list" =
  let _ = infer_from_file "do_not_type/097fun_vs_list.ml" in
  [%expect {| Infer error: Unification failed on '2 list and '0 -> '0  |}]
;;

let%expect_test "do_not_type_097fun_vs_unit" =
  let _ = infer_from_file "do_not_type/097fun_vs_unit.ml" in
  [%expect {| Infer error: Unification failed on unit and '0 -> '0  |}]
;;

let%expect_test "do_not_type_098" =
  let _ = infer_from_file "do_not_type/098rec_int.ml" in
  [%expect {| Infer error: Ill right-hand side of let rec  |}]
;;

let%expect_test "do_not_type_099" =
  let _ = infer_from_file "do_not_type/099.ml" in
  [%expect {| Infer error: Ill left-hand side : only variables are allowed  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let n x = x in let f g = g 3 in f n  |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x = x |} in
  [%expect {| val f : '0 -> '0 |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x = x + 2 |} in
  [%expect {| val f : int -> int|}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let x = 2 in x = 1 |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let rec fac n = if n < 1 then 1 else n * fac (n - 1) |} in
  [%expect {| val fac : int -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let rec x = 1+ x|} in
  [%expect {| Infer error: Ill right-hand side of let rec |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test
      {|let rec is_even n = if n = 0 then true else is_odd (n - 1) and is_odd n = if n = 0 then false else is_even (n - 1)|}
  in
  [%expect {|
    val is_even : int -> bool
    val is_odd : int -> bool |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|let square x = x*x in let id x = x in (id square) (id  2) |}
  in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let x = 2 in let a = true in not a && x |} in
  [%expect {| Infer error: Unification failed on int and bool |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let rec f x = f |} in
  [%expect {| Infer error: Occurs check failed |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let a = if true then 2 + 9 else 1 |} in
  [%expect {| val a : int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| if a then 2 else 1 |} in
  [%expect {| Infer error: Undefined variable "a" |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| fun x y -> x + y |} in
  [%expect {|  |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let f x y z w = if y&&z then x else  w + 1 |} in
  [%expect {| val f : int -> (bool -> (bool -> (int -> int))) |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let a = fun x::y::z::w -> if z > 0 then y else x |} in
  [%expect {| val a : int list -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let b = fun (a,b,(2::t), d) -> a + d  |} in
  [%expect {| val b : (int * '1 * int list * int) -> int |}]
;;

let%expect_test _ =
  let _ = infer_program_test {| let (<|>) a b = a/b + b*a |} in
  [%expect {| val <|> : int -> (int -> int) |}]
;;

let%expect_test _ =
  let _ = infer_program_test {|let w [2; v] (y, dx, d) = (-4, 5+v, true&&d) |} in
  [%expect {| val w : int list -> (('2 * '3 * bool) -> (int * int * bool)) |}]
;;

let%expect_test _ =
  let _ =
    infer_program_test {|let f = fun ((3, true): int*bool) x -> if x then 4 else 0  |}
  in
  [%expect {| val f : (int * bool) -> (bool -> int) |}]
;;
