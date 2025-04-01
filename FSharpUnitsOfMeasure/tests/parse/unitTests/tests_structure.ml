(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pp
open Parse.Structure
open Pprint.Pprinter

let run_si = pp pprint_struct_item pstritem
let run_prog = pp pprint_program pprog
let _, _ = run_si, run_prog
(************************** Structure items **************************)

let%expect_test "parse structure item which is single expression" =
  run_si {| a |};
  [%expect {| a |}]
;;

let%expect_test "parse structure item which is do expr" =
  run_si {| do a |};
  [%expect {| a |}]
;;

let%expect_test "parse structure item which is single let binding" =
  run_si {| let x = y |};
  [%expect {|
    let x = y |}]
;;

let%expect_test "parse structure item which is single rec let binding" =
  run_si {| let rec x = x |};
  [%expect {|
    let rec x = x |}]
;;

let%expect_test "parse structure item which is multiple let bindings" =
  run_si {|let a = b
      and c = d
      and e = f
      and i = j|};
  [%expect {|
    let a = b and c = d and e = f and i = j|}]
;;

(* let%expect_test "parse structure item which is nested let bindings" =
  run_si
    {| let a = f in
         let b = g in
         let c = h in
         let d = j in
         E |};
  [%expect {|
    let a = f in let b = g in let c = h in let d = j in E|}]
;; *)

let%expect_test "parse factorial" =
  run_si
    {|let rec factorial n =
        if (n < 2) then 1
        else n * factorial (n - 1) |};
  [%expect
    {|
    let rec factorial = fun n -> if n < 2 then 1 else n * (factorial (n - 1))|}]
;;

let%expect_test "parse measure type definition" =
  run_si {|[<Measure>] type aasdf |};
  [%expect {|
    [<Measure>] type aasdf|}]
;;

let%expect_test "parse measure type definition with binding" =
  run_si {|[<Measure>] type a = m^3|};
  [%expect {|
    [<Measure>] type a = m ^ 3|}]
;;

let%expect_test "parse measure type definition with hard binding" =
  run_si {|[<Measure>] type a = m^3 * s / cm^-1|};
  [%expect {|
    [<Measure>] type a = ((m ^ 3) * s) / (1 / cm)|}]
;;

let%expect_test "don't parse strange idents as measure type def" =
  run_si {| [<Measure>] type + = m^3 |};
  [%expect {|
    : no more choices|}]
;;

let%expect_test "don't parse strange measure type defs" =
  run_si {| [<Mejure>] typchik |};
  [%expect {|
    : no more choices|}]
;;

(************************** Programs **************************)

let%expect_test "parse simple binding as a program" =
  run_prog {| let x = y |};
  [%expect {|
    let x = y |}]
;;

let%expect_test "parse program of bindings separated by ;;" =
  run_prog {|
     let x = y;; let z = 5000
     |};
  [%expect {|
    let x = y

    let z = 5000 |}]
;;

(* works even without '\n'.
   see pprog *)
let%expect_test "parse program of bindings separated by newline" =
  run_prog {|
    let x = y
    let z = w
    |};
  [%expect {|
    let x = y

    let z = w |}]
;;

let%expect_test "parse example 1 program" =
  run_prog {| '_' [];; |};
  [%expect {|
    '_' [] |}]
;;

let%expect_test "parse example 2 program" =
  run_prog {| let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5>;; |};
  [%expect {|
    let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5> |}]
;;

let%expect_test "parse example 3 program" =
  run_prog
    {| match ('X' : char) with _ -> _j531 | _O -> false | "$TNsq^]am" -> a47 | t5 -> gs | __U4' -> g | zqy'p -> true;;
 |};
  [%expect
    {|
    match ('X' : char) with _ -> _j531 | _O -> false | "$TNsq^]am" -> a47 | t5 -> gs | __U4' -> g | zqy'p -> true |}]
;;

let%expect_test "parse example 5 program" =
  run_prog
    {|
  let rec -2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)> = y ;;
    |};
  [%expect
    {|
    let rec -2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)> = y |}]
;;

let%expect_test "parse example 6 program" =
  run_prog {|
let rec true = -4534607695307062870<((v / 1) ^ 5) * ((_L0 * m) ^ 7)>;;   |};
  [%expect {|
    let rec true = -4534607695307062870<((v / 1) ^ 5) * ((_L0 * m) ^ 7)> |}]
;;

let%expect_test "parse example 7 program" =
  run_prog {|
  let 1.04931758405<1 * _5q1> = x1DZ';; |};
  [%expect {|
    let 1.04931758405<1 * _5q1> = x1DZ' |}]
;;

let%expect_test "parse example 8 program" =
  run_prog
    {|
    (match a with 0.0 -> -1312004488025042530 | _ -> "", match a with a -> a | a -> a, a);;
 |};
  [%expect
    {|
    match a with 0. -> -1312004488025042530 | _ -> ("", match a with a -> a | a -> (a, a)) |}]
;;

let%expect_test "parse example 9 program" =
  run_prog
    {|
let false = 3092098660336030153 and a = if if -885480591476070376<a> then u7A_S then fun eLm -> ";2MYS8)[D7[7X1t(3lL}W<D-CUbv@eV?X*QnM G|t+:O/na3";; |};
  [%expect
    {|
    let false = 3092098660336030153 and a = if if -885480591476070376<a> then u7A_S then fun eLm -> ";2MYS8)[D7[7X1t(3lL}W<D-CUbv@eV?X*QnM G|t+:O/na3" |}]
;;

let%expect_test "parse example 10 program" =
  run_prog
    {|
  let 267742048371772592 = match Some a with 0.0 -> a | -28986.9328323<1> -> bsV and _ = 0;;
|};
  [%expect
    {|
    let 267742048371772592 = match Some a with 0. -> a | -28986.9328323<1> -> bsV and _ = 0 |}]
;;

let%expect_test _ =
  run_prog
    {|
[<Measure>] type o_ = ((1 * 1) * td) * 1

[<Measure>] type iq1 = (1 / 1) ^ 56

|};
  [%expect
    {|
  [<Measure>] type o_ = ((1 * 1) * td) * 1

  [<Measure>] type iq1 = (1 / 1) ^ 56 |}]
;;

let%expect_test _ =
  run_prog
    {|
  let rec length xs =
  match xs with
  | [] -> 0
  | h::tl -> 1 + length tl

let length_tail =
  let rec helper acc xs =
  match xs with
  | [] -> acc
  | h::tl -> helper (acc + 1) tl
  in
  helper 0

let rec map f xs =
  match xs with
  | [] -> []
  | a::[] -> [f a]
  | a::b::[] -> [f a; f b]
  | a::b::c::[] -> [f a; f b; f c]
  | a::b::c::d::tl -> f a :: f b :: f c :: f d :: map f tl

let rec append xs ys = match xs with [] -> ys | x::xs -> x::(append xs ys)

let concat =
  let rec helper xs =
    match xs with
    | [] -> []
    | h::tl -> append h (helper tl)
  in helper

let rec iter f xs = match xs with [] -> () | h::tl -> let () = f h in iter f tl

let rec cartesian xs ys =
  match xs with
  | [] -> []
  | h::tl -> append (map (fun a -> (h,a)) ys) (cartesian tl ys)

let main =
  let () = iter print_int [1;2;3] in
  let () = print_int (length (cartesian [1;2] [1;2;3;4])) in
  0

  |};
  [%expect
    {|
let rec length = fun xs -> match xs with [] -> 0 | h :: tl -> 1 + (length tl)

let length_tail = let rec helper = fun acc -> fun xs -> match xs with [] -> acc | h :: tl -> (helper (acc + 1)) tl in helper 0

let rec map = fun f -> fun xs -> match xs with [] -> [] | a :: [] -> [f a] | a :: b :: [] -> [f a; f b] | a :: b :: c :: [] -> [f a; f b; f c] | a :: b :: c :: d :: tl -> (f a) :: ((f b) :: ((f c) :: ((f d) :: ((map f) tl))))

let rec append = fun xs -> fun ys -> match xs with [] -> ys | x :: xs -> x :: ((append xs) ys)

let concat = let rec helper = fun xs -> match xs with [] -> [] | h :: tl -> (append h) (helper tl) in helper

let rec iter = fun f -> fun xs -> match xs with [] -> () | h :: tl -> let () = f h in (iter f) tl

let rec cartesian = fun xs -> fun ys -> match xs with [] -> [] | h :: tl -> (append ((map (fun a -> (h, a))) ys)) ((cartesian tl) ys)

let main = let () = (iter print_int) [1; 2; 3] in let () = print_int (length ((cartesian [1; 2]) [1; 2; 3; 4])) in 0 |}]
;;
