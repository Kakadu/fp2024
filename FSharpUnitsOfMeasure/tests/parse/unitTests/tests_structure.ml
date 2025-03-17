(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pp
open Parse.Structure
open Pprint.Pprinter

let run_si = pp pprint_struct_item pstritem
let run_prog = pp pprint_program pprog

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

let%expect_test "parse structure item which is nested let bindings" =
  run_si
    {| let a = f in
         let b = g in
         let c = h in
         let d = j in
         E |};
  [%expect {|
    let a = f in let b = g in let c = h in let d = j in E|}]
;;

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
  [%expect
    {|
    [<Measure>] type a = m ^ 3|}]
;;

let%expect_test "parse measure type definition with hard binding" =
  run_si {|[<Measure>] type a = m^3 * s / cm^-1|};
  [%expect
    {|
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
  pp
    pprint_program
    pprog
    {| let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5>;; |};
  [%expect {|
    let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5> |}]
;;

let%expect_test "parse example 3 program" =
  pp
    pprint_program
    pprog
    {| match ('X' : char) with _ -> _j531 | _O -> false | "$TNsq^]am" -> a47 | t5 -> gs | __U4' -> g | zqy'p -> true;;
 |};
  [%expect
    {|
    match ('X' : char) with _ -> _j531 | _O -> false | "$TNsq^]am" -> a47 | t5 -> gs | __U4' -> g | zqy'p -> true |}]
;;

let%expect_test "parse example 4 program" =
  pp
    pprint_program
    pprog
    {|
  let rec true = v and _ = q and _ = -4532424078885192588 in _p;;
  
  let 1483337398865069489 = kr and _ = _6r and _ = viwYZ and -4486306013525438741 = 3265597.524462 and _ = cDCL and l7 = false and "A4%" = -546.405974;;
  
  let 'A' = 0.000023 and lO1xQ = true and false = -3569041840172302343 and _ = -3411816405488708691 and -774524459665610219 = "ID]G<b[" and r8hu = y7O0 and aSS0b = -1580.381791 and false = 3014.442914 and eO_S8 = -0.103727<((dd_ / (((((bht * (1 * xd)) * (fkQC ^ 99)) / ((g ^ 64) / (1 ^ 5))) * (((1 ^ 37) * 1) / 1)) / a_2N)) ^ 49) ^ 57>;;
  
  let _ = '(' and j18jw = _Z6Qn and "zy@&qhIfoFgqh=&7R)\nS@E#\ned{7" = true and _ = 'Z' and _e'Jt = acp;;
  
  match let y = zdB and true = z1'13 and i = 4541.311808 in -0.069444 with _ -> _v9x8 | d -> "*.X4eW*<i,y{\n=8X$/2^^V% t3K6SigH?(WH<1'nq{WmS6.Npq:p[a5_<^H>=p;{Eq*TA!`n,!KsD7VO#D0m)GiY" | _'sH0 -> false | c -> pR8q | -2407035380919485009 -> "n=" | 2179.187086 -> false | -0.000855<xM4> -> _C;;
  
    |};
  [%expect
    {|
    let rec true = v and _ = q and _ = -4532424078885192588 in _p

    let 1483337398865069489 = kr and _ = _6r and _ = viwYZ and -4486306013525438741 = 3265597.52446 and _ = cDCL and l7 = false and "A4%" = -546.405974

    let 'A' = 2.3e-05 and lO1xQ = true and false = -3569041840172302343 and _ = -3411816405488708691 and -774524459665610219 = "ID]G<b[" and r8hu = y7O0 and aSS0b = -1580.381791 and false = 3014.442914 and eO_S8 = -0.103727<((dd_ / (((((bht * (1 * xd)) * (fkQC ^ 99)) / ((g ^ 64) / (1 ^ 5))) * (((1 ^ 37) * 1) / 1)) / a_2N)) ^ 49) ^ 57>

    let _ = '(' and j18jw = _Z6Qn and "zy@&qhIfoFgqh=&7R)\\nS@E#\\ned{7" = true and _ = 'Z' and _e'Jt = acp

    match let y = zdB and true = z1'13 and i = 4541.311808 in -0.069444 with _ -> _v9x8 | d -> "*.X4eW*<i,y{\\n=8X$/2^^V% t3K6SigH?(WH<1'nq{WmS6.Npq:p[a5_<^H>=p;{Eq*TA!`n,!KsD7VO#D0m)GiY" | _'sH0 -> false | c -> pR8q | -2407035380919485009 -> "n=" | 2179.187086 -> false | -0.000855<xM4> -> _C |}]
;;

let%expect_test "parse example 5 program" =
  pp
    pprint_program
    pprog
    {|
  let rec -2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)> = y ;;
    |};
  [%expect
    {|
    let rec -2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)> = y |}]
;;

let%expect_test "parse example 6 program" =
  pp
    pprint_program
    pprog
    {|
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
  pp
    pprint_program
    pprog
    {|
    (match a with 0. -> -1312004488025042530 | _ -> "", match a with a -> a | a -> a, a);;
 |};
  [%expect
    {|
    match a with 0. -> -1312004488025042530 | _ -> ("", match a with a -> a | a -> (a, a)) |}]
;;

let%expect_test "parse example 8 program" =
  run_prog
    {|
    (match a with 0. -> -1312004488025042530 | _ -> "", match a with a -> a | a -> a, a);;
 |};
  [%expect
    {|
    match a with 0. -> -1312004488025042530 | _ -> ("", match a with a -> a | a -> (a, a)) |}]
;;

let%expect_test "parse example 8 program" =
  run_prog
    {|
match a with 0. -> -1312004488025042530 | _ -> ("", match a with a -> a | a -> (a, a));; |};
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
  let 267742048371772592 = match Some a with 0. -> a | -28986.9328323<1> -> bsV and _ = 0;;
|};
  [%expect
    {|
    let 267742048371772592 = match Some a with 0. -> a | -28986.9328323<1> -> bsV and _ = 0 |}]
;;

(* doesn't parse '\n' between two definitions *)
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
