(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pprint.Pp
open Parse.Structure
open Pprint.Pprinter

(************************** Structure items **************************)

let%expect_test "parse structure item which is single expression" =
  pp pprint_struct_item pstritem {| a |};
  [%expect {| a |}]
;;

let%expect_test "parse structure item which is do expr" =
  pp pprint_struct_item pstritem {| do a |};
  [%expect {| a |}]
;;

let%expect_test "parse structure item which is single let binding" =
  pp pprint_struct_item pstritem {| let x = y |};
  [%expect {|
    let x = y |}]
;;

let%expect_test "parse structure item which is single rec let binding" =
  pp pprint_struct_item pstritem {| let rec x = x |};
  [%expect {|
    let rec x = x |}]
;;

let%expect_test "parse structure item which is multiple let bindings" =
  pp
    pprint_struct_item
    pstritem
    {|let a = b
      and c = d
      and e = f
      and i = j|};
  [%expect {|
    let a = b and c = d and e = f and i = j|}]
;;

(* Doesn't halt on 4+ nested let ... in's, the reason is in pexpr_letin *)
(* let%expect_test "parse structure item which is nested let bindings" =
  pp
    pprint_struct_item
    pstritem
    {| let a = f in
         let b = g in
         let c = h in
         let d = j in
         E |};
  [%expect
    {|
    (Str_item_eval
       (Expr_let (Nonrecursive,
          (Bind ((Pattern_ident_or_op "a"), (Expr_const (Const_int 1)))),
          [],
          (Expr_let (Nonrecursive,
             (Bind ((Pattern_ident_or_op "b"), (Expr_const (Const_int 2)))),
             [],
             (Expr_let (Nonrecursive,
                (Bind ((Pattern_ident_or_op "c"), (Expr_const (Const_int 3)))),
                [], (Expr_ident_or_op "e")))
             ))
          )))|}]
;; *)

let%expect_test "parse factorial" =
  pp
    pprint_struct_item
    pstritem
    {|let rec factorial n =
        if (n < 2) then 1
        else n * factorial (n - 1) |};
  [%expect
    {|
    let rec factorial = fun n -> if n < 2 then 1 else n * (factorial (n - 1))|}]
;;

let%expect_test "parse measure type definition" =
  pp2 pp_structure_item pstritem {|[<Measure>] type aasdf |};
  [%expect {|
    (Str_item_type_def (Measure_type_def ("aasdf", None)))|}]
;;

let%expect_test "parse measure type definition with binding" =
  pp2 pp_structure_item pstritem {|[<Measure>] type a = m^3|};
  [%expect
    {|
    (Str_item_type_def
       (Measure_type_def ("a", (Some (Measure_pow ((Measure_ident "m"), 3))))))|}]
;;

let%expect_test "parse measure type definition with hard binding" =
  pp2 pp_structure_item pstritem {|[<Measure>] type a = m^3 * s / cm^-1|};
  [%expect
    {|
    (Str_item_type_def
       (Measure_type_def ("a",
          (Some (Measure_div (
                   (Measure_prod ((Measure_pow ((Measure_ident "m"), 3)),
                      (Measure_ident "s"))),
                   (Measure_div (Measure_dimless, (Measure_ident "cm"))))))
          )))|}]
;;

let%expect_test "don't parse strange idents as measure type def" =
  pp2 pp_structure_item pstritem {| [<Measure>] type + = m^3 |};
  [%expect {|
    : no more choices|}]
;;

let%expect_test "don't parse strange measure type defs" =
  pp2 pp_structure_item pstritem {| [<Mejure>] typchik a |};
  [%expect {|
    : no more choices|}]
;;

(************************** Programs **************************)

let%expect_test "parse simple binding as a program" =
  pp pprint_program pprog {| let x = y |};
  [%expect {|
    let x = y;; |}]
;;

let%expect_test "parse program of bindings separated by ;;" =
  pp pprint_program pprog {|
     let x = y;; let z = 5000
     |};
  [%expect {|
    let x = y;;

    let z = 5000;; |}]
;;

(* Not done yet *)
let%expect_test "parse program of bindings separated by newline" =
  pp pprint_program pprog {|
    let x = y;;
    let z = w
    |};
  [%expect {|
    let x = y;;

    let z = w;; |}]
;;

let%expect_test "parse example 1 program" =
  pp pprint_program pprog {| '_' [];; |};
  [%expect {|
    '_' [];; |}]
;;

let%expect_test "parse example 2 program" =
  pp
    pprint_program
    pprog
    {| let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5>;; |};
  [%expect {|
    let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5>;; |}]
;;

let%expect_test "parse example 3 program" =
  pp
    pprint_program
    pprog
    {| match ('X' : char) with _ -> _j531 | _O -> false | "$TNsq^]am" -> a47 | t5 -> gs | __U4' -> g | zqy'p -> true;;
 |};
  [%expect
    {|
    match ('X' : char) with _ -> _j531 | _O -> false | "$TNsq^]am" -> a47 | t5 -> gs | __U4' -> g | zqy'p -> true;; |}]
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
    let rec true = v and _ = q and _ = -4532424078885192588 in _p;;

    let 1483337398865069489 = kr and _ = _6r and _ = viwYZ and -4486306013525438741 = 3265597.52446 and _ = cDCL and l7 = false and "A4%" = -546.405974;;

    let 'A' = 2.3e-05 and lO1xQ = true and false = -3569041840172302343 and _ = -3411816405488708691 and -774524459665610219 = "ID]G<b[" and r8hu = y7O0 and aSS0b = -1580.381791 and false = 3014.442914 and eO_S8 = -0.103727<((dd_ / (((((bht * (1 * xd)) * (fkQC ^ 99)) / ((g ^ 64) / (1 ^ 5))) * (((1 ^ 37) * 1) / 1)) / a_2N)) ^ 49) ^ 57>;;

    let _ = '(' and j18jw = _Z6Qn and "zy@&qhIfoFgqh=&7R)\\nS@E#\\ned{7" = true and _ = 'Z' and _e'Jt = acp;;

    match let y = zdB and true = z1'13 and i = 4541.311808 in -0.069444 with _ -> _v9x8 | d -> "*.X4eW*<i,y{\\n=8X$/2^^V% t3K6SigH?(WH<1'nq{WmS6.Npq:p[a5_<^H>=p;{Eq*TA!`n,!KsD7VO#D0m)GiY" | _'sH0 -> false | c -> pR8q | -2407035380919485009 -> "n=" | 2179.187086 -> false | -0.000855<xM4> -> _C;; |}]
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
    let rec -2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)> = y;; |}]
;;

let%expect_test "parse example 6 program" =
  pp
    pprint_program
    pprog
    {|
let rec true = -4534607695307062870<((v / 1) ^ 5) * ((_L0 * m) ^ 7)>;;   |};
  [%expect
    {|
    let rec true = -4534607695307062870<((v / 1) ^ 5) * ((_L0 * m) ^ 7)>;; |}]
;;

let%expect_test "parse example 7 program" =
  pp pprint_program pprog {|
  let 1.04931758405<1 * _5q1> = x1DZ';; |};
  [%expect {|
    let 1.04931758405<1 * _5q1> = x1DZ';; |}]
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
    match a with 0. -> -1312004488025042530 | _ -> ("", match a with a -> a | a -> (a, a));; |}]
;;

let%expect_test "parse example 8 program" =
  pp2
    pp_program
    pprog
    {|
    (match a with 0. -> -1312004488025042530 | _ -> "", match a with a -> a | a -> a, a);;
 |};
  [%expect
    {|
    [(Str_item_eval
        (Expr_match ((Expr_ident_or_op "a"),
           (Rule ((Pattern_const (Const_float 0.)),
              (Expr_const (Const_int -1312004488025042530)))),
           [(Rule (Pattern_wild,
               (Expr_tuple ((Expr_const (Const_string "")),
                  (Expr_match ((Expr_ident_or_op "a"),
                     (Rule ((Pattern_ident_or_op "a"), (Expr_ident_or_op "a"))),
                     [(Rule ((Pattern_ident_or_op "a"),
                         (Expr_tuple ((Expr_ident_or_op "a"),
                            (Expr_ident_or_op "a"), []))
                         ))
                       ]
                     )),
                  []))
               ))
             ]
           )))
      ] |}]
;;

let%expect_test "parse example 8 program" =
  pp2
    pp_program
    pprog
    {|
match a with 0. -> -1312004488025042530 | _ -> ("", match a with a -> a | a -> (a, a));; |};
  [%expect
    {|
    [(Str_item_eval
        (Expr_match ((Expr_ident_or_op "a"),
           (Rule ((Pattern_const (Const_float 0.)),
              (Expr_const (Const_int -1312004488025042530)))),
           [(Rule (Pattern_wild,
               (Expr_tuple ((Expr_const (Const_string "")),
                  (Expr_match ((Expr_ident_or_op "a"),
                     (Rule ((Pattern_ident_or_op "a"), (Expr_ident_or_op "a"))),
                     [(Rule ((Pattern_ident_or_op "a"),
                         (Expr_tuple ((Expr_ident_or_op "a"),
                            (Expr_ident_or_op "a"), []))
                         ))
                       ]
                     )),
                  []))
               ))
             ]
           )))
      ] |}]
;;

let%expect_test "parse example 9 program" =
  pp2
    pp_program
    pprog
    {|
let false = 3092098660336030153 and a = if if -885480591476070376<a> then u7A_S then fun eLm -> ";2MYS8)[D7[7X1t(3lL}W<D-CUbv@eV?X*QnM G|t+:O/na3";; |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive,
        (Bind ((Pattern_const (Const_bool false)),
           (Expr_const (Const_int 3092098660336030153)))),
        [(Bind ((Pattern_ident_or_op "a"),
            (Expr_ifthenelse (
               (Expr_ifthenelse (
                  (Expr_const
                     (Const_unit_of_measure
                        (Unit_of_measure ((Mnum_int -885480591476070376),
                           (Measure_ident "a"))))),
                  (Expr_ident_or_op "u7A_S"), None)),
               (Expr_lam ((Pattern_ident_or_op "eLm"),
                  (Expr_const
                     (Const_string
                        ";2MYS8)[D7[7X1t(3lL}W<D-CUbv@eV?X*QnM G|t+:O/na3"))
                  )),
               None))
            ))
          ]
        ))
      ] |}]
;;
