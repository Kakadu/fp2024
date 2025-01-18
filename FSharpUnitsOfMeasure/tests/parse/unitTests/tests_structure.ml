(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Structure
open Parse.Expressions

(************************** Structure items **************************)

let%expect_test "parse structure item which is single expression" =
  pp pp_structure_item parse_structure_item {| a |};
  [%expect {| (Str_item_eval (Expr_ident_or_op "a")) |}]
;;

let%expect_test "parse structure item which is do expr" =
  pp pp_structure_item parse_structure_item {| do a |};
  [%expect {| (Str_item_eval (Expr_ident_or_op "a")) |}]
;;

let%expect_test "parse structure item which is single let binding" =
  pp pp_structure_item parse_structure_item {| let x = y |};
  [%expect
    {|
    (Str_item_def (Nonrecursive,
       (Bind ((Pattern_ident_or_op "x"), (Expr_ident_or_op "y"))), [])) |}]
;;

let%expect_test "parse structure item which is single rec let binding" =
  pp pp_structure_item parse_structure_item {| let rec x = x |};
  [%expect
    {|
    (Str_item_def (Recursive,
       (Bind ((Pattern_ident_or_op "x"), (Expr_ident_or_op "x"))), [])) |}]
;;

let%expect_test "parse structure item which is multiple let bindings" =
  pp
    pp_structure_item
    parse_structure_item
    {|let a = b
      and c = d
      and e = f
      and i = j|};
  [%expect
    {|
    (Str_item_def (Nonrecursive,
       (Bind ((Pattern_ident_or_op "a"), (Expr_ident_or_op "b"))),
       [(Bind ((Pattern_ident_or_op "c"), (Expr_ident_or_op "d")));
         (Bind ((Pattern_ident_or_op "e"), (Expr_ident_or_op "f")));
         (Bind ((Pattern_ident_or_op "i"), (Expr_ident_or_op "j")))]
       ))|}]
;;

(* Doesn't halt on 4+ nested let ... in's, the reason is in parse_expr_let *)
(* let%expect_test "parse structure item which is nested let bindings" =
  pp
    pp_structure_item
    parse_structure_item
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
    pp_structure_item
    parse_structure_item
    {|let rec factorial n =
        if (n < 2) then 1
        else n * factorial (n - 1) |};
  [%expect
    {|
    (Str_item_def (Recursive,
       (Bind ((Pattern_ident_or_op "factorial"),
          (Expr_lam ((Pattern_ident_or_op "n"),
             (Expr_ifthenelse (
                (Expr_apply (
                   (Expr_apply ((Expr_ident_or_op "<"), (Expr_ident_or_op "n"))),
                   (Expr_const (Const_int 2)))),
                (Expr_const (Const_int 1)),
                (Some (Expr_apply (
                         (Expr_apply ((Expr_ident_or_op "*"),
                            (Expr_ident_or_op "n"))),
                         (Expr_apply ((Expr_ident_or_op "factorial"),
                            (Expr_apply (
                               (Expr_apply ((Expr_ident_or_op "-"),
                                  (Expr_ident_or_op "n"))),
                               (Expr_const (Const_int 1))))
                            ))
                         )))
                ))
             ))
          )),
       []))|}]
;;

(************************** Programs **************************)

let%expect_test "parse simple binding as a program" =
  pp pp_program parse_program {| let x = y |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive,
        (Bind ((Pattern_ident_or_op "x"), (Expr_ident_or_op "y"))), []))
      ] |}]
;;

let%expect_test "parse program of bindings separated by ;;" =
  pp pp_program parse_program {|
     let x = y;; let z = 5000
     |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive,
        (Bind ((Pattern_ident_or_op "x"), (Expr_ident_or_op "y"))), []));
      (Str_item_def (Nonrecursive,
         (Bind ((Pattern_ident_or_op "z"), (Expr_const (Const_int 5000)))),
         []))
      ] |}]
;;

(* Not done yet *)
let%expect_test "parse program of bindings separated by newline" =
  pp pp_program parse_program {|
    let x = y
    let z = w
    |};
  [%expect {|
    : end_of_input |}]
;;

let%expect_test "parse example 1 program" =
  pp pp_program parse_program {| '_' [];; |};
  [%expect
    {|
    [(Str_item_eval (Expr_apply ((Expr_const (Const_char '_')), (Expr_list []))))
      ] |}]
;;

let%expect_test "parse example 2 program" =
  pp
    pp_program
    parse_program
    {| let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5>;; |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive,
        (Bind ((Pattern_ident_or_op "_Sy"), (Expr_const (Const_string "<X&D(")))),
        [(Bind ((Pattern_ident_or_op "_QQf"), (Expr_ident_or_op "tOWs")));
          (Bind (Pattern_wild,
             (Expr_apply ((Expr_ident_or_op "-"),
                (Expr_const
                   (Const_unit_of_measure
                      (Unit_of_measure ((Mnum_float 3625.090462),
                         (Measure_ident "b9o'5")))))
                ))
             ))
          ]
        ))
      ] |}]
;;

let%expect_test "parse example 3 program" =
  pp
    pp_program
    parse_program
    {| match ('X' : char) with _ -> _j531 | _O -> false | "$TNsq^]am" -> a47 | t5 -> gs | __U4' -> g | zqy'p -> true;;
 |};
  [%expect
    {|
    [(Str_item_eval
        (Expr_match (
           (Expr_typed ((Expr_const (Const_char 'X')), (Type_ident "char"))),
           (Rule (Pattern_wild, (Expr_ident_or_op "_j531"))),
           [(Rule ((Pattern_ident_or_op "_O"), (Expr_const (Const_bool false))));
             (Rule ((Pattern_const (Const_string "$TNsq^]am")),
                (Expr_ident_or_op "a47")));
             (Rule ((Pattern_ident_or_op "t5"), (Expr_ident_or_op "gs")));
             (Rule ((Pattern_ident_or_op "__U4'"), (Expr_ident_or_op "g")));
             (Rule ((Pattern_ident_or_op "zqy'p"), (Expr_const (Const_bool true))
                ))
             ]
           )))
      ] |}]
;;

let%expect_test "parse example 4 program" =
  pp
    pp_program
    parse_program
    {|
  let rec true = v and _ = q and _ = -4532424078885192588 in _p;;
  
  let 1483337398865069489 = kr and _ = _6r and _ = viwYZ and -4486306013525438741 = 3265597.524462 and _ = cDCL and l7 = false and "A4%" = -546.405974;;
  
  let 'A' = 0.000023 and lO1xQ = true and false = -3569041840172302343 and _ = -3411816405488708691 and -774524459665610219 = "ID]G<b[" and r8hu = y7O0 and aSS0b = -1580.381791 and false = 3014.442914 and eO_S8 = -0.103727<((dd_ / (((((bht * (1 * xd)) * (fkQC ^ 99)) / ((g ^ 64) / (1 ^ 5))) * (((1 ^ 37) * 1) / 1)) / a_2N)) ^ 49) ^ 57>;;
  
  let _ = '(' and j18jw = _Z6Qn and "zy@&qhIfoFgqh=&7R)\nS@E#\ned{7" = true and _ = 'Z' and _e'Jt = acp;;
  
  match let y = zdB and true = z1'13 and i = 4541.311808 in -0.069444 with _ -> _v9x8 | d -> "*.X4eW*<i,y{\n=8X$/2^^V% t3K6SigH?(WH<1'nq{WmS6.Npq:p[a5_<^H>=p;{Eq*TA!`n,!KsD7VO#D0m)GiY" | _'sH0 -> false | c -> pR8q | -2407035380919485009 -> "n=" | 2179.187086 -> false | -0.000855<xM4> -> _C;;
  
    |};
  [%expect
    {|
    [(Str_item_eval
        (Expr_let (Recursive,
           (Bind ((Pattern_const (Const_bool true)), (Expr_ident_or_op "v"))),
           [(Bind (Pattern_wild, (Expr_ident_or_op "q")));
             (Bind (Pattern_wild, (Expr_const (Const_int -4532424078885192588))))
             ],
           (Expr_ident_or_op "_p"))));
      (Str_item_def (Nonrecursive,
         (Bind ((Pattern_const (Const_int 1483337398865069489)),
            (Expr_ident_or_op "kr"))),
         [(Bind (Pattern_wild, (Expr_ident_or_op "_6r")));
           (Bind (Pattern_wild, (Expr_ident_or_op "viwYZ")));
           (Bind ((Pattern_const (Const_int -4486306013525438741)),
              (Expr_const (Const_float 3265597.52446))));
           (Bind (Pattern_wild, (Expr_ident_or_op "cDCL")));
           (Bind ((Pattern_ident_or_op "l7"), (Expr_const (Const_bool false))));
           (Bind ((Pattern_const (Const_string "A4%")),
              (Expr_const (Const_float -546.405974))))
           ]
         ));
      (Str_item_def (Nonrecursive,
         (Bind ((Pattern_const (Const_char 'A')),
            (Expr_const (Const_float 2.3e-05)))),
         [(Bind ((Pattern_ident_or_op "lO1xQ"), (Expr_const (Const_bool true))));
           (Bind ((Pattern_const (Const_bool false)),
              (Expr_const (Const_int -3569041840172302343))));
           (Bind (Pattern_wild, (Expr_const (Const_int -3411816405488708691))));
           (Bind ((Pattern_const (Const_int -774524459665610219)),
              (Expr_const (Const_string "ID]G<b["))));
           (Bind ((Pattern_ident_or_op "r8hu"), (Expr_ident_or_op "y7O0")));
           (Bind ((Pattern_ident_or_op "aSS0b"),
              (Expr_const (Const_float -1580.381791))));
           (Bind ((Pattern_const (Const_bool false)),
              (Expr_const (Const_float 3014.442914))));
           (Bind ((Pattern_ident_or_op "eO_S8"),
              (Expr_apply ((Expr_ident_or_op "-"),
                 (Expr_const
                    (Const_unit_of_measure
                       (Unit_of_measure ((Mnum_float 0.103727),
                          (Measure_pow (
                             (Measure_pow (
                                (Measure_div ((Measure_ident "dd_"),
                                   (Measure_div (
                                      (Measure_prod (
                                         (Measure_div (
                                            (Measure_prod (
                                               (Measure_prod (
                                                  (Measure_ident "bht"),
                                                  (Measure_prod (Measure_dimless,
                                                     (Measure_ident "xd")))
                                                  )),
                                               (Measure_pow (
                                                  (Measure_ident "fkQC"), 99))
                                               )),
                                            (Measure_div (
                                               (Measure_pow ((Measure_ident "g"),
                                                  64)),
                                               (Measure_pow (Measure_dimless, 5))
                                               ))
                                            )),
                                         (Measure_div (
                                            (Measure_prod (
                                               (Measure_pow (Measure_dimless, 37
                                                  )),
                                               Measure_dimless)),
                                            Measure_dimless))
                                         )),
                                      (Measure_ident "a_2N")))
                                   )),
                                49)),
                             57))
                          ))))
                 ))
              ))
           ]
         ));
      (Str_item_def (Nonrecursive,
         (Bind (Pattern_wild, (Expr_const (Const_char '(')))),
         [(Bind ((Pattern_ident_or_op "j18jw"), (Expr_ident_or_op "_Z6Qn")));
           (Bind (
              (Pattern_const (Const_string "zy@&qhIfoFgqh=&7R)\\nS@E#\\ned{7")),
              (Expr_const (Const_bool true))));
           (Bind (Pattern_wild, (Expr_const (Const_char 'Z'))));
           (Bind ((Pattern_ident_or_op "_e'Jt"), (Expr_ident_or_op "acp")))]
         ));
      (Str_item_eval
         (Expr_match (
            (Expr_let (Nonrecursive,
               (Bind ((Pattern_ident_or_op "y"), (Expr_ident_or_op "zdB"))),
               [(Bind ((Pattern_const (Const_bool true)),
                   (Expr_ident_or_op "z1'13")));
                 (Bind ((Pattern_ident_or_op "i"),
                    (Expr_const (Const_float 4541.311808))))
                 ],
               (Expr_const (Const_float -0.069444)))),
            (Rule (Pattern_wild, (Expr_ident_or_op "_v9x8"))),
            [(Rule ((Pattern_ident_or_op "d"),
                (Expr_const
                   (Const_string
                      "*.X4eW*<i,y{\\n=8X$/2^^V% t3K6SigH?(WH<1'nq{WmS6.Npq:p[a5_<^H>=p;{Eq*TA!`n,!KsD7VO#D0m)GiY"))
                ));
              (Rule ((Pattern_ident_or_op "_'sH0"),
                 (Expr_const (Const_bool false))));
              (Rule ((Pattern_ident_or_op "c"), (Expr_ident_or_op "pR8q")));
              (Rule ((Pattern_const (Const_int -2407035380919485009)),
                 (Expr_const (Const_string "n="))));
              (Rule ((Pattern_const (Const_float 2179.187086)),
                 (Expr_const (Const_bool false))));
              (Rule (
                 (Pattern_const
                    (Const_unit_of_measure
                       (Unit_of_measure ((Mnum_float -0.000855),
                          (Measure_ident "xM4"))))),
                 (Expr_ident_or_op "_C")))
              ]
            )))
      ] |}]
;;

let%expect_test "parse example 5 program" =
  pp
    pp_program
    parse_program
    {|
  let rec -2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)> = y ;;
    |};
  [%expect
    {|
    [(Str_item_def (Recursive,
        (Bind (
           (Pattern_const
              (Const_unit_of_measure
                 (Unit_of_measure ((Mnum_int -2202013031993883487),
                    (Measure_prod ((Measure_ident "o0H6t"),
                       (Measure_pow (
                          (Measure_prod (
                             (Measure_prod (Measure_dimless,
                                (Measure_pow (
                                   (Measure_div (
                                      (Measure_div (Measure_dimless,
                                         Measure_dimless)),
                                      Measure_dimless)),
                                   2))
                                )),
                             (Measure_prod (Measure_dimless,
                                (Measure_prod (Measure_dimless,
                                   (Measure_prod (
                                      (Measure_pow ((Measure_ident "rQ1_R"), 7)),
                                      Measure_dimless))
                                   ))
                                ))
                             )),
                          6))
                       ))
                    )))),
           (Expr_ident_or_op "y"))),
        []))
      ] |}]
;;

let%expect_test "parse example 6 program" =
  pp
    pp_program
    parse_program
    {|
let rec true = -4534607695307062870<((v / 1) ^ 5) * ((_L0 * m) ^ 7)>;;   |};
  [%expect
    {|
    [(Str_item_def (Recursive,
        (Bind ((Pattern_const (Const_bool true)),
           (Expr_apply ((Expr_ident_or_op "-"),
              (Expr_const
                 (Const_unit_of_measure
                    (Unit_of_measure ((Mnum_int 4534607695307062870),
                       (Measure_prod (
                          (Measure_pow (
                             (Measure_div ((Measure_ident "v"), Measure_dimless)),
                             5)),
                          (Measure_pow (
                             (Measure_prod ((Measure_ident "_L0"),
                                (Measure_ident "m"))),
                             7))
                          ))
                       ))))
              ))
           )),
        []))
      ] |}]
;;

let%expect_test "parse example 7 program" =
  pp pp_program parse_program {|
  let 1.04931758405<1 * _5q1> = x1DZ';; |};
  [%expect
    {|
    [(Str_item_def (Nonrecursive,
        (Bind (
           (Pattern_const
              (Const_unit_of_measure
                 (Unit_of_measure ((Mnum_float 1.04931758405),
                    (Measure_prod (Measure_dimless, (Measure_ident "_5q1"))))))),
           (Expr_ident_or_op "x1DZ'"))),
        []))
      ] |}]
;;
