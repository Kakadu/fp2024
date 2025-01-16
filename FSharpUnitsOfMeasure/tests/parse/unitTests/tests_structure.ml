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
let rec j7Z = _lgc and _ = bp28 and _s = b4W and true = 4534607695307062870<((v / 1) ^ 5) * ((_L0 * m) ^ 7)> and
-2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)> = y and
_ = _H and -0.083825<1> = _'0S6 and _04 = z7_L and _ = n and 0.001366 = false and fWY8C = "" and _ = j7yI2
and false = j68wD and _ = '3' and b = "y*9gBW" and bxN_ = true and _ = m_ and true = wf1zy and c9f = jr26b
and zGI = "" and _ = uCD1 and _ = sNV_R and x4 = j and -0.000001<1> = "=p\\)*wor" and bo_j = aeY and -628.886928 = u
and fL = "7*kVB>W{8).$Zlkk{\"\\]r7$hUhwK7CV" and false = _y42 and 3089291205124737905 = -33544.593186 and _ = _0TJ
and _ = iOx1 and _Kk = q__ and _ = -2574877848667227382<f47> and tb74 = ' ' and _ = "?Jf" and _ = rO2 and cjZ = zKpY5
and -3754946449163551734<bK / _''rM> = true and pX25D = _q0n and j5 = c27 and a5 = zx and o = "f[{`5CH" and
_ = "#.f" and n478 = d19j8 and nG = true and s = -0.000400 and 32.542220<(k / (o_' * k9X'C)) ^ 45> = h and
j'_ = -2297698304594498760<1> and true = ncXiZ and xX4 = _dztV and _ = wX and false = k2'6 and e92' = 54.557530
and 67061.541791<1> = "" and "Dz\nFN{g]=Xq/!\n+vrk\\ta+EK$" = i885 and _Ww = y'lp6 and _Xn06 = f and oZ_ = s02K9 and false = k and -6.020625 = 0.000000 and dk = ir8 and -342.854030 = aO and _ = o and _ = -106.986437 and yE3V = -0.000010 and nn = _66x and -0.001635 = '|' and j_ld = 1165660.320127<e95D' * (1 ^ 69)> and qbjv0 = 2648399603976260398 and i'''9 = u1B2 and -0.000001 = b8 and -4269602629199018086 = -1204493915358000121<1> and gh = false and 'G' = _7UkD and _ = d and _ = "IUyO=bCX<w[ZhNlFn%{W(d@Kpo2UAcaO]C<W,AObMG/eeZ" and -0.000022<((f0 / (uRDH ^ 7)) * (xA / 1)) ^ 4> = -0.000008 and _ = q'vi and _ = 2214994724711011379 and true = _PS and _ = "" and j = -4388646751400294290 and lSOp = -3853063768692514483<1 * ((su'9I / 1) * (_w742 * 1))>;;

let 0.000002 = ltTI and _'96 = false and _ = p9'1 and -535751137833501303<(xFY28 / (1 * (oE * (1 / qm)))) * 1> = true and -2210233641572818975 = sE2K' and _ = m and "b,Z" = a's and _ = 'N' and '(' = '\'' and true = n and mb = 0.000012 and _ = -2961933386370371785<(t7JO * (((1 / (w ^ 29)) * (((1 / 1) / 1) / ((1 ^ 97) ^ 6))) ^ 98)) / 1> and _ = _'3 and g'Q = -416.119397 and _qmD = '5' and _ = false and _ = false and 106510101046464266 = jvo_9 and jPgN1 = -1284479997276135239<xE> and -2534764107692139831 = l and _ = huE and -59168.801573<1> = _2V and _ = c and l = 1792296140003349788<1> and false = 373.351120 and 'D' = 2043477.055770 and _ = _I2 and h6N40 = -3815672301510237652<(gL ^ 2) * (((((_'P_ / (1 * ((1 ^ 2) / ((x / 1) / 1)))) / 1) ^ 7) * (1 * (v / 1))) * ((((1 ^ 2) ^ 4) ^ 7) ^ 9))> and 1658810944614430274<q5O> = y'a_ and qzYqU = l_Y'3 and ai_ = _AE8 and _ = 1809101836127695682 and _ = 0.000346 and fd01 = '4' and -0.902130 = c and _ = p7 and -3039871785343240407 = _mS9J and false = pK and 3347495553617970679<1 ^ 78> = y1ck and eTaD = w2h6V and _ = 109043.949010<(((1 / 1) * __Sc) ^ 33) / ((u0E ^ 7) ^ 32)> and _ = _zg2 and _ = _d2Ki and 'q' = "_xtT " and 616380868499048877 = '5' and -954074232540884350 = 714253.922174<k_Kj5> and _ = og_1 and "Rvpqm" = o7F3i and 431640995748256716<_MXJm> = bb and _ = -3812048725744478250 and _ = k and h_B3R = true and -0.034807 = o0 and false = 'G' and true = 2286448748575322655 and _ = false and 'd' = p9 and c = ";cW,zhEa}" and kgL1 = n and d = -0.000005<(((_lp / _M6) * (aPHn ^ 27)) * ((g4qN * b3) / (1 ^ 8))) * (vme4 / 1)> and iD = 1051350575329771926 and _ = jK_uY and 'm' = _P7 and false = -3111917588957775025 and g = "Xc1&$@<" and r'yJH = hU1W8 and r427 = 1470193727283571991 and rtT = p and 0.000092<(y7Ws ^ 5) * (_p0hj * (1 / 1))> = 0.000010 and _ = a and _ = "_'#/4" and true = o0_ and 0.112045 = p8QH and true = 0.006023 and _ = 'r' and _ = "R_ex" and -0.000000 = vdJk and lXM = tckK_ and o9 = _A'4 and sb3 = sN and 757633770569328753 = ".>!j!VKG\nYM`p=o|W*e?O5K+|8<ia>[=nlUek7k5-\n$q1D%.M>__Hx&*2=In1'gu{@A^m\"9[$" and i'E = n and _ = 3885290108641647689 and _7X = true and _ = p2Ml and d = -3038253807373362651 and hPQ = z' and -4605880381525804245 = z58L1 and wHA6 = mA6Wt and -1771723548821597230<((1 / (((fxKW8 / kVM_) ^ 24) / (_LHu / _o2w))) / 1) * d> = zaPow and _ = _MQ and 4471037639686707367<ou9_ ^ 4> = yoYL and _ = _3yxl;;
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

let%expect_test "parse example 6.1 program" =
  pp
    pp_program
    parse_program
    {|
let rec true = 4534607695307062870<((v / 1) ^ 5) * ((_L0 * m) ^ 7)>;;   |};
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
