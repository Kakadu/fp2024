(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Units_of_measure

(************************** Int and floats as measure numbers **************************)

let%expect_test "parse int as measure number int" =
  pp pp_measure_num pm_num {|1|};
  [%expect {| (Mnum_int 1) |}]
;;

let%expect_test "parse float as measure number float" =
  pp pp_measure_num pm_num {|1.0f|};
  [%expect {| (Mnum_float 1.) |}]
;;

(************************** Measure powers **************************)

let%expect_test "parse measure ident" =
  pp pp_measure pm {| kg |};
  [%expect {| (Measure_ident "kg") |}]
;;

let%expect_test "parse measure ident to the 3 power" =
  pp pp_measure pm {|kg^3|};
  [%expect {| (Measure_pow ((Measure_ident "kg"), 3)) |}]
;;

let%expect_test "parse measure ident to the negative 3 power" =
  pp pp_measure pm {|kg^-3|};
  [%expect {| (Measure_div (Measure_dimless, (Measure_pow ((Measure_ident "kg"), 3)))) |}]
;;

let%expect_test "parse measure ident to the 3 power with whitespaces between everything" =
  pp pp_measure pm {|kg      ^      -    3|};
  [%expect {| (Measure_div (Measure_dimless, (Measure_pow ((Measure_ident "kg"), 3)))) |}]
;;

(************************** Measure power sequences **************************)

let%expect_test "parse two measure powers in sequence as product" =
  pp pp_measure pm {| kg^3 cm^3 |};
  [%expect
    {|
    (Measure_prod ((Measure_pow ((Measure_ident "kg"), 3)),
       (Measure_pow ((Measure_ident "cm"), 3)))) |}]
;;

let%expect_test "parse three measure powers in sequence" =
  pp pp_measure pm {| kg^3 cm^3 lb^3 |};
  [%expect
    {|
    (Measure_prod (
       (Measure_prod ((Measure_pow ((Measure_ident "kg"), 3)),
          (Measure_pow ((Measure_ident "cm"), 3)))),
       (Measure_pow ((Measure_ident "lb"), 3)))) |}]
;;

let%expect_test "parse two measure powers and measure id in sequence " =
  pp pp_measure pm {| kg^3 cm^3 lb |};
  [%expect
    {|
    (Measure_prod (
       (Measure_prod ((Measure_pow ((Measure_ident "kg"), 3)),
          (Measure_pow ((Measure_ident "cm"), 3)))),
       (Measure_ident "lb"))) |}]
;;

let%expect_test "parse many measure powers in sequence as product" =
  pp pp_measure pm {| kg cm^3 N^-3 v v^4 |};
  [%expect
    {|
    (Measure_prod (
       (Measure_prod (
          (Measure_prod (
             (Measure_prod ((Measure_ident "kg"),
                (Measure_pow ((Measure_ident "cm"), 3)))),
             (Measure_div (Measure_dimless,
                (Measure_pow ((Measure_ident "N"), 3))))
             )),
          (Measure_ident "v"))),
       (Measure_pow ((Measure_ident "v"), 4)))) |}]
;;

(************************** Measure product **************************)

let%expect_test "parse product of two measure idents" =
  pp pp_measure pm {| kg * m |};
  [%expect {|
    (Measure_prod ((Measure_ident "kg"), (Measure_ident "m"))) |}]
;;

let%expect_test "parse product of three measure idents" =
  pp pp_measure pm {| kg * m * s |};
  [%expect
    {|
    (Measure_prod ((Measure_prod ((Measure_ident "kg"), (Measure_ident "m"))),
       (Measure_ident "s"))) |}]
;;

let%expect_test "parse product of two measure powers" =
  pp pp_measure pm {| kg^2 * m^2 |};
  [%expect
    {|
    (Measure_prod ((Measure_pow ((Measure_ident "kg"), 2)),
       (Measure_pow ((Measure_ident "m"), 2)))) |}]
;;

let%expect_test "parse product of three measure powers" =
  pp pp_measure pm {| kg^2 * m^2 * s^2 |};
  [%expect
    {|
    (Measure_prod (
       (Measure_prod ((Measure_pow ((Measure_ident "kg"), 2)),
          (Measure_pow ((Measure_ident "m"), 2)))),
       (Measure_pow ((Measure_ident "s"), 2)))) |}]
;;

let%expect_test "parse product of mixed measures" =
  pp pp_measure pm {| kg^2 * m |};
  [%expect
    {|
    (Measure_prod ((Measure_pow ((Measure_ident "kg"), 2)), (Measure_ident "m"))) |}]
;;

(************************** Measure division **************************)

let%expect_test "parse division of two measure idents" =
  pp pp_measure pm {| kg / m |};
  [%expect {|
    (Measure_div ((Measure_ident "kg"), (Measure_ident "m"))) |}]
;;

let%expect_test "parse division of three measure idents" =
  pp pp_measure pm {| kg / m / s |};
  [%expect
    {|
    (Measure_div ((Measure_div ((Measure_ident "kg"), (Measure_ident "m"))),
       (Measure_ident "s"))) |}]
;;

let%expect_test "parse division of two measure powers" =
  pp pp_measure pm {| kg^2 / m^2 |};
  [%expect
    {|
    (Measure_div ((Measure_pow ((Measure_ident "kg"), 2)),
       (Measure_pow ((Measure_ident "m"), 2)))) |}]
;;

let%expect_test "parse division of three measure powers" =
  pp pp_measure pm {| kg^2 / m^2 / s^2 |};
  [%expect
    {|
    (Measure_div (
       (Measure_div ((Measure_pow ((Measure_ident "kg"), 2)),
          (Measure_pow ((Measure_ident "m"), 2)))),
       (Measure_pow ((Measure_ident "s"), 2))))  |}]
;;

let%expect_test "parse division of mixed measures" =
  pp pp_measure pm {| kg^2 / m |};
  [%expect
    {|
    (Measure_div ((Measure_pow ((Measure_ident "kg"), 2)), (Measure_ident "m"))) |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse sequence of idents with parentheses RA" =
  pp pp_measure pm {| kg (cm s g) |};
  [%expect
    {|
    (Measure_prod ((Measure_ident "kg"),
       (Measure_prod ((Measure_prod ((Measure_ident "cm"), (Measure_ident "s"))),
          (Measure_ident "g")))
       )) |}]
;;

let%expect_test "parse sequence of idents and powers with parentheses" =
  pp pp_measure pm {| kg (cm s^2 g^-1) |};
  [%expect
    {|
    (Measure_prod ((Measure_ident "kg"),
       (Measure_prod (
          (Measure_prod ((Measure_ident "cm"),
             (Measure_pow ((Measure_ident "s"), 2)))),
          (Measure_div (Measure_dimless, (Measure_ident "g")))))
       )) |}]
;;

let%expect_test "parse sequence of idents with parentheses LA" =
  pp pp_measure pm {| (kg cm s) g |};
  [%expect
    {|
    (Measure_prod (
       (Measure_prod (
          (Measure_prod ((Measure_ident "kg"), (Measure_ident "cm"))),
          (Measure_ident "s"))),
       (Measure_ident "g"))) |}]
;;

let%expect_test "parse RA product" =
  pp pp_measure pm {| kg * (cm * s) |};
  [%expect
    {|
    (Measure_prod ((Measure_ident "kg"),
       (Measure_prod ((Measure_ident "cm"), (Measure_ident "s"))))) |}]
;;

let%expect_test "parse RA division" =
  pp pp_measure pm {| kg / (cm / s) |};
  [%expect
    {|
    (Measure_div ((Measure_ident "kg"),
       (Measure_div ((Measure_ident "cm"), (Measure_ident "s"))))) |}]
;;

(************************** Other **************************)

let%expect_test "parse 1 as measure" =
  pp pp_measure pm {| 1 |};
  [%expect
    {|
    (Measure_div ((Measure_ident "kg"),
       (Measure_div ((Measure_ident "cm"), (Measure_ident "s"))))) |}]
;;

let%expect_test "parse inverse as measure" =
  pp pp_measure pm {| 1 / m |};
  [%expect
    {|
    (Measure_div ((Measure_ident "kg"),
       (Measure_div ((Measure_ident "cm"), (Measure_ident "s"))))) |}]
;;

let%expect_test "parse something strange" =
  pp
    pp_measure
    pm
    {| ((kg * cm^3 / (s^-1 / y^2) * cm kg^2 / (g^-3 pb^2)) m^3) cm (u^3 r^-1)|};
  [%expect
    {|
    (Measure_prod (
       (Measure_prod (
          (Measure_prod (
             (Measure_div (
                (Measure_prod (
                   (Measure_div (
                      (Measure_prod ((Measure_ident "kg"),
                         (Measure_pow ((Measure_ident "cm"), 3)))),
                      (Measure_div (
                         (Measure_div (Measure_dimless, (Measure_ident "s"))),
                         (Measure_pow ((Measure_ident "y"), 2))))
                      )),
                   (Measure_prod ((Measure_ident "cm"),
                      (Measure_pow ((Measure_ident "kg"), 2))))
                   )),
                (Measure_prod (
                   (Measure_div (Measure_dimless,
                      (Measure_pow ((Measure_ident "g"), 3)))),
                   (Measure_pow ((Measure_ident "pb"), 2))))
                )),
             (Measure_pow ((Measure_ident "m"), 3)))),
          (Measure_ident "cm"))),
       (Measure_prod ((Measure_pow ((Measure_ident "u"), 3)),
          (Measure_div (Measure_dimless, (Measure_ident "r")))))
       )) |}]
;;

let%expect_test "parse something strange" =
  pp
    pp_measure
    pm
    {| (kg * cm^3 / (s^-1 / y^2))|};
  [%expect
    {|
    (Measure_div (
       (Measure_prod ((Measure_ident "kg"),
          (Measure_pow ((Measure_ident "cm"), 3)))),
       (Measure_div ((Measure_div (Measure_dimless, (Measure_ident "s"))),
          (Measure_pow ((Measure_ident "y"), 2))))
       )) |}]
;;

(************************** Units of measure **************************)

let%expect_test "parse constant with measure ident" =
  pp pp_unit_of_measure puom {|1<m>|};
  [%expect {| (Unit_of_measure ((Mnum_int 1), (Measure_ident "m"))) |}]
;;

let%expect_test "parse constant with measure to the 3 power" =
  pp pp_unit_of_measure puom {|1<m^3>|};
  [%expect {| (Unit_of_measure ((Mnum_int 1), (Measure_pow ((Measure_ident "m"), 3)))) |}]
;;
