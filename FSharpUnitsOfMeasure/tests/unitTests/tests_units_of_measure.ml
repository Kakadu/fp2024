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

let%expect_test "parse three measure powers in sequence as left-associated product" =
  pp pp_measure pm {| kg^3 cm^3 lb^3 |};
  [%expect
    {|
    (Measure_prod (
       (Measure_prod ((Measure_pow ((Measure_ident "kg"), 3)),
          (Measure_pow ((Measure_ident "cm"), 3)))),
       (Measure_pow ((Measure_ident "lb"), 3)))) |}]
;;

let%expect_test "parse many measure powers in sequence as product" =
  pp pp_measure pm {| kg cm^3 N^-3 v v^4 |};
  [%expect
    {|
    : end_of_input |}]
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