(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Pprint.Pp
open Parse.Units_of_measure
open Pprint.Pprinter

let run = pp pprint_measure pm
let _ = run
(************************** Measure powers **************************)

let%expect_test "parse measure ident" =
  run {| kg |};
  [%expect {| kg |}]
;;

let%expect_test "parse measure ident to the 3 power" =
  run {|kg^3|};
  [%expect {| kg ^ 3 |}]
;;

let%expect_test "parse measure ident to the negative 3 power" =
  run {|kg^-3|};
  [%expect {| 1 / (kg ^ 3) |}]
;;

let%expect_test "parse measure ident to the 3 power with whitespaces between everything" =
  run {|kg      ^      -    3|};
  [%expect {| 1 / (kg ^ 3) |}]
;;

(************************** Measure power sequences **************************)

let%expect_test "parse two measure powers in sequence as product" =
  run {| kg^3 cm^3 |};
  [%expect {|
    (kg ^ 3) * (cm ^ 3) |}]
;;

let%expect_test "parse three measure powers in sequence" =
  run {| kg^3 cm^3 lb^3 |};
  [%expect {|
    ((kg ^ 3) * (cm ^ 3)) * (lb ^ 3) |}]
;;

let%expect_test "parse two measure powers and measure id in sequence " =
  run {| kg^3 cm^3 lb |};
  [%expect {|
    ((kg ^ 3) * (cm ^ 3)) * lb |}]
;;

let%expect_test "parse many measure powers in sequence as product" =
  run {| kg cm^3 N^-3 v v^4 |};
  [%expect {|
    (((kg * (cm ^ 3)) * (1 / (N ^ 3))) * v) * (v ^ 4) |}]
;;

(************************** Measure product **************************)

let%expect_test "parse product of two measure idents" =
  run {| kg * m |};
  [%expect {|
    kg * m |}]
;;

let%expect_test "parse product of three measure idents" =
  run {| kg * m * s |};
  [%expect {|
    (kg * m) * s |}]
;;

let%expect_test "parse product of two measure powers" =
  run {| kg^2 * m^2 |};
  [%expect {|
    (kg ^ 2) * (m ^ 2) |}]
;;

let%expect_test "parse product of three measure powers" =
  run {| kg^2 * m^2 * s^2 |};
  [%expect {|
    ((kg ^ 2) * (m ^ 2)) * (s ^ 2) |}]
;;

let%expect_test "parse product of mixed measures" =
  run {| kg^2 * m |};
  [%expect {|
    (kg ^ 2) * m |}]
;;

(************************** Measure division **************************)

let%expect_test "parse division of two measure idents" =
  run {| kg / m |};
  [%expect {|
    kg / m |}]
;;

let%expect_test "parse division of three measure idents" =
  run {| kg / m / s |};
  [%expect {|
    (kg / m) / s |}]
;;

let%expect_test "parse division of two measure powers" =
  run {| kg^2 / m^2 |};
  [%expect {|
    (kg ^ 2) / (m ^ 2) |}]
;;

let%expect_test "parse division of three measure powers" =
  run {| kg^2 / m^2 / s^2 |};
  [%expect {|
    ((kg ^ 2) / (m ^ 2)) / (s ^ 2)  |}]
;;

let%expect_test "parse division of mixed measures" =
  run {| kg^2 / m |};
  [%expect {|
    (kg ^ 2) / m |}]
;;

(************************** Parentheses **************************)

let%expect_test "parse sequence of idents with parentheses RA" =
  run {| kg (cm s g) |};
  [%expect {|
    kg * ((cm * s) * g) |}]
;;

let%expect_test "parse sequence of idents and powers with parentheses" =
  run {| kg (cm s^2 g^-1) |};
  [%expect {|
    kg * ((cm * (s ^ 2)) * (1 / g)) |}]
;;

let%expect_test "parse sequence of idents with parentheses LA" =
  run {| (kg cm s) g |};
  [%expect {|
    ((kg * cm) * s) * g |}]
;;

let%expect_test "parse RA product" =
  run {| kg * (cm * s) |};
  [%expect {|
    kg * (cm * s) |}]
;;

let%expect_test "parse RA division" =
  run {| kg / (cm / s) |};
  [%expect {|
    kg / (cm / s) |}]
;;

(************************** Other **************************)

let%expect_test "parse 1 as measure" =
  run {| 1 |};
  [%expect {|
    1 |}]
;;

let%expect_test "parse inverse as measure" =
  run {| 1 / m |};
  [%expect {|
    1 / m |}]
;;

let%expect_test "parse something strange" =
  pp
    pprint_measure
    pm
    {| ((kg * cm^3 / (s^-1 / y^2) * cm kg^2 / (g^-3 pb^2)) m^3) cm (u^3 r^-1)|};
  [%expect
    {|
    ((((((kg * (cm ^ 3)) / ((1 / s) / (y ^ 2))) * (cm * (kg ^ 2))) / ((1 / (g ^ 3)) * (pb ^ 2))) * (m ^ 3)) * cm) * ((u ^ 3) * (1 / r)) |}]
;;

let%expect_test "parse something strange" =
  run {| (kg * cm^3 / (s^-1 / y^2))|};
  [%expect {|
    (kg * (cm ^ 3)) / ((1 / s) / (y ^ 2)) |}]
;;

(************************** Units of measure **************************)

let%expect_test "parse constant with measure ident" =
  pp pprint_uom puom {|1<m>|};
  [%expect {| 1<m> |}]
;;

let%expect_test "parse constant with measure to the 3 power" =
  pp pprint_uom puom {|1<m^3>|};
  [%expect {| 1<m ^ 3> |}]
;;

let%expect_test "parse uom with strange name" =
  pp pprint_uom puom {|1<b9o'5>|};
  [%expect {| 1<b9o'5> |}]
;;

let%expect_test "parse uom 1" =
  pp pprint_uom puom {|421088.397651<(((bWY ^ 2) * 1) * (1 ^ 4))>|};
  [%expect {|
    421088.397651<((bWY ^ 2) * 1) * (1 ^ 4)> |}]
;;

let%expect_test "parse uom 1" =
  pp
    pprint_uom
    puom
    {|
  -2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)>|};
  [%expect
    {|
    -2202013031993883487<o0H6t * (((1 * (((1 / 1) / 1) ^ 2)) * (1 * (1 * ((rQ1_R ^ 7) * 1)))) ^ 6)> |}]
;;
