(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
open Pp
open Parse.Measure

(************************** Measure numbers **************************)

let%expect_test "parse int as measure number int" =
  pp pp_measure_num parse_measure_num {|1|};
  [%expect {| (Mnum_int 1) |}]
;;

let%expect_test "parse float as measure number float" =
  pp pp_measure_num parse_measure_num {|1.0f|};
  [%expect {| (Mnum_float 1.) |}]
;;
