(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 309 *)

open Base
open Angstrom
open Ast
open Common

let parse_measure_num_int = parse_int >>| fun i -> Mnum_int i
let parse_measure_num_float = parse_float >>| fun f -> Mnum_float f
let parse_measure_num = parse_measure_num_int <|> parse_measure_num_float
let parse_measure_ident = parse_ident >>| fun id -> Measure_ident id

let parse_exp =
  let* sign = option "" (string "-") in
  let* num = skip_ws *> parse_int in
  match sign with
  | "-" -> return (Int_exp_neg num)
  | _ -> return (Int_exp_pos num)
;;

let parse_measure_power parse_measure =
  let* measure = parse_measure in
  let* exp = option (Int_exp_pos 1) (skip_token "^" *> parse_exp) in
  return (Measure_pow (measure, exp))
;;

(* Sequence should have higher priority than product and division*)
(* I think that chainls are needed to save left associativity *)
let parse_measure_power_seq parse_measure =
  let* seq = many1 (skip_ws *> parse_measure_power parse_measure) in
  let wrap = function
    | h :: tl -> return @@ List.fold tl ~init:h ~f:(fun x y -> Measure_prod (x, y))
    | _ -> fail "Cannot parse measure sequence"
  in
  wrap seq
;;

let parse_measure =
  let parse_measure =
    parse_measure_power_seq parse_measure_ident <|> parse_measure_ident
  in
  skip_ws *> parse_measure <* skip_ws
;;

let parse_unit_of_measure =
  let* num = skip_ws *> parse_measure_num in
  let* measure = string "<" *> parse_measure_ident <* skip_ws <* string ">" in
  return (Unit_of_measure (num, measure))
;;
