(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 309 *)

open Base
open Angstrom
open Ast
open Common

let pm_num_int = parse_int >>| fun i -> Mnum_int i
let pm_num_float = parse_float >>| fun f -> Mnum_float f
let pm_num = pm_num_int <|> pm_num_float
let pm_id = parse_ident >>| fun id -> Measure_ident id

(* Power has highest priority*)
let pm_pow pm =
  let p_expo =
    let* sign = skip_token "^" *> option "" (string "-") in
    let* num = skip_ws *> parse_int in
    return (if String.( = ) sign "-" then -num else num)
  in
  let* m = pm in
  let* exp = p_expo in
  match exp with
  | 0 -> return @@ Measure_dimless
  | 1 -> return @@ m
  | -1 -> return @@ Measure_div (Measure_dimless, m)
  | p when p > 1 -> return @@ Measure_pow (m, exp)
  | n when n < -1 ->
    let exp = -exp in
    return @@ Measure_div (Measure_dimless, Measure_pow (m, exp))
  | _ -> fail "Cannot parse measure power"
;;

(* Sequence should have higher priority than product and division *)
(* I think that chainls are needed to save left associativity *)
let pm_pow_seq pm =
  let* seq = many1 (skip_ws *> pm_pow pm) in
  let wrap = function
    | h :: tl -> return @@ List.fold tl ~init:h ~f:(fun x y -> Measure_prod (x, y))
    | _ -> fail "Cannot parse measure sequence"
  in
  wrap seq
;;

let pm =
  let pm = pm_pow_seq pm_id <|> pm_id in
  skip_ws *> pm <* skip_ws
;;

let puom =
  let* num = skip_ws *> pm_num in
  let* measure = string "<" *> pm <* skip_ws <* string ">" in
  return (Unit_of_measure (num, measure))
;;
