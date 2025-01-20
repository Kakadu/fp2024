(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

(** This file contains parsers for part of F# 4.1 grammar, taken from
    https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf, page 309 *)

open Base
open Angstrom
open Ast
open Common

let pm_num_int = psint >>| fun i -> Mnum_int i
let pm_num_float = psfloat >>| fun f -> Mnum_float f
let pm_num = pm_num_int <|> pm_num_float
let pm_id = pid >>| fun id -> Measure_ident id
let pm_diml = skip_token "1" *> return Measure_dimless
let pm_atom pm = choice [ skip_token "(" *> pm <* skip_token ")"; pm_id; pm_diml ]

let pm_pow pm_atom =
  let p_expo =
    let* sign = skip_token "^" *> option "" (string "-") in
    let* num = skip_ws *> pint in
    return (if String.( = ) sign "-" then -num else num)
  in
  let* m = pm_atom in
  let* exp = option 1 p_expo in
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

let pm_pow_seq pm_p =
  let* seq = many1 (skip_ws *> pm_pow pm_p) in
  let wrap = function
    | h :: tl -> return @@ List.fold tl ~init:h ~f:(fun x y -> Measure_prod (x, y))
    | _ -> fail "Cannot parse measure sequence"
  in
  wrap seq
;;

let pm_prod = skip_token "*" *> return (fun m1 m2 -> Measure_prod (m1, m2))
let pm_div = skip_token "/" *> return (fun m1 m2 -> Measure_div (m1, m2))

let pm =
  fix (fun pm ->
    let pm = pm_pow_seq (pm_pow (pm_atom pm)) in
    (* let pm = pm_diml <|> pm in *)
    let pm = chainl pm (pm_prod <|> pm_div) <|> pm in
    skip_ws *> pm <* skip_ws_no_nl)
;;

let puom =
  let* num = skip_ws *> pm_num in
  let* measure = string "<" *> pm <* skip_ws <* string ">" in
  return (Unit_of_measure (num, measure))
;;
