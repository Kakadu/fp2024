(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Base

(* https://ocaml.org/manual/4.07/manual049.html *)
let is_keyword = function
  | "if"
  | "then"
  | "else"
  | "fun"
  | "let"
  | "rec"
  | "and"
  | "in"
  | "match"
  | "with"
  | "true"
  | "false"
  | "Some"
  | "None"
  | "val" -> true
  | _ -> false
;;

let skip_ws = skip_while Char.is_whitespace
let trim t = skip_ws *> t <* skip_ws
let token t = skip_ws *> string t <* skip_ws
let round_par p = token "(" *> p <* token ")"
let square_par p = token "[" *> p <* token "]"

(** Parses first letter then try parse the rest of id *)
let prs_id =
  let is_first_letter ch =
    Char.is_lowercase ch || Char.is_uppercase ch || Char.equal '_' ch
  in
  let is_rest_letter ch = Char.is_alphanum ch || Char.equal '_' ch in
  let* p_first = satisfy is_first_letter >>| Char.escaped in
  let* p_rest = take_while is_rest_letter in
  let id = p_first ^ p_rest in
  if is_keyword id then fail "Error! parse_id: id match the keyword." else return id
;;
