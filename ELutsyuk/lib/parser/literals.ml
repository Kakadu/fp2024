(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom
open Auxiliaries
open Ast

let parse_int =
  trim
  @@
  let* sign = choice [ char '+' *> return 1; char '-' *> return (-1); return 1 ] in
  let* digit = take_while1 is_digit >>| int_of_string in
  return @@ Int (sign * digit)
;;

let parse_str =
  let parse_empty_string = string "{||}" >>| fun _ -> "" in
  let parse_content = string "{|" *> take_till (Char.equal '|') <* string "|}" in
  let* str = parse_empty_string <|> parse_content in
  return @@ Str str
;;

let parse_bool =
  let* bool =
    choice [ trim @@ (string "true" *> return true); token "false" *> return false ]
  in
  skip_separators *> (return @@ Bool bool)
;;

let parse_unit =
  let* _ = token "()" in
  return Unit
;;

let parse_lit = choice [ parse_int; parse_str; parse_bool; parse_unit ]
