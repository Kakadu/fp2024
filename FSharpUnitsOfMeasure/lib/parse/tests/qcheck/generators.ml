(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open Base
open QCheck.Gen

let gen_digit = char_range '0' '9'
let gen_letter = oneof [ char_range 'a' 'z'; char_range 'A' 'Z' ]
let gen_first_ident_char = oneof [ gen_letter; return '_' ]
let gen_ident_char = oneof [ gen_letter; gen_digit; return '_'; return '\'' ]

let gen_ident =
  let* first = gen_first_ident_char in
  let* rest = string_size ~gen:gen_ident_char (int_range 0 5) in
  return (Char.to_string first ^ rest)

