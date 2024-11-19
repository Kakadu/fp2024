(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck.Gen

let gen_char =
  let open QCheck.Gen in
  map Char.chr (int_range (Char.code 'a') (Char.code 'h'))
;;

let gen_identifier =
  let open QCheck.Gen in
  string_size (int_range 1 8) ~gen:gen_char
;;

let gen_string_content = char_range '#' '}'
