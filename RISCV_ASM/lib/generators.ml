(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck.Gen

let gen_alpha =
  oneof
    [ map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
    ; map Char.chr (int_range (Char.code 'A') (Char.code 'Z'))
    ; return '.'
    ]
;;

let gen_alpha_or_digit =
  oneof
    [ map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
    ; map Char.chr (int_range (Char.code 'A') (Char.code 'Z'))
    ; map Char.chr (int_range (Char.code '0') (Char.code '9'))
    ]
;;

let gen_my_label =
  let* length = int_range 2 50 in
  let* first_char = gen_alpha in
  let* rest_chars = list_repeat (length - 1) gen_alpha_or_digit in
  let all_chars = first_char :: rest_chars in
  return (String.concat "" (List.map (String.make 1) all_chars))
;;

let gen_char_for_string =
  oneof
    [ map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
    ; map Char.chr (int_range (Char.code 'A') (Char.code 'Z'))
    ; map Char.chr (int_range (Char.code '0') (Char.code '9'))
    ; return '_'
    ; return ':'
    ; return '('
    ; return ')'
    ; return '.'
    ]
;;

let gen_my_string =
  let* length = int_range 1 50 in
  let* chars = list_repeat length gen_char_for_string in
  return (String.concat "" (List.map (String.make 1) chars))
;;
