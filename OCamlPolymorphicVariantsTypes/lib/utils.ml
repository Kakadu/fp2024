(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Converter digit character value to integer value in range from [0] to [9] *)
let int_of_digit_char symbol = int_of_char symbol - int_of_char '0'

(** Converter string value to list of characters *)
let char_list_of_string s = List.of_seq (String.to_seq s)

(** Converter list of characters value to string *)
let string_of_char_list char_list = String.of_seq (List.to_seq char_list)

let is_empty = function
  | [] -> true
  | _ -> false
;;
