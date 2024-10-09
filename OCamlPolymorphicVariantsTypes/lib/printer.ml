(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility

(** Convert custom parse result to string view *)
let string_of_parse_result converter = function
  | ParseFail -> "Parse process failed"
  | ParseError (msg, state) ->
    Printf.sprintf "ParseError(line=%d pos=%d): %s" state.line state.inline msg
  | ParseSuccess (r, _) -> converter r
;;

(** Convert [literal] parse result to string by function [show_literal] *)
let string_of_literal_parse_result = string_of_parse_result show_literal

(** Convert [expression] parse result to string by function [show_expression] *)
let string_of_expression_parse_result = string_of_parse_result show_expression

(** Convert [struct_item] parse result to string by function [show_struct_item] *)
let string_of_struct_item_parse_result = string_of_parse_result show_struct_item
