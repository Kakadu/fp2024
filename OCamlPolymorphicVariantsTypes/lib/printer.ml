(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser_utility

(** Convert custom parse result to string view *)
let string_of_parse_result converter = function
  | ParseFail -> "Parse process failed"
  | ParseError (msg, state) ->
    Printf.sprintf "ParseError(line=%d pos=%d): %s" state.line state.inline msg
  | ParseSuccess (r, _) -> converter r
;;
