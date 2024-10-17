(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type parser_state =
  { input : char list
  ; line : int
  ; inline : int
  }
[@@deriving show { with_path = false }]

type 'a parse_result =
  | ParseSuccess of 'a * parser_state
  | ParseError of string * parser_state
  | ParseFail
[@@deriving show { with_path = false }]

type 'a parser = parser_state -> 'a parse_result [@@deriving show { with_path = false }]

val pfail : 'a parser
val perror : string -> 'a parser
val preturn : 'a -> 'a parser
val ( <*> ) : 'a 'b. ('a -> 'b) parser -> 'a parser -> 'b parser
val ( >>= ) : 'a 'b. 'a parser -> ('a -> 'b parser) -> 'b parser
val ( >>| ) : 'a 'b. 'a parser -> ('a -> 'b) -> 'b parser
val ( *> ) : 'a 'b. 'a parser -> 'b parser -> 'b parser
val ( <* ) : 'a 'b. 'a parser -> 'b parser -> 'a parser
val many : 'a parser -> 'a list parser
val many1 : 'a parser -> 'a list parser
val ( <|> ) : 'a parser -> 'a parser -> 'a parser
val one_of : 'a parser list -> 'a parser

type char_predicate = char -> bool [@@deriving show { with_path = false }]

val is_whitespace : char_predicate
val is_digit : char_predicate
val is_ascci_letter : char_predicate

type state_changer = char -> parser_state -> parser_state
[@@deriving show { with_path = false }]

val default_schanger : state_changer

val satisfy
  :  char_predicate
  -> (char -> 'a)
  -> state_changer option
  -> parser_state
  -> 'a parse_result

val dsatisfy : char_predicate -> (char -> 'a) -> parser_state -> 'a parse_result
val asatisfy : (char -> bool) -> parser_state -> unit parse_result
val symbol : char -> char parser
val sequence : char list -> char list parser
val ssequence : string -> char list parser
val skip_ws : unit parser
val digit : parser_state -> int parse_result
val asequence : char list -> unit parser
val assequence : string -> unit parser
val parse : 'a parser -> string -> 'a parse_result
