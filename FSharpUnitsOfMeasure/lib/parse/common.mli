open Base
open Ast
open Angstrom

(** [skip_ws] skips whitespaces and tabulation *)
val skip_ws : unit t

(** [is_keyword] returns true if given string is a keyword of F# *)
val is_keyword : string -> bool

(** [is_ident_char] returns true if char can be used in a identificator *)
val is_ident_char : char -> bool

(** [is_ident_char] returns true if char can be used
    as a first char of an identificator *)
val is_ident_start_char : char -> bool

(** [parse_ident] parses identificator and returns it *)
val parse_ident : string t

(** [parse_const] parses constant and returns it *)
val parse_const : constant t
