open Base
open Ast
open Angstrom

(** [skip_ws] skips whitespaces and tabulation *)
val skip_ws : unit t

(** [skip_ws_before p] skips whitespaces, then runs [p] and returns its result *)
val skip_ws_before : 'a t -> 'a t

(** [skip_ws_after p] runs [p], skips whitespaces after it and returns [p]'s result *)
val skip_ws_after : 'a t -> 'a t

(** [skip_ws_around p] skips whitespaces, then runs [p],
    then skips whitespaces after it and returns [p]'s result *)
val skip_ws_around : 'a t -> 'a t

(** [skip_token str] skips [str] exactly, with whitespaces around it *)
val skip_token : string -> unit t

(** [is_keyword] returns true if given string is a keyword of F# *)
val is_keyword : string -> bool

(** [is_ident_char] returns true if char can be used in an identificator *)
val is_ident_char : char -> bool

(** [is_ident_char] returns true if char can be used
    as a first char of an identificator *)
val is_ident_start_char : char -> bool

(** [parse_ident] parses identificator and returns it *)
val parse_ident : string t

(** [parse_const] parses constant and returns it *)
val parse_const : constant t
