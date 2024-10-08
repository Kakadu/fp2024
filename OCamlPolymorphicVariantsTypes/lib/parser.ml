(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility

(** Parser of integer in range from [0] to [Int64.max_int].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let integer state =
  let rec helper counter state =
    match digit state with
    | ParseSuccess (value, new_state) -> helper (value + (counter * 10)) new_state
    | _ -> preturn (IntLiteral counter) state
  in
  match digit state with
  | ParseSuccess (value, new_state) -> helper value new_state
  | _ -> ParseFail
;;
