(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility

(** Parser of integer literals: [0 .. Int64.max_int].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let integer =
  let rec helper counter =
    digit
    >>= (fun v -> helper (v + (counter * 10)))
    <|> (preturn counter >>= fun v -> preturn (IntLiteral v))
  in
  digit >>= fun d -> helper d
;;

(** Parser of boolean literals: [true], [false].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let boolean =
  ssequence "true"
  <|> ssequence "false"
  >>= fun cl -> preturn (BoolLiteral (List.length cl = 4))
;;

(** Parser of constants expression: [integer] and [boolean]

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let const_expr = integer <|> boolean >>= fun r -> preturn (Const r)
