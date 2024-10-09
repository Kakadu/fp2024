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
  skip_ws >>= fun _ -> digit >>= fun d -> helper d
;;

(** Parser of boolean literals: [true], [false].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let boolean =
  skip_ws
  >>= fun _ ->
  ssequence "true"
  <|> ssequence "false"
  >>= fun cl -> preturn (BoolLiteral (List.length cl = 4))
;;

(** Parser of constants expression: [integer] and [boolean]

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let const_expr = skip_ws >>= fun _ -> integer <|> boolean >>= fun r -> preturn (Const r)

(** Parser of basic expressions: [<unary>] | [<const>] | [<tuple>] | [<block>] *)
let rec basic_expr state = (skip_ws >>= fun _ -> unary_expr <|> const_expr) state

(** Parser of unary expression *)
and unary_expr state =
  let helper =
    symbol '+' *> basic_expr
    <|> (symbol '-' *> basic_expr >>= fun e -> preturn (Unary (Negate, e)))
  in
  (skip_ws >>= fun _ -> helper <|> symbol '~' *> helper) state
;;
