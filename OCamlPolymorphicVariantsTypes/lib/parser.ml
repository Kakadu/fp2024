(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility

(** Parser of some elements sequence:
    - [start]: first element of sequence
    - [element_parser]: parser of one element in sequence
    - [list_converter]: coverter of elements sequence to one new element
    - [separator]: char sequence which splitted sequence elements *)
let element_sequence : 'a 'b. 'a -> 'a parser -> ('a list -> 'b) -> string -> 'b parser =
  fun start element_parser list_converter sep ->
  let next_element sep =
    skip_ws
    *> ssequence sep
    *> (element_parser
        <|> perror (Printf.sprintf "Not found elements after separator: '%s'" sep))
  in
  skip_ws
  *> (ssequence sep
      >>> many (next_element sep)
      >>= fun l -> preturn (list_converter (List.append [ start ] l)))
;;

(** Parser of integer literals: [0 .. Int64.max_int].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let integer =
  let rec helper counter =
    digit
    >>= (fun v -> helper (v + (counter * 10)))
    <|> (preturn counter >>= fun v -> preturn (IntLiteral v))
  in
  skip_ws *> digit >>= fun d -> helper d
;;

(** Parser of boolean literals: [true], [false].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let boolean =
  skip_ws *> ssequence "true"
  <|> ssequence "false"
  >>= fun cl -> preturn (BoolLiteral (List.length cl = 4))
;;

(** Parser of constants expression: [integer] and [boolean]

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let const_expr = skip_ws *> integer <|> boolean >>= fun r -> preturn (Const r)

(** Parser of all expression which defines on [Miniml.Ast] module *)
let rec expr state = basic_expr state

(** Parser of basic expressions: [<unary>] | [<const>] | [<tuple>] | [<block>] *)
and basic_expr state = (skip_ws *> unary_expr <|> bracket_expr <|> const_expr) state

(** Parser of unary expression *)
and unary_expr state =
  let helper =
    symbol '+' *> basic_expr
    <|> (symbol '-' *> skip_ws *> basic_expr >>= fun e -> preturn (Unary (Negate, e)))
  in
  (skip_ws *> (helper <|> symbol '~' *> helper)) state

(** Parser of expression sequence:
    - [start]: first element of sequence
    - [converter]: coverter of elements sequence to one new element
    - [separator]: char sequence which splitted sequence elements *)
and expression_sequence start converter separator =
  element_sequence start expr converter separator

(** Parser of brackets expression:
    - unit: [()]
    - one expression: [(<expr>)]
    - expression block: [(<expr>; ...; <expr>)]
    - tuple: [(<expr>, ..., <expr>)] *)
and bracket_expr state =
  let expr_block ex =
    skip_ws *> expression_sequence ex (fun l -> ExpressionBlock l) ";"
  in
  let tuple_expr ex = skip_ws *> expression_sequence ex (fun l -> Tuple l) "," in
  let brackets_subexpr =
    skip_ws *> expr
    >>= (fun ex ->
          expr_block ex
          <|> tuple_expr ex
          <|> (skip_ws *> symbol ')' >>> preturn ex)
          <|> perror "Unsupported separator of bracket expression")
    <|> preturn (Const UnitLiteral)
  in
  (skip_ws
   *> (symbol '(' *> brackets_subexpr
       <* (skip_ws *> symbol ')' <|> perror "Not found close bracket")))
    state
;;
