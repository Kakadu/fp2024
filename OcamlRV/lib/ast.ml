(** Copyright 2024-2025, Viacheslav Sidorov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* [a-zA-Z_0-9] *)
type identifier = string [@@deriving show { with_path = false }]

type binary_operator =
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
  | Lt (* < *)
  | Gt (* > *)
  | Eq (* = *)
  | Neq (* <> *)
  | Lte (* <= *)
  | Gte (* >= *)
  | And (* && *)
  | Or (* || *)
[@@deriving show { with_path = false }]

type unary_operator =
  | UnPlus (* + *)
  | UnMinus (* - *)
  | UnNeg (* not *)
[@@deriving show { with_path = false }]

type rec_flag =
  | NonRec (* let a *)
  | Rec (* let rec a *)
[@@deriving show { with_path = false }]

type placement =
  | Global
  | Inline of identifier
[@@deriving show { with_path = false }]

type literal =
  | IntLiteral of int (* 123 *)
  | BoolLiteral of bool (* true | false *)
  | StringLiteral of string (* "string" *)
  | UnitLiteral (* () value *)
[@@deriving show { with_path = false }]

type pattern =
  | PAny (* _ *)
  | PLiteral of literal (* 123, true, "string" *)
  | PVar of identifier (* x *)
  | PTuple of pattern list (* p_1 ,..., p_n *)
  | PCons of pattern * pattern (* p1::p2 *)
[@@deriving show { with_path = false }]

type expression =
  | ExprVariable of identifier (* x | y | z*)
  | ExprLiteral of literal (* 123 | true | "string" *)
  | ExprBinOperation of binary_operator * expression * expression (* 1 + 1 | 2 * 2 *)
  | ExprUnOperation of unary_operator * expression (* -x | not true *)
  | ExprIf of expression * expression * expression (* if expr1 then expr2 else expr3 *)
  | ExprLet of rec_flag * binding * expression (* let x = expr1 in expr2 *)
  | ExprApply of identifier * expression list (* fact n *)
  | ExprTuple of expression list (* 1, 2, 3 *)
  | ExprDefinition of
      placement
      * rec_flag
      * identifier
      * identifier list
      * expression list (* let [rec] f = (..) *)
[@@deriving show { with_path = false }]

(* Used in `let` expr *)
and binding = pattern * expression [@@deriving show { with_path = false }]

(* Used in `match` expr *)
type case = pattern * expression [@@deriving show { with_path = false }]
type program = expression list [@@deriving show { with_path = false }]
