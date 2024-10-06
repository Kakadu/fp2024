(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type identifier = string [@@deriving show { with_path = false }]

type binary_operator =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Lt (** < *)
  | Gt (** > *)
  | Eq (** = *)
  | Neq (** <> *)
  | Lte (** <= *)
  | Gte (** >= *)
  | And (** && *)
  | Or (** || *)
[@@deriving show { with_path = false }]

type unary_operator =
  | UnaryPlus (** + *)
  | UnaryMinus (** - *)
  | UnaryNeg (** not *)
[@@deriving show { with_path = false }]

type rec_flag =
  | NonRec (** let a *)
  | Rec (** let rec a *)
[@@deriving show { with_path = false }]

type literal =
  | IntLiteral of int (** 123 *)
  | BoolLiteral of bool (** true | false *)
  | StringLiteral of string (** "string" *)
  | UnitLiteral (** () *)
  | NilLiteral (** [] *)
[@@deriving show { with_path = false }]

type pattern =
  | PAny (** _ *)
  | PLiteral of literal (** 123, true, "string" *)
  | PVar of identifier (** x *)
  | PTuple of pattern list (** p_1 ,..., p_n *)
  | PCons of pattern * pattern (** p1::p2 *)
  | PPoly of identifier * pattern option (** `Tag p *)
[@@deriving show { with_path = false }]

type expression =
  | ExprVariable of identifier (** x | y | z*)
  | ExprLiteral of literal (** 123 | true | "string" *)
  | ExprBinOperation of binary_operator * expression * expression (** 1 + 1 | 2 * 2 *)
  | ExprUnOperation of unary_operator * expression (** -x | not true *)
  | ExprIf of expression * expression * expression option
  (** if expr1 then expr2 else expr3 *)
  | ExprMatch of expression * case list (** match e with p_1 -> e_1 |...| p_n -> e_n *)
  | ExprLet of rec_flag * binding list * expression
  (** [ExprLet(rec_flag, [(p_1, e_1) ; ... ; (p_n, e_n)], e)] *)
  | ExprApply of expression * expression (** fact n *)
  | ExprTuple of expression list (** 1, 2, 3 *)
  | ExprCons of expression * expression (** t::tl *)
  | ExprPoly of identifier * expression option (** `Tag expr *)
  | ExprFun of pattern * pattern list * expression (** fun p -> e *)
[@@deriving show { with_path = false }]

(** Used in `match` expression *)
and case = pattern * expression [@@deriving show { with_path = false }]

(** Used in `let` expression*)
and binding = pattern * expression [@@deriving show { with_path = false }]

type structure_item =
  | SEval of expression
  | SValue of rec_flag * binding list
  (** [SValue(rec_flag, [(p_1, e_1) ; ... ; (p_n, e_n)])] *)
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]
