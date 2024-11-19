(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type identifier = string [@@deriving show { with_path = false }, qcheck]

let gen_identifier = Qcheck_utils.gen_identifier

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
[@@deriving show { with_path = false }, qcheck]

type unary_operator =
  | UnaryPlus (** + *)
  | UnaryMinus (** - *)
  | UnaryNeg (** not *)
[@@deriving show { with_path = false }, qcheck]

type rec_flag =
  | NonRec (** let a *)
  | Rec (** let rec a *)
[@@deriving show { with_path = false }, qcheck]

type literal =
  | IntLiteral of (int[@gen QCheck.Gen.int_range (-1000) 1000])
  | BoolLiteral of (bool[@gen QCheck.Gen.bool])
  | StringLiteral of
      (string
      [@gen QCheck.Gen.(string_size ~gen:Qcheck_utils.gen_string_content (0 -- 32))])
  | UnitLiteral
  | NilLiteral
[@@deriving show { with_path = false }, qcheck]

type pattern =
  | PAny (** _ *)
  | PLiteral of literal (** 123, true, "string" *)
  | PVar of identifier (** x *)
  | PCons of pattern * pattern (** p1::p2 *)
  | PTuple of
      pattern
      * pattern
      * (pattern list[@gen QCheck.Gen.(list_size (0 -- 4) (gen_pattern_sized (n / 2)))])
  (** p_1 ,..., p_n *)
[@@deriving show { with_path = false }, qcheck]

type expression =
  | ExprVariable of identifier (** x | y | z*)
  | ExprLiteral of literal (** 123 | true | "string" *)
  | ExprBinOperation of binary_operator * expression * expression (** 1 + 1 | 2 * 2 *)
  (* | ExprUnOperation of unary_operator * expression (** -x | not true *) *)
  | ExprIf of expression * expression * expression option
  (** if expr1 then expr2 else expr3 *)
  | ExprMatch of
      expression
      * (case list[@gen QCheck.Gen.(list_size (1 -- 4) (gen_case_sized (n / 2)))])
  (** match e with p_1 -> e_1 |...| p_n -> e_n *)
  | ExprLet of
      rec_flag
      * (binding list[@gen QCheck.Gen.(list_size (1 -- 4) (gen_binding_sized (n / 2)))])
      * expression (** [ExprLet(rec_flag, [(p_1, e_1) ; ... ; (p_n, e_n)], e)] *)
  | ExprApply of expression * expression (** fact n *)
  | ExprTuple of
      (expression list
      [@gen QCheck.Gen.(list_size (0 -- 4) (gen_expression_sized (n / 2)))])
  (** 1, 2, 3 *)
  | ExprCons of expression * expression (** t::tl *)
  | ExprFun of (pattern[@gen gen_pattern_sized (n / 2)]) * expression (** fun p -> e *)
  | OptNone (** None *)
  | OptSome of expression (** Some x *)
[@@deriving show { with_path = false }, qcheck]

(** Used in `match` expression *)
and case = (pattern[@gen gen_pattern_sized (n / 2)]) * expression
[@@deriving show { with_path = false }, qcheck]

(** Used in `let` expression*)
and binding = (pattern[@gen gen_pattern_sized (n / 2)]) * expression
[@@deriving show { with_path = false }, qcheck]

let gen_expression =
  QCheck.Gen.(
    let* n = small_nat in
    gen_expression_sized n)
;;

let gen_case =
  QCheck.Gen.(
    let* n = small_nat in
    gen_case_sized n)
;;

let gen_binding =
  QCheck.Gen.(
    let* n = small_nat in
    gen_binding_sized n)
;;

type structure_item =
  | SEval of expression
  | SValue of rec_flag * (binding list[@gen QCheck.Gen.(list_size (1 -- 4) gen_binding)])
  (** [SValue(rec_flag, [(p_1, e_1) ; ... ; (p_n, e_n)])] *)
[@@deriving show { with_path = false }, qcheck]

type structure =
  (structure_item list[@gen QCheck.Gen.(list_size (1 -- 2) gen_structure_item)])
[@@deriving show { with_path = false }, qcheck]
