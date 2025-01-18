(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type identifier = string [@@deriving show { with_path = false }]

let gen_char =
  let open QCheck.Gen in
  map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
;;

let gen_identifier =
  let open QCheck.Gen in
  string_size (int_range 1 8) ~gen:gen_char
;;

let gen_string =
  let open QCheck.Gen in
  string_size (int_range 0 32) ~gen:gen_char
;;

let div = 15

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

type constant =
  | CInt of (int[@gen QCheck.Gen.int_range 0 1000])
  | CBool of bool
  | CString of (string[@gen gen_string])
  | CUnit
  | CNil
[@@deriving show { with_path = false }, qcheck]

type fresh = int [@@deriving show { with_path = false }]

type type_annot =
  | AInt
  | ABool
  | AString
  | AUnit
  | AVar of (fresh[@gen QCheck.Gen.int_range 0 1000])
  | AFun of type_annot * type_annot
  | AList of type_annot
  | ATuple of type_annot list
  | AOption of type_annot
[@@deriving show { with_path = false }, qcheck]

type pattern =
  | PAny (** _ *)
  | PConstant of constant (** 123, true, "string" *)
  | PVar of identifier (** x *)
  | PCons of pattern * pattern (** p1::p2 *)
  | PTuple of
      pattern
      * pattern
      * (pattern list
        [@gen QCheck.Gen.(list_size small_nat (gen_pattern_sized (n / div)))])
  (** p_1 ,..., p_n *)
  | PList of
      pattern
      * (pattern list
        [@gen QCheck.Gen.(list_size small_nat (gen_pattern_sized (n / div)))])
  | POption of pattern option
  | PType of pattern * type_annot
[@@deriving show { with_path = false }, qcheck]

type expression =
  | ExprVariable of identifier (** x | y | z*)
  | ExprConstant of constant (** 123 | true | "string" *)
  | ExprBinOperation of binary_operator * expression * expression (** 1 + 1 | 2 * 2 *)
  | ExprUnOperation of unary_operator * expression (** -x | not true *)
  | ExprIf of expression * expression * expression option
  (** if expr1 then expr2 else expr3 *)
  | ExprMatch of
      expression
      * case
      * (case list[@gen QCheck.Gen.(list_size small_nat (gen_case_sized (n / div)))])
  | ExprFunction of
      case * (case list[@gen QCheck.Gen.(list_size small_nat (gen_case_sized (n / div)))])
  (** match e with p_1 -> e_1 |...| p_n -> e_n *)
  | ExprLet of
      rec_flag
      * binding
      * (binding list[@gen QCheck.Gen.(list_size (0 -- 4) (gen_binding_sized (n / div)))])
      * expression
  (** [ExprLet(rec_flag, (p_1, e_1), [(p_2, e_2) ; ... ; (p_n, e_n)], e)] *)
  | ExprApply of expression * expression (** fact n *)
  | ExprTuple of
      expression
      * expression
      * (expression list
        [@gen QCheck.Gen.(list_size small_nat (gen_expression_sized (n / div)))])
  (** 1, 2, 3 *)
  | ExprList of
      expression
      * (expression list
        [@gen QCheck.Gen.(list_size small_nat (gen_expression_sized (n / div)))])
  | ExprCons of expression * expression (** t::tl *)
  | ExprFun of (pattern[@gen gen_pattern_sized (n / div)]) * expression (** fun p -> e *)
  | ExprOption of expression option
  | ExprType of expression * type_annot
[@@deriving show { with_path = false }, qcheck]

(** Used in `match` expression *)
and case = (pattern[@gen gen_pattern_sized (n / div)]) * expression
[@@deriving show { with_path = false }, qcheck]

(** Used in `let` expression*)
and binding = (pattern[@gen gen_pattern_sized (n / div)]) * expression
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
  | SValue of
      rec_flag
      * binding
      * (binding list[@gen QCheck.Gen.(list_size (0 -- 2) gen_binding)])
  (** [SValue(rec_flag, (p_1, e_1), [(p_2, e_2) ; ... ; (p_n, e_n)])] *)
[@@deriving show { with_path = false }, qcheck]

type structure =
  (structure_item list[@gen QCheck.Gen.(list_size (1 -- 2) gen_structure_item)])
[@@deriving show { with_path = false }, qcheck]
