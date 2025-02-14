(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = Id of string (** identifier *)

val show_id : id -> string

type const =
  | Int of int (** integer constant, e.g. 1 *)
  | String of string (** string constant, e.g. "hello" *)
  | Bool of bool (** boolean constant, e.g. true *)
  | Unit (** unit constant, e.g. () *)

(** language constants of type int, string, bool, and unit respectively *)
val show_const : const -> string

type bin_op =
  | Add (** addition of two ints *)
  | Mult (** multiplication of two ints *)
  | Sub (** subtraction of two ints *)
  | Div (** division of two ints *)
  | Gt (** greater than *)
  | Lt (** less than *)
  | Eq (** equal *)
  | Neq (** not equal *)
  | Gte (** greater than or equal *)
  | Lte (** less than or equal *)
  | And (** logical AND *)
  | Or (** logical OR *)
  | Cons (** :: *)

val show_bin_op : bin_op -> string

type un_op =
  | Negative (** arithmetic unary - *)
  | Positive (** arithmetic unary + *)
  | Not (** logical not *)

val show_un_op : un_op -> string

(** flag for let expressions *)
type rec_flag =
  | Recursive (** rec value *)
  | Non_recursive (** plain value *)

val show_rec_flag : rec_flag -> string

type pattern =
  | PVar of id (** variable pattern, e.g. x *)
  | PConst of const (** pattern of a constant *)
  | PTuple of pattern * pattern * pattern list (** patterns (P0, .., Pn), n >= 2 *)
  | PAny (** wildcard pattern '_' *)
  | PList of pattern list (** patterns [P0; ..; Pn], n >= 0 *)
  | PCons of pattern * pattern (** P0 :: P1 :: .. :: Pn, n>= 2 *)
  | POption of pattern option (** Some p, None *)
  | PConstraint of pattern * Typedtree.ty (** P : T *)

val show_pattern : pattern -> string

type expr =
  | Econst of const (** constants, e.g. 10, "meow", true *)
  | Evar of id (** identifiers, e.g. "x", "f"*)
  | Eif_then_else of expr * expr * expr option
  (** if E0 then E1 else E2; else expression is optional *)
  | Eoption of expr option (** option type, Some e, None *)
  | Etuple of expr * expr * expr list (** expressions (E0, .., En), n >= 2 *)
  | Elist of expr list (** expressions [E0; ..; En], n >= 0 *)
  | Ebin_op of bin_op * expr * expr (** E0 bin_op E1, e.g. 1 + 3 *)
  | Ematch of expr * case * case list (** match E with P1 -> E1 ... Pn -> Pn *)
  | Efunction of case * case list (** function P1 -> E1 ... Pn -> Pn *)
  | Eun_op of un_op * expr (** E0 un_op E1, e.g. Negative 2, Not true *)
  | Elet of rec_flag * value_binding * value_binding list * expr
  (** let (rec) P1 = E1 and P2 = E2 and ... and Pn = En in E, e.g. let x = 5 in x - 10 *)
  | Efun_application of expr * expr (** E0 E1, e.g. f x *)
  | Efun of pattern * pattern list * expr
  (** anonymous functions, e.g. fun x y -> x + 1 - y, arguments num >= 1 *)
  | Econstraint of expr * Typedtree.ty (** E : T *)

and case = Ecase of pattern * expr (** pattern-matching case, e.g. | P -> E *)
and value_binding = Evalue_binding of pattern * expr (** P = E *)

val show_expr : expr -> string
val show_case : case -> string
val show_value_binding : value_binding -> string

type structure_item =
  | SEval of expr (** plain expression E *)
  | SValue of rec_flag * value_binding * value_binding list
  (** let (rec) P1 = E1 and P2 = E2 and ... and Pn = En e.g. let x = 5 *)

val show_structure_item : structure_item -> string

type structure = structure_item list

val show_structure : structure -> string
val gen_structure : structure QCheck.Gen.t
val arb_structure : structure QCheck.arbitrary
