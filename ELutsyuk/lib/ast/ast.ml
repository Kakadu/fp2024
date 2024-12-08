(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)
type id = string (* expression identifier *) [@@deriving show { with_path = false }]

type constant =
  | Int of int (** e.g. [ -1 ], [ 2 ], [ 3 ] *)
  | Str of string (** e.g. [ "hello" ], [ "miniML" ] *)
  | Bool of bool (** [ true ], [ false ] *)
  | Unit (** [ () ] *)
[@@deriving show { with_path = false }]

type types =
  | TyInt
  | TyString
  | TyBool
  | TyUnit
  | TyList of types
[@@deriving show { with_path = false }]

type binary_op =
  | Mult (** [ * ] *)
  | Div (** [ / ] *)
  | Add (** [ + ] *)
  | Sub (** [ - ] *)
  | Eq (** [ = ] *)
  | NonEq (** [ <> ] *)
  | Lt (** [ < ] *)
  | LtEq (** [ <= ] *)
  | Gt (** [ > ] *)
  | GtEq (** [ >= ] *)
  | And (** [ && ] *)
  | Or (** [ || ] *)
[@@deriving show { with_path = false }]

type pattern =
  | PCons of constant (** [ cons ] *)
  | PVar of id (** [ var ] *)
  | PAny (** [ _ ] *)
  | PTuple of pattern * pattern * pattern list (** [ | (p1, p2) -> ... ] *)
    (* | PTypes of pattern * types *)
[@@deriving show { with_path = false }]

type recursion_state =
  | Rec (** [let rec ...] *)
  | NonRec (** [ let ... ]*)
[@@deriving show { with_path = false }]

type expression =
  | Var of id (** e.g. [ x ], [ variable ] *)
  | Cons of constant (** e.g. [ 42 ], [ {|meow|} ], [ true ] *)
  | BinaryOp of binary_op * expression * expression
  (** e.g. [ exp1 >= exp2 ], [ exp1 + exp2 ] *)
  | Let of let_binding * expression (** [ let (rec) pat =  in f 5 ] *)
  | App of expression * expression (** foo x ==> App (Var "foo", Var "x") *)
  | Fun of pattern * expression (** fun x y -> e ==> Fun ("x", Fun ("y", Var "e")) *)
  | Branch of expression * expression * expression
  (** [ if exp then exp else Some(exp) ] *)
  | Tuple of expression * expression * expression list
  (** contain at least two elements, e.g. [ (exp, exp) ] *)
  | List of expression list (** [ [exp; exp; exp] ]*)
  | Match of expression * match_case * match_case list
  (** [ match exp with | pat1 -> exp1 | pat2 -> exp2 ] *)
[@@deriving show { with_path = false }]

(** represents binding of a variable or function in a [ let ] expression *)
and let_binding =
  { is_rec : recursion_state
  ; pat : pattern
  ; expr : expression
  }
[@@deriving show { with_path = false }]

(** represents a case in a [match] expression *)
and match_case =
  { match_pat : pattern
  ; match_expr : expression
  }
[@@deriving show { with_path = false }]

(** represents constructs that can appear in a program *)
type program_item =
  | Evaluation of expression (** expr that are run but not saved in variable *)
  | Binding of let_binding (** variable of function that can be used later in program *)
[@@deriving show { with_path = false }]

(** represents the entire program as a list of [ program_item ] *)
type program = program_item list [@@deriving show { with_path = false }]
