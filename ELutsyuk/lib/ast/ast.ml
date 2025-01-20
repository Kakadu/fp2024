(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

type id = string (* expression identifier *) [@@deriving show { with_path = false }]

(** Represents constant values. *)
type const =
  | Int of int (** Integer constants, e.g. [-1], [ 2], [0]. *)
  | Str of string (** String constants, e.g. [{|meow|}], [{|miniML|}]. *)
  | Bool of bool (** Boolean constants, e.g. [true], [false]. *)
  | Unit (** [()] *)
[@@deriving show { with_path = false }]

(** Represents basic types in the language. *)
type typ =
  | TypInt (** Integer type, e.g. [int]. *)
  | TypStr (** String type, e.g. [string]. *)
  | TypBool (** Boolean type, e.g. [bool]. *)
  | TypUnit (** Unit type, e.g. [unit]. *)
  | TypList of typ (** List type, e.g. [int list], [string list]. *)
[@@deriving show { with_path = false }]

(** Represents binary operators, such as arithmetic or logical operations. *)
type binop =
  | Mul (** Multiplication, e.g. [a * b]. *)
  | Div (** Division, e.g. [a / b]. *)
  | Add (** Addition, e.g. [a + b]. *)
  | Sub (** Subtraction, e.g. [a - b]. *)
  | Eq (** Equality comparison, e.g. [a = b]. *)
  | Ne (** Inequality comparison, e.g. [a <> b]. *)
  | Lt (** Less-than comparison, e.g. [a < b]. *)
  | Le (** Less-than-or-equal comparison, e.g. [a <= b]. *)
  | Gt (** Greater-than comparison, e.g. [a > b]. *)
  | Ge (** Greater-than-or-equal comparison, e.g. [a >= b]. *)
  | And (** Logical AND, e.g. [a && b]. *)
  | Or (** Logical OR, e.g. [a || b]. *)
[@@deriving show { with_path = false }]

(** Represents patterns for matching values in expressions. *)
type pat =
  | PatConst of const (** Matches a constant value, e.g. [42], [true]. *)
  | PatVar of id (** Matches any value and binds it to a variable, e.g. [x]. *)
  | PatAny (** Matches any value without binding it, e.g. [_]. *)
  | PatTup of pat * pat * pat list (** Matches tuples, e.g. [(x, y)], [(a, b, c)]. *)
[@@deriving show { with_path = false }]

(** Indicates whether a [let] binding is recursive or non-recursive. *)
type rec_state =
  | Rec (** Recursive binding, e.g. [let rec fact = ...]. *)
  | NonRec (** Non-recursive binding, e.g. [let x = ...]. *)
[@@deriving show { with_path = false }]

(** Represents expressions in the language. *)
type expr =
  | Var of id (** Variable reference, e.g. [x], [my_var]. *)
  | Const of const (** Constant value, e.g. [42], ["hello"], [true]. *)
  | BinOp of binop * expr * expr (** Binary operation, e.g. [x + y], [a >= b]. *)
  | Let of let_binding * expr (** [let] expression, e.g. [let x = 5 in e]. *)
  | App of expr * expr (** Function application, e.g. [e1 e2], [(fun x -> x) 42]. *)
  | Fun of pat * expr (** Function definition, e.g. [fun p -> e]. *)
  | Branch of expr * expr * expr
  (** Conditional expression, e.g. [if e1 then e2 else e3]. *)
  | Tup of expr * expr * expr list (** Tuple expression, e.g. [(e1, e2)], [(x, y, z)]. *)
  | List of expr list (** List expression, e.g. [[]], [[e1; e2; e3]]. *)
  | Match of expr * match_case * match_case list
  (** Pattern matching, e.g. [match x with | 0 -> "zero" | _ -> "nonzero"]. *)
[@@deriving show { with_path = false }]

(** Represents a binding in a [let] expression. *)
and let_binding =
  { is_rec : rec_state (** Whether the binding is recursive or non-recursive. *)
  ; pat : pat (** The pattern being bound, e.g. [x], [(a, b)]. *)
  ; expr : expr (** The expression being assigned, e.g. [42], [fun x -> x + 1]. *)
  }
[@@deriving show { with_path = false }]

(** Represents a single case in a [match] expression. *)
and match_case =
  { match_pat : pat (** The pattern to match, e.g. [0], [_], [(x, y)]. *)
  ; match_expr : expr (** The expression to evaluate if the pattern matches. *)
  }
[@@deriving show { with_path = false }]

(** Represents a top-level item in a program. *)
type top_level_item =
  | EvalExpr of expr
  (** An expression to be evaluated but not bound, e.g. [1 + 2], [print_endline "Hi"]. *)
  | Binding of let_binding
  (** A value or function binding, e.g. [let x = 5], [let rec fact n = ...]. *)
[@@deriving show { with_path = false }]

(** Represents an entire program as a list of top-level items. *)
type program = top_level_item list [@@deriving show { with_path = false }]
