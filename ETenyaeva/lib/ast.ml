(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string (** identifier *) [@@deriving show { with_path = false }]

type rec_flag =
  | Rec (** recursive *)
  | NonRec (** non-recursive *)
[@@deriving show { with_path = false }]

type const =
  | Int of int (** integer, e.g. 26 *)
  | Bool of bool (** boolean, e.g. true *)
  | String of string (** string, e.g. "string" *)
  | Char of char (** char, e.g. 'a' *)
  | Unit (** [()] *)
[@@deriving show { with_path = false }] 

type typ =
  | TypInt (** integer type - [int] *)
  | TypChar (** char type - [char] *)
  | TypStr (** string type - [string] *)
  | TypBool (** boolean type - [bool] *)
  | TypUnit (** unit type - [unit] *)
  | TypVar of id (** variable type *)
  | TypArrow of typ * typ (** arrow type *)
  | TypList of typ (** list type, e.g. [int list], [string list] *)
  | TypTuple of typ * typ * typ list (** tuple type, e.g. [int * int * string] *)
  | TypOption of typ (** type option *)
[@@deriving show { with_path = false }]

type binary_oper =
  | Add (** [+] *)
  | Sub (** [-] *)
  | Mult (* [*] *)
  | Div (** [/] *) 
  | And (** [&&] *)
  | Or (** [||] *)
  | Equals (** [=] *)
  | NotEquals (** [<>] *)
  | LessThan (** [<] *)
  | LessEquals (** [<=] *)
  | GreaterThan (** [>] *)
  | GreaterEquals (** [>=] *)
[@@deriving show { with_path = false }]

type unary_oper =
  | Neg (** negation of a value, e.g. -5 *)
  | Not (** [not] *)
[@@deriving show { with_path = false }]

type pattern =
  | PatConst of const (** matches a constant value, e.g. 42, true *)
  | PatVar of id (** matches any value and binds it to a variable, e.g. x *)
  | PatAny (** matches any value without binding it - [_] *)
  | PatTup of pattern * pattern * pattern list (** matches tuples, e.g. (x, y), (a, b, c) *)
  | PatList of pattern list (** matches lists of patterns, e.g. [y; x] *)
  | PatListConstructor of pattern list (** matches lists of patterns, e.g. a::b::[] *)
  | PatOption of pattern option (** matches an optional pattern, e.g. Some x or None *)
  | PatWithTyp of typ * pattern (** typed pattern, e.g. a: int *)
[@@deriving show { with_path = false }]

type expr =
  | ExpVar of id (** variable, e.g. x *)
  | ExpConst of const (** constant, e.g. 10*)
  | ExpIfThenElse of expr * expr * expr option (** conditional expression, e.g. if a then b else c*)
  | ExpFun of pattern * expr (** function, e.g. fun (x, y) -> x + y *)
  | ExpBinOper of binary_oper * expr * expr (** binary operation, e.g. 1 + 5*)
  | ExpUnOper of unary_oper * expr (** unary operation, e.g. -7 *)
  | ExpList of expr list (** list expression, e.g. [1, "string", 2, (1 + 7)] *)
  | ExpListConstructor of expr list (** list expression, e.g. 1::2::[] *)
  | ExpLet of rec_flag * let_binding * let_binding list * expr (** let, e.g. let x = 5 *)
  | ExpApp of expr * expr (** application, e.g. (fun (x, y) -> x + y) (1, 2) *)
  | ExpTup of expr * expr * expr list (** tuple expression, e.g. (e1, e2), (x, y, z) *)
  | ExpMatch of expr * match_case * match_case list (** pattern matching, e.g. match x with | 0 -> "zero" | _ -> "nonzero" *)
  | ExpOption of expr option (** optonal expression, e.g. Some x*)
  | ExpWithTyp of typ * expr (** typed expression, e.g. a: int *)
[@@deriving show { with_path = false }]

and let_binding =
  { pat : pattern (** the pattern being bound, e.g. x, (a, b) *)
  ; expr : expr (** the expression being assigned, e.g. 42, fun x -> x + 1 *)
  }
[@@deriving show { with_path = false }]

and match_case =
  { match_pat : pattern (** the pattern to match, e.g. x, _ *)
  ; match_expr : expr (** the expression to evaluate if the pattern matches *)
  }
[@@deriving show { with_path = false }]

type structure_item =
  | EvalExp of expr
  (** an expression to be evaluated but not bound, e.g. 1 + 2*)
  | Binding of rec_flag * let_binding * let_binding list
  (** a value or function binding, e.g. let x = 1*)
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]
