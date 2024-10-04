(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = Ident of string [@@deriving eq, show { with_path = false }]

type literal =
  | Int_lt of int
  | Bool_lt of bool
  | String_lt of string
  | Unit_lt
  | Null_lt
[@@deriving eq, show { with_path = false }]

type binary_operator =
  | Binary_equal
  | Binary_unequal
  | Binary_less
  | Binary_less_or_equal
  | Binary_greater
  | Binary_greater_or_equal
  | Binary_add
  | Binary_subtract
  | Binary_multiply
  | Logical_or
  | Logical_and
  | Binary_divide
  | Binary_or_bitwise
  | Binary_xor_bitwise
  | Binary_and_bitwise
[@@deriving eq, show { with_path = false }]

type unary_operator =
  | Unary_plus
  | Unary_minus
  | Unary_negative
[@@deriving eq, show { with_path = false }]

type pattern =
  | Wild (** _ *)
  | PCons of pattern * pattern (** hd :: tl*)
  | PConst of literal (** | 4 -> *)
  | PVar of ident (** ident for other patterns *)
  | Variant of ident list (** | Blue, Green, Yellow -> *)
[@@deriving eq, show { with_path = false }]

type is_recursive =
  | Nonrec
  | Rec
[@@deriving eq, show { with_path = false }]

type expr =
  | Const of literal
  | Tuple of expr list
  | List_expr of expr * expr (** List (1, List (2, Null)) *)
  | Variable of ident
  | Bin_expr of binary_operator * expr * expr (**operator, first operand, second operand*)
  | If_then_else of expr * expr list * expr list option
  (**condition, then body, else body*)
  | Function_def of is_recursive * string option * expr list * expr
  (**rec, name, args, body*)
  | Function_call of expr * expr list (** sum 1 2 *)
  | Match of expr * (pattern * expr) list (**matching value, pattern-value list *)
  | LetIn of ident * expr * expr (**name, value, inner value*)
[@@deriving eq, show { with_path = false }]

type statement =
  | Let of ident * expr (**name, value*)
  | ActivePattern of ident list * expr
  (** let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd *)
[@@deriving eq, show { with_path = false }]

type construction =
  | Expr of expr
  | Statement of statement
[@@deriving eq, show { with_path = false }]
