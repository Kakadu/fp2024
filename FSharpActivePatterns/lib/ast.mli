(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = Ident of string (** identifier *)
[@@deriving eq, show { with_path = false }]

type literal =
  | Int_lt of int (** [0], [1], [30] *)
  | Bool_lt of bool (** [false], [true] *)
  | String_lt of string (** ["Hello world"] *)
  | Unit_lt (** [Unit] *)
[@@deriving eq, show { with_path = false }]

type binary_operator =
  | Binary_equal (** [=] *)
  | Binary_unequal (** [<>] *)
  | Binary_less (** [<] *)
  | Binary_less_or_equal (** [<=] *)
  | Binary_greater (** [>] *)
  | Binary_greater_or_equal (** [>=] *)
  | Binary_add (** [+] *)
  | Binary_subtract (** [-] *)
  | Binary_multiply (** [*] *)
  | Logical_or (** [||] *)
  | Logical_and (** [&&] *)
  | Binary_divide (** [/] *)
  | Binary_or_bitwise (** [|||] *)
  | Binary_xor_bitwise (** [^^^] *)
  | Binary_and_bitwise (** [&&&] *)
[@@deriving eq, show { with_path = false }]

type unary_operator =
  | Unary_minus (** unary [-] *)
  | Unary_not (** unary [not] *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | Wild (** [_] *)
  | PCons of pattern * pattern (** | [hd :: tl] -> *)
  | PTuple of pattern list (** | [(a, b)] -> *)
  | PConst of literal (** | [4] -> *)
  | PVar of ident (** pattern identifier *)
  | Variant of ident list (** | [Blue, Green, Yellow] -> *)
[@@deriving eq, show { with_path = false }]

type is_recursive =
  | Nonrec (** let factorial n = ... *)
  | Rec (** let rec factorial n = ... *)
[@@deriving eq, show { with_path = false }]

type expr =
  | Const of literal (** [Int], [Bool], [String], [Unit], [Null] *)
  | Tuple of expr list (** [(1, "Hello world", true)] *)
  | List_expr of expr * expr
  (** {[
        [ 1; 2; 3 ]
      ]} *)
  | Variable of ident (** [x], [y] *)
  | Unary_expr of unary_operator * expr (** -x *)
  | Bin_expr of binary_operator * expr * expr (** [1 + 2], [3 ||| 12] *)
  | If_then_else of expr * expr * expr option (** [if n % 2 = 0 then "Even" else "Odd"] *)
  | Function_def of expr list * expr (** fun x y -> x + y *)
  | Function_call of expr * expr (** [sum 1 ] *)
  | Match of expr * (pattern * expr) list (** [match x with | x -> ... | y -> ...] *)
  | LetIn of is_recursive * ident option * expr list * expr * expr
  (** [let f x y = x + y in f 3 4] *)
[@@deriving eq, show { with_path = false }]

type statement =
  | Let of is_recursive * ident * expr list * expr (** [let name = expr] *)
  | ActivePattern of ident list * expr
  (** [let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd] *)
[@@deriving eq, show { with_path = false }]

type construction =
  | Expr of expr (** expression *)
  | Statement of statement (** statement *)
[@@deriving eq, show { with_path = false }]
