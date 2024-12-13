(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Identifier of variables and other definitions. **)
type identifier = string [@@deriving show { with_path = false }]

(** Literals of miniML **)
type literal =
  | IntLiteral of int (* 123  | 0 *)
  | BoolLiteral of bool (* true | false *)
  | UnitLiteral (* () value *)
[@@deriving show { with_path = false }]

(** Operators for unary expressions **)
type unary_operator =
  | Negate (* ~- *)
  | Positive (* ~+ *)
[@@deriving show { with_path = false }]

(** Definitions recurcive types *)
type recursive_type =
  | Recursive (* let rec f = e *)
  | Nonrecursive (* let f = e*)
[@@deriving show { with_path = false }]

(** Operators for binary expressions **)
type binary_operator =
  | Add (* + *)
  | Subtract (* - *)
  | Multiply (* * *)
  | Division (* / *)
  | And (* && *)
  | Or (* || *)
  | Gt (* > *)
  | Lt (* < *)
  | Gte (* >= *)
  | Lte (* <= *)
  | Equals (* = *)
  | Unequals (* <> *)
[@@deriving show { with_path = false }]

type core_type =
  | ArrowType of core_type * core_type (* T1 -> T2, example int -> bool *)
  | TypeConstructor of
      identifier * core_type (* IT T1, exmaple int list: int - T2, list - IT *)
  | TupleType of core_type * core_type * core_type list (* T1 * T2 * ... * TN *)
  | TypeIdentifier of identifier (* int *)
  | AnyType (* _ *)
[@@deriving show { with_path = false }]

type pattern =
  | PAny (* _ *)
  | PUnit (* () *)
  | PVar of identifier
  | PTuple of pattern * pattern * pattern list (* (<pattern>, ..., <pattern>) *)
  | PConstrain of pattern * core_type (* <pattern> : <core_type> *)
[@@deriving show { with_path = false }]

type expression =
  | Const of literal (* 123 | true *)
  | Variable of identifier (* x | factorial *)
  | Unary of unary_operator * expression (* ~-123 *)
  | Binary of expression * binary_operator * expression (* 12 + 34 | true && (x > y) *)
  | Tuple of expression * expression * expression list (* (E1, E2, E3 ... EN) *)
  | ExpressionsList of expression list (* [1; 2; (let x = 6 in x)] *)
  | Construct of identifier * expression option (* For example: None or Some <expr> *)
  | Match of expression * case list (* match E with <cases> *)
  | If of expression * expression * expression option (* if x then false else true *)
  | Lambda of pattern list * expression (* fun (x, (y,z)) -> x / (y + z) *)
  | Func of case list (* function <cases>*)
  | Apply of expression * expression list
    (* factorial (n / 2) | (fun (x, (y,z)) -> x / (y + z)) (5, (2, 1)) *)
  | Define of definition * expression (* let <definition> in <expr> *)
  | ExpressionBlock of expression list (* (let g x = x / 2 in g y; y); *)
[@@deriving show { with_path = false }]

and case =
  { pattern : pattern
  ; filter : expression option
  ; result : expression (* [(pattern -> E)] or [(pattern when filter -> result)] *)
  }
[@@deriving show { with_path = false }]

and value_binding = pattern * expression [@@deriving show { with_path = false }]

and definition = recursive_type * value_binding list
(* [rec] P1 = E1 and ... and <identifierN> PN = EN *)
[@@deriving show { with_path = false }]

type struct_item =
  | DefineItem of definition (* let f x = x;; *)
  | EvalItem of expression (* (fun x -> print_endline x) "Hello world";; *)
[@@deriving show { with_path = false }]

type program = struct_item list [@@deriving show { with_path = false }]
