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

type pattern =
  | PVar of identifier
  | PTuple of pattern list
[@@deriving show { with_path = false }]

type expresssion =
  | Const of literal (* 123 | true *)
  | Variable of identifier (* x | factorial *)
  | Unary of unary_operator * expresssion (* ~-123 *)
  | Binary of expresssion * binary_operator * expresssion (* 12 + 34 | true && (x > y) *)
  | Tuple of statement list (* (1, 2, (let x = 6 in x)) *)
  | If of statement * statement * statement (* if x then false else true *)
  | Lambda of pattern list * statement (* fun (x, (y,z)) -> x / (y + z) *)
  | Apply of expresssion * statement list
    (* factorial (n / 2) | (fun (x, (y,z)) -> x / (y + z)) (5, (2, 1)) *)
[@@deriving show { with_path = false }]

and statement =
  | EvalStatement of expresssion (* (f x) | 12 + 45; *)
  | DefineStatement of definition (* let g x y = x + y in *)
  | StatementsBlock of statement list (* (let g x = x / 2 in g y); *)
[@@deriving show { with_path = false }]

and definition = identifier * recursive_type * pattern list * statement
[@@deriving show { with_path = false }]

type struct_item =
  | DefineItem of definition (* let f x = x;; *)
  | StatementItem of statement (* if x > 0 then f x else f (-x);; *)
[@@deriving show { with_path = false }]

type program = identifier * struct_item list [@@deriving show { with_path = false }]
