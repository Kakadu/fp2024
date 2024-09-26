(** Copyright 2021-2024, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Identifier of variables and other definitions.
    Valid identifier maths on regex: `[a-zA-Z_][a-zA-Z_0-9]*` or `( [a-zA-Z_][a-zA-Z_0-9]* )`
    **)
type identifier = string [@@deriving show { with_path = false }]

(** Literals of miniML **)
type literal =
  | IntLiteral of int (* 123  | 0 *)
  | BoolLiteral of bool (* true | false *)
  | UnitLiteral (* () value *)
[@@deriving show { with_path = false }]

(** Operators for unary expressions **)
type unary_operator =
  | UnaryMinus (* ~+ *)
  | UnaryPlus (* ~- *)
  | Not (* not *)
[@@deriving show { with_path = false }]

(** Operators for binary expressions **)
type binary_operator =
  | BinaryPlus (* + *)
  | BinaryMinus (* - *)
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

(** Expression of miniML **)
type expression =
  | Const of literal (* 123 | true | () *)
  | Variable of identifier (* x | y | factorial *)
  | UnaryExpression of unary_operator * expression (* ~+(12 + x) | ~-x | not true *)
  | BinaryExpression of expression * binary_operator * expression (* 12 + 34 | 56 / x *)
  | Apply of identifier * expression list (* factorial (n / 2) *)
  | Tuple of expression list (* 1,2,3,x,true *)
[@@deriving show { with_path = false }]

(** Definitions types *)
type def_type =
  | Recursive (* let rec f = e *)
  | Nonrecursive (* let f = e*)
[@@deriving show { with_path = false }]

(** Definitions visability types:
    - Global
    - Inline

    For example
    [example.ml]
    - 1 |
    - 2 |[let f = ]
    - 3 |[  let g x = e in]
    - 4 |[  g 10]
    - 5 |

    In example [f] is global and [g] is inline definitions *)
type placement =
  | Global
  | Inline of identifier
[@@deriving show { with_path = false }]

(** Statements of miniML **)
type statement =
  | EvalStatement of expression (* x + y;; *)
  | IfStatement of
      expression * statement list * statement list (* if x > 0 then (..) else (..) *)
  | DefinitionStatement of
      def_type
      * placement
      * identifier
      * identifier list
      * statement list (* let [rec] f = (..) *)
[@@deriving show { with_path = false }]

(** miniMl's program:
    - identifier: name of file which contains minML's code
    - statement list: global statements of program
      **)
type program = identifier * statement list [@@deriving show { with_path = false }]
