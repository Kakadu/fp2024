(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

type id = string (* expression identificator *) [@@deriving show { with_path = false }]

(* constants *)
type literal =
  | Int of int (* ex: 1, 2, 3 *)
  | Str of string (* ex: "hello", "miniML" *)
  | Bool of bool (* true, false *)
[@@deriving show { with_path = false }]

type binary_op =
  | Eq (* = *)
  | NotEq (* != *)
  | Lt (* < *)
  | LtEq (* <= *)
  | Gt (* > *)
  | GtEq (* >= *)
  | And (* && *)
  | Or (* || *)
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
[@@deriving show { with_path = false }]

type pattern =
  | PatVar of id (* [var] *)
  | PatAny (* [_] *)
  | PatLiteral of literal (* [literal] *)
  | PatCons of pattern * pattern (* | expr1 :: expr2 -> ... *)
  | PatSome of pattern (* | Some(pattern) -> ... *)
  | PatNone (* | None -> ... *)
[@@deriving show { with_path = false }]

type recursion_state =
  | Rec
  | NonRec
[@@deriving show { with_path = false }]

type expression =
  | ExpVar of id (* variable name *)
  | ExpConst of literal (* literal *)
  | ExpBinop of binary_op * expression * expression (* expr binop expr *)
  | ExpLet of
      recursion_state * (id * expression) list * expression (* let [rec] expr in expr *)
  | ExpApp of expression * expression (* expr expr *)
  | ExpLambda of id * expression (* fun id -> expr *)
  | ExpIf of expression * expression * expression option (* if exp then exp else exp *)
  | ExpWhile of expression * expression (* while expr do expr done *)
  | ExpTuple of expression * expression * expression list (* at least two elements *)
  | ExpList of expression list
  | ExpOptSome of expression (* Some(expr) *)
  | ExpOptNone (* None *)
  | ExpMatch of expression * (pattern * expression) list
[@@deriving show { with_path = false }]

type program = expression list [@@deriving show { with_path = false }]