(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = string [@@deriving show { with_path = false }]
and is_rec = bool [@@deriving show { with_path = false }]

type bin_oper =
  | Plus (* [+] *)
  | Minus (* [-] *)
  | Multiply (* [*] *)
  | Division (* [/] *)
  | And (* [&&] *)
  | Or (* [||] *)
  | GretestEqual (* [>=] *)
  | LowestEqual (* [<=] *)
  | GreaterThan (* [>] *)
  | LowerThan (* [<] *)
  | Equal (* [=] *)
  | NotEqual (* [<>] *)
[@@deriving show { with_path = false }]

and unar_oper =
  | Negative (* [-x] *)
  | Not (* [not x]*)
[@@deriving show { with_path = false }]

type const =
  | ConstInt of int (* Integer constant: Example - [21] *)
  | ConstBool of bool (* Boolean constant: Example - [true] or [false] *)
  | ConstString of string (* String constant: Example - "I like OCaml!" *)
[@@deriving show { with_path = false }]

type pattern =
  | PatVariable of ident (* [x] *)
  | ParConst of const (* [21] or [true] or [false] *)
  | PatTuple of pattern list (* (x1; x2 ... xn) *)
  | PatOption of pattern option (* Matching with an optional value (Some/None) *)
[@@deriving show { with_path = false }]

type expr =
  | ExpIfThenElse of expr * expr * expr option
  (* ExpIfThenElse (x > 5, 10, 20(may return null) *)
  | ExpIdent of ident (* ExpIdent "x" *)
  | ExpConst of const (* ExpConst (ConstInt 666) *)
  | ExpBinOper of bin_oper * expr * expr (* ExpBinOper(Plus, 1, 2) *)
  | ExpUnarOper of unar_oper * expr (* ExpUnarOper(not, x)*)
  | ExpTuple of expr list (* ExpTuple[x1; x2 .. xn] *)
  | ExpList of expr list (* ExpList[x1; x2 .. xn] *)
  | ExpLambda of pattern list * expr (* ExpLambda([x;y;z], x+y+z)*)
  | ExpFunction of expr * expr (* ExpFunction(x, y)*)
  | ExpMatch of expr * (pattern * expr) list
  (* ExpMatch (x, [PatVariable "y", 10; PatConst (ConstInt 0), 0]) *)
  | ExpLet of is_rec * pattern * expr * expr
(* let x = 10 in x + 5 <=> ExpLet(false, "x", 10, x + 5) *)
(* let x = 10 <=> ExpLet(false, "x", 10, "x")*)
[@@deriving show { with_path = false }]
