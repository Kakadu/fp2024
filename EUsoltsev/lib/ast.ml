(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type ident = string [@@deriving show { with_path = false }]
type is_rec = bool [@@deriving show { with_path = false }]

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

type unar_oper =
  | Negative (* [-x] *)
  | Not (* [not x]*)
[@@deriving show { with_path = false }]

type const =
  | ConstInt of int (* Integer constant: Example - [21] *)
  | ConstBool of bool (* Boolean constant: Example - [true] or [false] *)
  | ConstString of string (* String constant: Example - "I like OCaml!" *)
  | ConstUnit
[@@deriving show { with_path = false }]

type binder = int [@@deriving show { with_path = false }]

type ty =
  | TyVar of binder
  | TyPrim of string
  | TyArrow of ty * ty
  | TyList of ty
  | TyTuple of ty list
  | TyOption of ty
[@@deriving show { with_path = false }]

type pattern =
  | PatVariable of ident (* [x] *)
  | PatConst of const (* [21] or [true] or [false] *)
  | PatTuple of pattern * pattern * pattern list (* (x1; x2 ... xn) *)
  | PatAny
  | PatType of pattern * ty
  | PatUnit
[@@deriving show { with_path = false }]

type expr =
  | ExpIdent of ident (* ExpIdent "x" *)
  | ExpConst of const (* ExpConst (ConstInt 666) *)
  | ExpBranch of expr * expr * expr option
  | ExpBinOper of bin_oper * expr * expr (* ExpBinOper(Plus, 1, 2) *)
  | ExpUnarOper of unar_oper * expr (* ExpUnarOper(not, x)*)
  | ExpTuple of expr * expr * expr list (* ExpTuple[x1; x2 .. xn] *)
  | ExpList of expr list (* ExpList[x1; x2 .. xn] *)
  | ExpLambda of pattern list * expr (* ExpLambda([x;y;z], x+y+z)*)
  | ExpTypeAnnotation of expr * ty
  | ExpOption of expr option
  | ExpFunction of expr * expr (* ExpFunction(x, y)*)
  | ExpLet of is_rec * pattern * expr * expr option
  | ExpLetAnd of is_rec * (pattern * expr) list * expr option
(* let x = 10 in x + 5 <=> ExpLet(false, "x", 10, x + 5) *)
(* let x = 10 <=> ExpLet(false, "x", 10, None)*)
[@@deriving show { with_path = false }]

type program = expr list [@@deriving show { with_path = false }]

let rec pp_ty fmt = function
  | TyPrim x -> fprintf fmt "%s" x
  | TyVar x -> fprintf fmt "%s" @@ Char.escaped (Char.chr (x + 97))
  | TyArrow (l, r) ->
    (match l, r with
     | TyArrow _, _ -> fprintf fmt "(%a) -> %a" pp_ty l pp_ty r
     | _, _ -> fprintf fmt "%a -> %a" pp_ty l pp_ty r)
  | TyTuple elems ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") pp_ty)
      elems
  | TyList ty ->
    (match ty with
     | TyArrow _ | TyTuple _ -> fprintf fmt "(%a) list" pp_ty ty
     | _ -> fprintf fmt "%a list" pp_ty ty)
  | TyOption ty ->
    (match ty with
     | TyArrow _ | TyTuple _ -> fprintf fmt "(%a) option" pp_ty ty
     | _ -> fprintf fmt "%a option" pp_ty ty)
;;
