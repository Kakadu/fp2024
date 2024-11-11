(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

let pp_const = function
  | Int i -> string_of_int i
  | String s -> "\"" ^ s ^ "\""
  | Bool b -> string_of_bool b
  | Unit -> "()"
;;

let pp_bin_op = function
  | Add -> "+"
  | Mult -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Gt -> ">"
  | Lt -> "<"
  | Eq -> "="
  | Neq -> "<>"
  | Gte -> ">="
  | Lte -> "<="
  | And -> "&&"
  | Or -> "||"
;;

let pp_un_op = function
  | Negative -> "-"
  | Positive -> "+"
  | Not -> "not"
;;

let pp_rec_flag = function
  | Recursive -> "rec"
  | Non_recursive -> ""
;;

let pp_pattern = function
  | PVar id -> id
  | PConst c -> pp_const c
  | PAny -> "_"
;;

let rec pp_expr = function
  | Econst c -> pp_const c
  | Evar id -> id
  | Eif_then_else (e1, e2, None) -> "if " ^ pp_expr e1 ^ " then " ^ pp_expr e2
  | Eif_then_else (e1, e2, Some e3) ->
    "if " ^ pp_expr e1 ^ " then " ^ pp_expr e2 ^ " else " ^ pp_expr e3
  | Etuple es -> "(" ^ String.concat ~sep:", " (List.map ~f:pp_expr es) ^ ")"
  | Elist es -> "[" ^ String.concat ~sep:"; " (List.map ~f:pp_expr es) ^ "]"
  | Efun (patterns, e) ->
    "fun " ^ String.concat ~sep:" " (List.map ~f:pp_pattern patterns) ^ " -> " ^ pp_expr e
  | Ebin_op (op, e1, e2) -> "(" ^ pp_expr e1 ^ " " ^ pp_bin_op op ^ " " ^ pp_expr e2 ^ ")"
  | Eun_op (op, e) -> pp_un_op op ^ pp_expr e
  | Elet (rec_flag, id, e1, e2) ->
    "let " ^ pp_rec_flag rec_flag ^ " " ^ id ^ " = " ^ pp_expr e1 ^ " in " ^ pp_expr e2
  | Efun_application (e1, e2) -> pp_expr e1 ^ " " ^ pp_expr e2
;;

let pp_structure_item (item : structure_item) : string =
  match item with
  | SEval e -> pp_expr e ^ ";"
  | SValue (rec_flag, id, e1, e2) ->
    "let " ^ pp_rec_flag rec_flag ^ " " ^ id ^ " = " ^ pp_expr e1 ^ " in " ^ pp_expr e2
;;

let prpr_structure fmt structure =
  List.iter ~f:(fun item -> Format.fprintf fmt "%s@." (pp_structure_item item)) structure
;;
