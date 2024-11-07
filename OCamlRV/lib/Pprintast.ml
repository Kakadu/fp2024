(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser
open Format

let pp_binop =
  let rec helper ppf = function
    | Add -> fprintf ppf "+"
    | Sub -> fprintf ppf "-"
    | Mul -> fprintf ppf "*"
    | Div -> fprintf ppf "/"
    | Lt -> fprintf ppf "<"
    | Gt -> fprintf ppf ">"
    | Eq -> fprintf ppf "="
    | Neq -> fprintf ppf "<>"
    | Lte -> fprintf ppf "<="
    | Gte -> fprintf ppf ">="
    | And -> fprintf ppf "&&"
    | Or -> fprintf ppf "||"
  in
  helper
;;

let pp_rec_flag =
  let rec helper ppf = function
    | NonRec -> fprintf ppf ""
    | Rec -> fprintf ppf "rec"
  in
  helper
;;

let pp_literal =
  let rec helper ppf = function
    | IntLiteral i -> fprintf ppf "%d" i
    | BoolLiteral b -> fprintf ppf "%b" b
    | StringLiteral s -> fprintf ppf "%s" s
    | UnitLiteral -> fprintf ppf "()"
    | NilLiteral -> fprintf ppf "[]"
  in
  helper
;;

let pp_pattern =
  let rec helper ppf = function
    | PAny -> fprintf ppf "_"
    | PLiteral l -> fprintf ppf "%a" pp_literal l
    | PVar v -> fprintf ppf "%s" v
    | PTuple pl -> ()
    | PCons (p1, p2) -> fprintf ppf "%a::%a" helper p1 helper p2
    | PPoly (id, po) -> ()
  in
  helper
;;

let pp_expr =
  let rec helper ppf = function
    | ExprVariable v -> fprintf ppf "%s" v
    | ExprLiteral l -> fprintf ppf "%a" pp_literal l
    | ExprBinOperation (op, e1, e2) ->
      fprintf ppf "%a %a %a" helper e1 pp_binop op helper e2
    | ExprUnOperation (op, e) -> ()
    | ExprIf (c, th, el) ->
      (match el with
       | None -> fprintf ppf "if %a then %a" helper c helper th
       | Some x -> fprintf ppf "if %a then %a else %a" helper c helper th helper x)
    | ExprMatch (e, cl) -> ()
    | ExprLet (rf, bl, e) -> ()
    | ExprApply (e1, e2) -> fprintf ppf "%a %a" helper e1 helper e2
    | ExprTuple el -> ()
    | ExprCons (e1, e2) -> fprintf ppf "%a::%a" helper e1 helper e2
    | ExprPoly (id, e) -> ()
    | ExprFun (p, e) -> fprintf ppf "fun %a -> %a" pp_pattern p helper e
  in
  helper
;;

let pp_structure =
  let rec helper ppf = function
    | SEval e -> fprintf ppf "%a" pp_expr e
    | SValue (rf, bl) -> fprintf ppf "hi\n"
  in
  helper
;;

let pp_structure_item_list ppf structure_list =
  List.iter (fun item -> fprintf ppf "%a;;\n" pp_structure item) structure_list
;;
