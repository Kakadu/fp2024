(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

let pp_binop ppf = function
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
;;

let pp_rec_flag ppf = function
  | NonRec -> fprintf ppf ""
  | Rec -> fprintf ppf " rec"
;;

let pp_literal ppf = function
  | IntLiteral i -> fprintf ppf "%d" i
  | BoolLiteral b -> fprintf ppf "%b" b
  | StringLiteral s -> fprintf ppf "%S" s
  | UnitLiteral -> fprintf ppf "()"
  | NilLiteral -> fprintf ppf "[]"
;;

let pp_pattern =
  let rec helper ppf = function
    | PAny -> fprintf ppf "_"
    | PLiteral l -> fprintf ppf "%a" pp_literal l
    | PVar v -> fprintf ppf "%s" v
    | PCons (p1, p2) -> fprintf ppf "%a::%a" helper p1 helper p2
  in
  helper
;;

let pp_expr =
  let rec helper ppf = function
    | ExprVariable v -> fprintf ppf "%s" v
    | ExprLiteral l -> fprintf ppf "%a" pp_literal l
    | ExprBinOperation (op, e1, e2) ->
      fprintf ppf "(%a %a %a)" helper e1 pp_binop op helper e2
    | ExprUnOperation _ -> ()
    | ExprIf (c, th, el) ->
      (match el with
       | None -> fprintf ppf "if %a then %a" helper c helper th
       | Some x -> fprintf ppf "if %a then %a else %a" helper c helper th helper x)
    | ExprMatch _ -> ()
    | ExprLet (rf, bl, e) ->
      let pp_binding ppf binding =
        let p, e = binding in
        match e with
        | ExprFun (p1, e1) ->
          fprintf ppf "%a %a = %a" pp_pattern p pp_pattern p1 helper e1
        | _ -> fprintf ppf "%a = %a" pp_pattern p helper e
      in
      let pp_binding_list ppf binding_list =
        let rec helper = function
          | [] -> ()
          | [ x ] -> fprintf ppf "%a" pp_binding x
          | x :: xs ->
            fprintf ppf "%a and " pp_binding x;
            helper xs
        in
        helper binding_list
      in
      fprintf ppf "let%a %a in %a" pp_rec_flag rf pp_binding_list bl helper e
    | ExprApply (e1, e2) ->
      (match e2 with
       | ExprBinOperation _ -> fprintf ppf "%a (%a)" helper e1 helper e2
       | _ -> fprintf ppf "%a %a" helper e1 helper e2)
    | ExprTuple t ->
      let rec pp_tuple ppf = function
        | [] -> ()
        | [ x ] -> fprintf ppf "%a" helper x
        | x :: xs ->
          fprintf ppf "%a, " helper x;
          pp_tuple ppf xs
      in
      fprintf ppf "(%a)" pp_tuple t
    | ExprCons (e1, e2) -> fprintf ppf "%a::%a" helper e1 helper e2
    | ExprFun (p, e) -> fprintf ppf "fun %a -> %a" pp_pattern p helper e
  in
  helper
;;

let pp_binding ppf binding =
  let p, e = binding in
  match e with
  | ExprFun (p1, e1) -> fprintf ppf "%a %a = %a" pp_pattern p pp_pattern p1 pp_expr e1
  | _ -> fprintf ppf "%a = %a" pp_pattern p pp_expr e
;;

let pp_binding_list ppf binding_list =
  let rec helper = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a" pp_binding x
    | x :: xs ->
      fprintf ppf "%a and " pp_binding x;
      helper xs
  in
  helper binding_list
;;

let pp_structure ppf = function
  | SEval e -> fprintf ppf "%a" pp_expr e
  | SValue (rf, bl) -> fprintf ppf "let%a %a" pp_rec_flag rf pp_binding_list bl
;;

let pp_structure_item_list ppf structure_list =
  List.iter (fun item -> fprintf ppf "%a;;\n" pp_structure item) structure_list
;;
