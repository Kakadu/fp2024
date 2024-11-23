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

let pp_unop ppf = function
  | UnaryPlus -> fprintf ppf "+"
  | UnaryMinus -> fprintf ppf "-"
  | UnaryNeg -> fprintf ppf "not "
;;

let pp_annot =
  let rec helper ppf = function
    | AInt -> fprintf ppf "int"
    | ABool -> fprintf ppf "bool"
    | AString -> fprintf ppf "string"
    | AUnit -> fprintf ppf "unit"
    | AList t -> fprintf ppf "%a list" helper t
    | ATuple l ->
      let rec pp_tuple_types ppf = function
        | [] -> fprintf ppf ""
        | [ x ] -> fprintf ppf "%a" helper x
        | x :: xs ->
          fprintf ppf "%a * " helper x;
          pp_tuple_types ppf xs
      in
      fprintf ppf "%a" pp_tuple_types l
  in
  helper
;;

let pp_rec_flag ppf = function
  | NonRec -> fprintf ppf ""
  | Rec -> fprintf ppf " rec"
;;

let pp_literal ppf = function
  | IntLiteral i -> fprintf ppf "%d" i
  | BoolLiteral b -> fprintf ppf "%b" b
  | StringLiteral s -> fprintf ppf "\"%s\"" s
  | UnitLiteral -> fprintf ppf "()"
  | NilLiteral -> fprintf ppf "[]"
;;

let pp_pattern =
  let rec helper ppf = function
    | PAny -> fprintf ppf "_"
    | PLiteral l -> fprintf ppf "%a" pp_literal l
    | PVar v -> fprintf ppf "%s" v
    | PCons (p1, p2) -> fprintf ppf "(%a::%a)" helper p1 helper p2
    | PTuple (p1, p2, rest) ->
      let rec pp_tuple ppf = function
        | [] -> fprintf ppf ""
        | [ x ] -> fprintf ppf "%a" helper x
        | x :: xs ->
          fprintf ppf "%a, " helper x;
          pp_tuple ppf xs
      in
      fprintf ppf "(%a)" pp_tuple (p1 :: p2 :: rest)
    | POption x ->
      (match x with
       | Some x -> fprintf ppf "Some %a" helper x
       | None -> fprintf ppf "None")
    | PType (pat, tp) -> fprintf ppf "(%a : %a)" helper pat pp_annot tp
  in
  helper
;;

let rec pp_expr =
  let rec helper ppf = function
    | ExprVariable v -> fprintf ppf "%s" v
    | ExprLiteral l -> fprintf ppf "%a" pp_literal l
    | ExprBinOperation (op, e1, e2) ->
      (match e1, e2 with
       | ExprVariable _, _ | ExprLiteral _, _ ->
         fprintf ppf "%a %a %a" helper e1 pp_binop op helper e2
       | _, ExprVariable _ | _, ExprLiteral _ ->
         fprintf ppf "%a %a %a" helper e1 pp_binop op helper e2
       | _ -> fprintf ppf "((%a) %a (%a))" helper e1 pp_binop op helper e2)
    | ExprUnOperation (op, e) -> fprintf ppf "%a%a" pp_unop op helper e
    | ExprIf (c, th, el) ->
      (match el with
       | None -> fprintf ppf "if %a then %a" helper c helper th
       | Some x -> fprintf ppf "if %a then %a else %a" helper c helper th helper x)
    | ExprMatch (e, branches) ->
      fprintf ppf "match %a with\n" helper e;
      List.iter
        (fun (pattern, branch_expr) ->
          fprintf ppf "| %a -> %a\n" pp_pattern pattern helper branch_expr)
        branches
    | ExprLet (rf, b, bl, e) ->
      fprintf ppf "let%a %a in %a" pp_rec_flag rf pp_binding_list (b :: bl) helper e
    | ExprApply (e1, e2) ->
      (match e2 with
       | ExprBinOperation _ -> fprintf ppf "%a (%a)" helper e1 helper e2
       | _ -> fprintf ppf "%a %a" helper e1 helper e2)
    | ExprTuple (p1, p2, rest) ->
      let pp_tuple_helper ppf x =
        match x with
        | ExprBinOperation _
        | ExprUnOperation _
        | ExprIf _
        | ExprMatch _
        | ExprLet _
        | ExprApply _
        | ExprFun _ -> fprintf ppf "(%a)" helper x
        | _ -> fprintf ppf "%a" helper x
      in
      let rec pp_tuple ppf = function
        | [] -> ()
        | [ x ] -> fprintf ppf "%a" pp_tuple_helper x
        | x :: xs ->
          fprintf ppf "%a, " pp_tuple_helper x;
          pp_tuple ppf xs
      in
      fprintf ppf "(%a)" pp_tuple (p1 :: p2 :: rest)
    | ExprList l ->
      let pp_list_helper ppf x =
        match x with
        | ExprBinOperation _
        | ExprUnOperation _
        | ExprIf _
        | ExprMatch _
        | ExprLet _
        | ExprApply _
        | ExprFun _ -> fprintf ppf "(%a)" helper x
        | _ -> fprintf ppf "%a" helper x
      in
      let rec pp_list ppf = function
        | [] -> ()
        | [ x ] -> fprintf ppf "%a" pp_list_helper x
        | x :: xs ->
          fprintf ppf "%a; " pp_list_helper x;
          pp_list ppf xs
      in
      fprintf ppf "[%a]" pp_list l
    | ExprCons (e1, e2) -> fprintf ppf "%a::%a" helper e1 helper e2
    | ExprFun (p, e) -> fprintf ppf "fun %a -> %a" pp_pattern p helper e
    | ExprOption x ->
      (match x with
       | Some x -> fprintf ppf "Some %a" helper x
       | None -> fprintf ppf "None")
  in
  helper

and pp_binding_list ppf =
  let pp_binding ppf binding =
    let p, e = binding in
    match e with
    | ExprFun (p1, e1) -> fprintf ppf "%a %a = %a" pp_pattern p pp_pattern p1 pp_expr e1
    | _ -> fprintf ppf "%a = %a" pp_pattern p pp_expr e
  in
  let rec helper = function
    | [] -> ()
    | [ x ] -> fprintf ppf "%a" pp_binding x
    | x :: xs ->
      fprintf ppf "%a and " pp_binding x;
      helper xs
  in
  helper
;;

let pp_structure ppf = function
  | SEval e -> fprintf ppf "%a" pp_expr e
  | SValue (rf, b, bl) -> fprintf ppf "let%a %a" pp_rec_flag rf pp_binding_list (b :: bl)
;;

let pp_structure_item_list ppf structure_list =
  List.iter (fun item -> fprintf ppf "%a;;\n" pp_structure item) structure_list
;;
