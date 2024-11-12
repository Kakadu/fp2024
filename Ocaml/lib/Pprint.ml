(** Copyright 2021-2023, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Typedtree
open Ast
open Interpreter.Interpret

let rec pp_typ ppf = function
  | Ty_var n -> fprintf ppf "'_%d" n
  | TyInt -> pp_print_string ppf "int"
  | TyBool -> pp_print_string ppf "bool"
  | TyString -> pp_print_string ppf "string"
  | TyTuple ty ->
    fprintf ppf "(";
    Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf " * ") pp_typ ppf ty;
    fprintf ppf ")"
  | TyList ty -> fprintf ppf "%a list" pp_typ ty
  | TyOption ty -> fprintf ppf "%a option" pp_typ ty
  | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
  | Ty_And l ->
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_newline ppf ())
      pp_typ
      ppf
      l
;;

let rec pp_typ_expr ppf = function
  | Some TInt -> pp_print_string ppf "int"
  | Some TBool -> pp_print_string ppf "bool"
  | Some TString -> pp_print_string ppf "string"
  | Some (TTuple ty) ->
    fprintf ppf "(";
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> fprintf ppf " * ")
      pp_typ_expr
      ppf
      (List.rev (trasform_to_some ty));
    fprintf ppf ")"
  | Some (TList ty) -> fprintf ppf "(%a list)" pp_typ_expr (Some ty)
  | Some (TOption ty) -> fprintf ppf "(%a option)" pp_typ_expr (Some ty)
  | Some (TFun (l, r)) ->
    fprintf ppf "(%a -> %a)" pp_typ_expr (Some l) pp_typ_expr (Some r)
  | None -> pp_print_string ppf "No type"

and trasform_to_some ty = List.fold_left (fun acc t -> Some t :: acc) [] ty

let pp_scheme ppf = function
  | S (xs, t) -> fprintf ppf "forall %a . %a" VarSet.pp xs pp_typ t
;;

let pp_expr =
  let rec helper ppf = function
    | EInt n -> fprintf ppf "%d" n
    | EBool n -> fprintf ppf "%b" n
    | EString n -> fprintf ppf "\"%s\"" n
    | ETuple expr ->
      fprintf ppf "(";
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> fprintf ppf ", ")
        helper
        ppf
        (List.rev expr);
      fprintf ppf ")"
    | EList expr ->
      fprintf ppf "[";
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> fprintf ppf "; ")
        helper
        ppf
        (List.rev expr);
      fprintf ppf "]"
    | ESome expr ->
      fprintf ppf "Some ";
      helper ppf expr
    | ENone -> fprintf ppf "None"
    | EFun (_, _) -> fprintf ppf "<fun>"
    | _ -> failwith "Expr printing error"
  in
  helper
;;

let pp_dec ppf ((name, expr), t) =
  fprintf ppf "val %s : " name;
  pp_typ ppf t;
  fprintf ppf " = ";
  pp_expr ppf expr
;;

let pp_val ppf t = function
  | VConst x ->
    fprintf ppf "- : ";
    pp_typ ppf t;
    fprintf ppf " = ";
    pp_expr ppf x
  | VClosure (_, _, x) | VRecClosure (_, _, _, x) ->
    fprintf ppf "- : ";
    pp_typ ppf t;
    fprintf ppf " = ";
    pp_expr ppf x
  | VDeclaration x ->
    (match t with
     | Ty_And l ->
       let com_l = List.combine x l in
       Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_newline ppf ())
         pp_dec
         ppf
         com_l
     | _ -> failwith "Printer: Unexpected Error")
;;
(* | VDeclaration(x) ->
   match t with
   | Ty_And l ->
   let com_l = List.combine x l in
   let _ = Format.pp_print_list
   ~pp_sep:(fun ppf () -> Format.pp_print_newline ppf ())
   pp_dec
   ppf
   (com_l) *)
(* | _ -> (match x with | (n, e) :: [] -> pp_dec ppf ((n,e), t)) *)

(*
   let paired = List.map2 (fun x y -> (x, y)) list1 list2 *)
(*
   | VConst of expr
   | VClosure of string option * value Env.t * expr
   | VRecClosure of string option * value Env.t * string list * expr
   | VDeclaration of (string * expr) list *)
(*   let pp_expr =
     let open Parsetree in
     let rec helper ppf = function
     | EConst n -> fprintf ppf "%d" n
     | EIf (c, th, el) -> fprintf ppf "if %a then %a else %a" helper c helper th helper el
     | EVar s -> pp_print_string ppf s
     | EApp (l, r) -> fprintf ppf "(%a %a)" helper l helper r
     | ELam (PVar name, e) -> fprintf ppf "(fun %s -> %a)" name helper e
     | ELet (_flg, PVar name, body, in_) ->
     fprintf ppf "let %s%s = %a in %a" "?" name helper body helper in_
     in
     helper
     ;; *)
