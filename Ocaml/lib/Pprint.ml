(** Copyright 2021-2023, Daniil Kadochnikov *)

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
    | EString n -> fprintf ppf "%S" n
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
    | _ -> fprintf ppf "PPrint: unexpected error while printing expr"
  in
  helper
;;

let pp_dec ppf ((name, expr), t) =
  fprintf ppf "val %s : " name;
  pp_typ ppf t;
  fprintf ppf " = ";
  pp_expr ppf expr
;;

let print_type ppf = function
  | t :: [] -> pp_typ ppf t
  | _ -> fprintf ppf "PPrint: unexpected error while uncovering value for printing"
;;

let pp_val ppf t = function
  | VConst x ->
    fprintf ppf "- : ";
    print_type ppf t;
    fprintf ppf " = ";
    pp_expr ppf x
  | VClosure (_, _, x) | VRecClosure (_, _, _, x) ->
    fprintf ppf "- : ";
    print_type ppf t;
    fprintf ppf " = ";
    pp_expr ppf x
  | VDeclaration x ->
    let com_l = List.combine x t in
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_newline ppf ())
      pp_dec
      ppf
      com_l
;;
