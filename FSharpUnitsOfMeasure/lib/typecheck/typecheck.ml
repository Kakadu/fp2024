(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

type ttype =
  | TIdent of string
  | TFun of ttype * ttype
  | TTuple of ttype * ttype * ttype list
  | TList of ttype
  | TAny of int

let rec place_type e n t =
  match e with
  | TFun (TAny k, tl) when k = n -> TFun (t, place_type tl n t)
  | TAny k when k = n -> t
  | x -> x
;;

type env = (string * ttype) list

let empty : env = [ "+", TFun (TIdent "int", TFun (TIdent "int", TIdent "int")) ]

let rec type_string t =
  match t with
  | TIdent i -> i
  | TFun (a, b) -> type_string a ^ " -> " ^ type_string b
  | TAny n -> "T " ^ string_of_int n
  | TList t -> type_string t ^ " list"
  | TTuple (x, y, ls) -> type_string x ^ " * " ^ type_string y
;;

let rec set_any_type nv env n =
  match env with
  | (name, TAny k) :: tl when k = n -> (name, nv) :: set_any_type nv tl n
  | (name, TFun (x, y)) :: tl ->
    let new_type =
      match x with
      | TAny k when k = n -> TFun (nv, place_type y n nv)
      | _ -> TFun (x, place_type y n nv)
    in
    (name, new_type) :: tl
  | h :: tl -> h :: set_any_type nv tl n
  | [] -> []
;;

let unnify_types env a b =
  if a = b
  then a, env
  else (
    match a with
    | TAny n -> b, set_any_type b env n
    | _ ->
      (match b with
       | TAny n -> a, set_any_type a env n
       | _ -> failwith "TODO: Ununified types"))
;;

type typping_result = ttype * env

let typeof_const const =
  match const with
  | Const_int _ -> TIdent "int"
  | Const_bool _ -> TIdent "bool"
  | Const_float _ -> TIdent "float"
  | Const_string _ -> TIdent "string"
  | _ -> failwith "TODO: I don't have such constant type"
;;

let typeof_ident env idx =
  match List.assoc_opt idx env with
  | Some x -> x
  | None -> failwith "TODO: I don't know this variable"
;;

let rec typeof_typed typ =
  match typ with
  | Type_ident x -> TIdent x
  | Type_func (x, y) -> TFun (typeof_typed x, typeof_typed y)
  | Type_tuple (x, y, ls) ->
    let rec tuple_ls a =
      match a with
      | h :: tl -> typeof_typed h :: tuple_ls tl
      | [] -> []
    in
    TTuple (typeof_typed x, typeof_typed y, tuple_ls ls)
;;

let typeof_ite env typeof t e =
  match e with
  | Some x ->
    let res = typeof env t in
    unnify_types (snd res) (fst res) (fst (typeof env x))
  | None -> typeof env t
;;

let typeof_match env typeof expr main_rule ls_rules =
  let expr_type = fst (typeof env expr) in
  let helper (Rule (p, e)) =
    match p with
    | Pattern_ident_or_op idx ->
      ignore (unnify_types env (TIdent idx) expr_type);
      typeof env e
    | _ -> failwith "TODO: i don't know this pattern"
  in
  let main_res = helper main_rule in
  let rules_fold (t1, env) (Rule (_, e)) =
    let res = typeof env e in
    unnify_types (snd res) t1 (fst res)
  in
  List.fold_left rules_fold main_res ls_rules
;;

let get_res_of_set nv env t =
  match t with
  | TAny n -> set_any_type nv env n
  | _ -> failwith "TODO: bad apply"
;;

let apply_type f_res i_res =
  match fst f_res with
  | TFun (TAny n, e) -> place_type e n (fst i_res), set_any_type (fst i_res) (snd f_res) n
  | TFun (nv, e) when nv = fst i_res -> e, snd f_res
  | TFun (nv, e) -> e, get_res_of_set nv (snd f_res) (fst i_res)
  | TAny n -> TAny n, set_any_type (TFun (fst i_res, TAny n)) (snd f_res) n
  | _ -> failwith "TODO: Un applied type"
;;

let load_pattern pat env tp =
  match pat with
  | Pattern_ident_or_op idx -> (idx, tp) :: env
  | _ -> failwith "TODO: I don't support such patterns 1"
;;

let get_pattern_type pat env =
  match pat with
  | Pattern_ident_or_op idx -> typeof_ident env idx
  | _ -> failwith "TODO: I don't support such patterns 2"
;;

let remove_pattern env pat =
  let rec remove_var env idx =
    match env with
    | (idy, _) :: tl when idy = idx -> tl
    | h :: tl -> h :: remove_var tl idx
    | [] -> []
  in
  match pat with
  | Pattern_ident_or_op x -> remove_var env x
  | _ -> failwith "TODO: I don't support such patterns 3"
;;

let typeof_lam env typeof pat expr =
  let output_res = typeof (load_pattern pat env (TAny (List.length env))) expr in
  let input_type = get_pattern_type pat (snd output_res) in
  TFun (input_type, fst output_res), snd output_res
;;

let typeof_let env typeof rf main_bind ls_bind expr =
  let let_res =
    let helper (Bind (p, e)) =
      if rf = Nonrecursive
      then typeof env e
      else typeof (load_pattern p env (TAny (List.length env))) e
    in
    helper main_bind
  in
  let main_env =
    if rf = Nonrecursive
    then (
      let load_main (Bind (p, _)) = load_pattern p (snd let_res) (fst let_res) in
      load_main main_bind)
    else (
      let load_main (Bind (p, _)) = load_pattern p env (fst let_res) in
      load_main main_bind)
  in
  let new_env =
    List.fold_left
      (fun x (Bind (p, e)) -> load_pattern p x (fst (typeof x e)))
      main_env
      ls_bind
  in
  fst (typeof new_env expr), new_env
;;

let rec typeof env expr =
  match expr with
  | Expr_const const -> typeof_const const, env
  | Expr_ident_or_op idx -> typeof_ident env idx, env
  | Expr_typed (_, typed) -> typeof_typed typed, env
  | Expr_ifthenelse (i, t, e) ->
    ignore (typeof env i);
    typeof_ite env typeof t e
  | Expr_match (expr, main_rule, ls_rules) ->
    typeof_match env typeof expr main_rule ls_rules
  | Expr_tuple (x, y, ls) ->
    let tuple_ls a = List.map (fun x -> fst (typeof env x)) a in
    TTuple (fst (typeof env x), fst (typeof env y), tuple_ls ls), env
  | Expr_apply (func, input) ->
    let fun_res = typeof env func in
    let input_res = typeof (snd fun_res) input in
    apply_type fun_res input_res
  | Expr_lam (pat, expr) -> typeof_lam env typeof pat expr
  | Expr_let (rf, main_bind, ls_bind, expr) ->
    typeof_let env typeof rf main_bind ls_bind expr
  | Expr_list ls ->
    let res =
      List.fold_left
        (fun (t1, _) e -> unnify_types (snd e) t1 (fst e))
        (TAny (-1), env)
        (List.map (typeof env) ls)
    in
    TList (fst res), snd res
  | _ -> failwith "TODO: I don't know this expr"
;;

let typecheck t =
  ignore (typeof empty t);
  t
;;
