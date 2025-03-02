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
  | PatList of pattern list
  | PatOption of pattern option
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
  | ExpLet of is_rec * bind * bind list * expr
  | ExpFunction of expr * expr (* ExpFunction(x, y)*)
  | ExpOption of expr option
[@@deriving show { with_path = false }]

and bind = pattern * expr [@@deriving show { with_path = false }]

type structure =
  | SEval of expr
  | SValue of is_rec * bind * bind list
[@@deriving show { with_path = false }]

type program = structure list [@@deriving show { with_path = false }]

let rec collect_vars = function
  | TyVar x -> [ x ]
  | TyPrim _ -> []
  | TyArrow (l, r) -> collect_vars l @ collect_vars r
  | TyList t -> collect_vars t
  | TyTuple ts -> List.concat_map collect_vars ts
  | TyOption t -> collect_vars t
;;

module IntMap = Map.Make (struct
    type t = int

    let compare = compare
  end)

let pp_ty fmt ty =
  let vars = collect_vars ty |> List.sort_uniq compare in
  let var_map =
    List.mapi (fun i x -> x, Char.chr (i + 97)) vars
    |> List.to_seq
    |> Seq.fold_left (fun map (k, v) -> IntMap.add k v map) IntMap.empty
  in
  let rec pp_ty_aux fmt ty =
    match ty with
    | TyVar x ->
      (match IntMap.find_opt x var_map with
       | Some c -> fprintf fmt "%c" c
       | None -> fprintf fmt "?")
    | TyPrim x -> fprintf fmt "%s" x
    | TyArrow (l, r) ->
      let needs_parens = function
        | TyArrow _ -> true
        | _ -> false
      in
      if needs_parens l
      then fprintf fmt "(%a) -> %a" pp_ty_aux l pp_ty_aux r
      else fprintf fmt "%a -> %a" pp_ty_aux l pp_ty_aux r
    | TyTuple tys ->
      fprintf
        fmt
        "(%a)"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt " * ")
           (fun fmt ty ->
             match ty with
             | TyArrow _ -> fprintf fmt "(%a)" pp_ty_aux ty
             | _ -> pp_ty_aux fmt ty))
        tys
    | TyList ty ->
      (match ty with
       | TyArrow _ | TyTuple _ -> fprintf fmt "(%a) list" pp_ty_aux ty
       | _ -> fprintf fmt "%a list" pp_ty_aux ty)
    | TyOption ty ->
      (match ty with
       | TyArrow _ | TyTuple _ -> fprintf fmt "(%a) option" pp_ty_aux ty
       | _ -> fprintf fmt "%a option" pp_ty_aux ty)
  in
  pp_ty_aux fmt ty
;;
