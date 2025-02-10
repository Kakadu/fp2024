[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Types

let pp_base_type_my fmt = function
  | BInt -> Format.fprintf fmt "int"
  | BBool -> Format.fprintf fmt "bool"
  | BUnit -> Format.fprintf fmt "unit"
  | BString -> Format.fprintf fmt "string"
;;

let minimize_variable t =
  let map =
    let rec helper (min, map) = function
      | TVar v when Base.Map.mem map v -> min, map
      | TVar v -> min + 1, Base.Map.add_exn map ~key:v ~data:min
      | TArrow (l, r) ->
        let min, map = helper (min, map) l in
        let min, map = helper (min, map) r in
        min, map
      | TTuple (f, s, xs) -> List.fold_left helper (min, map) (f :: s :: xs)
      | TList l -> helper (min, map) l
      | TOption o -> helper (min, map) o
      | TBase _ -> min, map
    in
    helper (0, Base.Map.empty (module Base.Int)) t |> snd
  in
  let rec helper = function
    | TVar v -> TVar (Base.Map.find_exn map v)
    | TArrow (l, r) -> TArrow (helper l, helper r)
    | TTuple (f, s, xs) -> TTuple (helper f, helper s, List.map helper xs)
    | TList l -> TList (helper l)
    | TOption o -> TOption (helper o)
    | TBase b -> TBase b
  in
  helper t
;;

let pp_typ_my fmt t =
  let t = if Config.vars_min then minimize_variable t else t in
  let rec helper fmt = function
    | TBase b -> pp_base_type_my fmt b
    | TVar v when Config.vars_char ->
      Format.fprintf fmt "'%c" (Char.chr (Char.code 'a' + v))
    | TVar v -> Format.fprintf fmt "'%s" (string_of_int v)
    | TArrow ((TArrow (_, _) as l), r) ->
      Format.fprintf fmt "(%a) -> %a" helper l helper r
    | TArrow (l, r) -> Format.fprintf fmt "%a -> %a" helper l helper r
    | TTuple (f, s, xs) ->
      Format.fprintf
        fmt
        "%a"
        (Format.pp_print_list
           ~pp_sep:(fun _ _ -> Format.printf " * ")
           (fun fmt ty ->
             match ty with
             | TBase _ | TVar _ -> Format.fprintf fmt "%a" helper ty
             | _ -> Format.fprintf fmt "(%a)" helper ty))
        (f :: s :: xs)
    | TList l ->
      (match l with
       | TBase _ | TVar _ -> Format.fprintf fmt "%a list" helper l
       | _ -> Format.fprintf fmt "(%a) list" helper l)
    | TOption o ->
      (match o with
       | TBase _ | TVar _ -> Format.fprintf fmt "%a option" helper o
       | _ -> Format.fprintf fmt "(%a) option" helper o)
  in
  helper fmt t
;;

let pp_error_my fmt e =
  match e with
  | UnificationFailed (f, s) ->
    Format.fprintf fmt "Can't unify (%a) and (%a)" pp_typ_my f pp_typ_my s
  | Unbound id -> Format.fprintf fmt "Unbound value %s" id
  | PatternNameTwice id -> Format.fprintf fmt "Variable %s is bound several time" id
  | _ as e -> Format.printf "%a" pp_error e
;;

let print_typ ?(name = "typ") t = Format.printf "%s: %a\n" name pp_typ_my t

let%expect_test "just type" =
  Format.printf "%a" pp_typ_my (TVar 4);
  [%expect {| 'a |}]
;;

let%expect_test "just arrow type" =
  Format.printf "%a" pp_typ_my (TArrow (TVar 4, TArrow (TVar 3, TVar 4)));
  [%expect {| 'a -> 'b -> 'a |}]
;;

let%expect_test "super arrow type" =
  Format.printf "%a" pp_typ_my (TArrow (TVar 2, TTuple (TVar 1, TVar 2, [ TVar 5 ])));
  [%expect {| 'a -> 'b * 'a * 'c |}]
;;

let%expect_test "ultra hard arrow type" =
  Format.printf
    "%a"
    pp_typ_my
    (TArrow (TVar 2, TTuple (TVar 1, TVar 2, [ TVar 5; TList (TVar 2) ])));
  [%expect {| 'a -> 'b * 'a * 'c * ('a list) |}]
;;

let%expect_test "option ." =
  Format.printf "%a" pp_typ_my (TOption (TVar 2));
  [%expect {| 'a option |}]
;;
