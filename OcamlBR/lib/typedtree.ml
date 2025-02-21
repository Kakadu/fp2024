(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_var = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

(* actual types *)
type ty =
  | TPrim of string
  | TVar of type_var
  | TArrow of ty * ty
  | TTuple of ty * ty * ty list
  | TList of ty
  | TOption of ty (* | TRecord of string *)
[@@deriving show { with_path = false }]

let gen_tprim =
  let open QCheck.Gen in
  let tprim = oneofl [ "int"; "string"; "bool"; "unit" ] in
  map (fun t -> TPrim t) tprim
;;

type scheme = S of VarSet.t * ty [@@deriving show { with_path = false }]

(* utility functions *)
let tprim_int = TPrim "int"
let tprim_string = TPrim "string"
let tprim_bool = TPrim "bool"
let tprim_unit = TPrim "unit"
let tarrow l r = TArrow (l, r)
let ( @-> ) = tarrow
let tlist ty = TList ty
(* let trecord s = TRecord s *)

let rec pp_ty ppf =
  let open Format in
  function
  | TVar n -> fprintf ppf "'%d" n
  | TPrim s -> fprintf ppf "%s" s
  | TArrow (l, r) ->
    fprintf
      ppf
      "%a -> %a"
      (fun ppf l ->
        match l with
        | TArrow _ -> fprintf ppf "(%a)" pp_ty l
        | _ -> pp_ty ppf l)
      l
      (fun ppf r ->
        match r with
        | TArrow _ -> fprintf ppf "(%a)" pp_ty r
        | _ -> pp_ty ppf r)
      r
  | TList t ->
    (match t with
     | TArrow _ -> fprintf ppf "(%a) list" pp_ty t
     | _ -> fprintf ppf "%a list" pp_ty t)
  | TTuple (t1, t2, rest) ->
    let tuple_content =
      String.concat " * " (List.map (Format.asprintf "%a" pp_ty) (t1 :: t2 :: rest))
    in
    fprintf ppf "(%s)" tuple_content
  | TOption t -> fprintf ppf "(%a) option" pp_ty t
;;

(* | TRecord s -> fprintf ppf "%s" s *)

(* errors *)
type error =
  [ `Occurs_check of string * ty
  | `Undefined_variable of string
  | `Unification_failed of ty * ty
  | `Ill_left_hand_side of string (* e.g. let 0 = 1, let rec (a, b) = <..> *)
  | `Ill_right_hand_side of string (* e.g. let rec x = x + 1 *)
  | `Duplicate_field_labels of string
  | `Undefined_type of string
  | `Multiple_definition_of_type of string
  | `Unexpected_function_type of ty
  ]

let pp_error ppf = function
  | `Occurs_check (s, t) -> Format.fprintf ppf {|Occurs check failed: %s %a|} s pp_ty t
  | `Undefined_variable s -> Format.fprintf ppf {|Undefined variable %S|} s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf {|Unification failed on %a and %a|} pp_ty l pp_ty r
  | `Ill_left_hand_side s -> Format.fprintf ppf {|Ill left-hand side %s|} s
  | `Ill_right_hand_side s -> Format.fprintf ppf {|Ill right-hand side %s|} s
  | `Duplicate_field_labels s -> Format.fprintf ppf {|Duplicate field labels: %s|} s
  | `Undefined_type s -> Format.fprintf ppf {|Undefined type: %s|} s
  | `Multiple_definition_of_type s ->
    Format.fprintf ppf {|Multiple definition of type name %s|} s
  | `Unexpected_function_type t ->
    Format.fprintf ppf {|Expected function type, got: %a|} pp_ty t
;;
