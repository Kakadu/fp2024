(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Ast.TypeExpr
open Stdlib

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Set.Make (String)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%s; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]
type scheme = Forall of binder_set * t [@@deriving show { with_path = false }]

open Base

(* get polymorphic type names from VarSet *)
let binder_to_list args =
  let args = VarSet.elements args in
  List.sort (List.map args ~f:Int.of_string) ~compare:Int.compare
;;

(** turn ['2, '5, '1231, ...] (value is not important, only order) list of
    names of polymorphic types into ['a, 'b, 'c ... ]
    when english alphabet is out, turn values into  ['aa, 'bb, ...] and etc.*)
let minimize dargs =
  let counter = 0 in
  let coef = 0 in
  let m = Map.empty (module Base.String) in
  List.fold_left dargs ~init:(m, coef, counter) ~f:(fun (m, coef, counter) el ->
    let str =
      let rec build coef counter str =
        if coef = 0
        then str ^ Char.escaped (Stdlib.Char.chr (counter + 97))
        else build (coef - 1) counter (str ^ Char.escaped (Stdlib.Char.chr (counter + 97)))
      in
      build coef counter ""
    in
    let counter = counter + 1 in
    let coef = coef + (counter / 26) in
    let counter = counter % 26 in
    let el = Stdlib.string_of_int el in
    Base.Map.set m ~key:el ~data:str, coef, counter)
;;

let rec pprint_type_tuple ?(poly_names_map = Map.empty (module String)) fmt = function
  | [] -> ()
  | [ h ] ->
    (match h with
     | Type_arrow (_, _) -> fprintf fmt "(%a)" (pprint_type ~poly_names_map) h
     | _ -> fprintf fmt "%a" (pprint_type ~poly_names_map) h)
  | h :: tl ->
    (match h with
     | Type_arrow (_, _) ->
       fprintf
         fmt
         "(%a) * %a"
         (pprint_type ~poly_names_map)
         h
         (pprint_type_tuple ~poly_names_map)
         tl
     | _ ->
       fprintf
         fmt
         "%a * %a"
         (pprint_type ~poly_names_map)
         h
         (pprint_type_tuple ~poly_names_map)
         tl)

and pprint_type ?(poly_names_map = Map.empty (module String)) fmt = function
  | Type_var num ->
    (match Map.find poly_names_map num with
     | Some k -> fprintf fmt "'%s" k
     | None -> fprintf fmt "'%s" num)
  | Type_arrow (ty1, ty2) ->
    (match ty1, ty2 with
     | Type_arrow (_, _), _ ->
       fprintf
         fmt
         "(%a) -> %a"
         (pprint_type ~poly_names_map)
         ty1
         (pprint_type ~poly_names_map)
         ty2
     | _ ->
       fprintf
         fmt
         "%a -> %a"
         (pprint_type ~poly_names_map)
         ty1
         (pprint_type ~poly_names_map)
         ty2)
  | Type_tuple (t1, t2, ty_lst) ->
    fprintf fmt "%a" (pprint_type_tuple ~poly_names_map) (t1 :: t2 :: ty_lst)
  | Type_construct (name, []) -> fprintf fmt "%s" name
  | Type_construct (name, ty_list) ->
    fprintf fmt "%a %s" (pprint_type_list_with_parens ~poly_names_map) ty_list name

and pprint_type_list_with_parens ?(poly_names_map = Map.empty (module String)) fmt ty_list
  =
  let rec print_types fmt = function
    | [] -> ()
    | [ ty ] -> (pprint_type_with_parens_if_tuple ~poly_names_map) fmt ty
    | ty :: rest ->
      fprintf
        fmt
        "%a %a"
        (pprint_type_with_parens_if_tuple ~poly_names_map)
        ty
        print_types
        rest
  in
  print_types fmt ty_list

and pprint_type_with_parens_if_tuple ?(poly_names_map = Map.empty (module String)) fmt ty =
  match ty with
  | Type_tuple _ -> fprintf fmt "(%a)" (pprint_type ~poly_names_map) ty
  | _ -> (pprint_type ~poly_names_map) fmt ty
;;

(*errors*)
type error =
  | Occurs_check of string * Ast.TypeExpr.t
  (** same polymotphic type occured while substitution apply ['a : 'a -> 'b]*)
  | Unification_failed of Ast.TypeExpr.t * Ast.TypeExpr.t
  | Unbound_variable of string
  | Arity_mismatch
  (** mismatch of types arity
      [type 'a foo = Foo
      type bar = Bar of foo] *)
  | Undeclared_type of string
  | Wrong_rec (** invalid right value in recursive let declaration *)
  | Unsupported_operator of string (** for binary operators*)
  | Incorrect_list_lengths

let collect_type_vars typ =
  let rec aux acc = function
    | Type_var num -> num :: acc
    | Type_arrow (t1, t2) -> aux (aux acc t1) t2
    | Type_tuple (t1, t2, tl) -> List.fold_left ~f:aux ~init:(aux (aux acc t1) t2) tl
    | Type_construct (_, ty_list) -> List.fold_left ~f:aux ~init:acc ty_list
  in
  aux [] typ
;;

let collect_vars_from_error = function
  | Occurs_check (str, typ) -> str :: collect_type_vars typ
  | Unification_failed (t1, t2) -> collect_type_vars t1 @ collect_type_vars t2
  | _ -> []
;;

let pp_inf_err fmt err =
  let type_vars = collect_vars_from_error err in
  let var_map, _, _ = minimize (List.map type_vars ~f:Stdlib.int_of_string) in
  match err with
  | Occurs_check (str, t) ->
    fprintf
      fmt
      "Occurs_check: %a and %a\n"
      (pprint_type ~poly_names_map:var_map)
      (Type_var str)
      (pprint_type ~poly_names_map:var_map)
      t
  | Unification_failed (typ1, typ2) ->
    fprintf
      fmt
      "Unification_failed: %a # %a"
      (pprint_type ~poly_names_map:var_map)
      typ1
      (pprint_type ~poly_names_map:var_map)
      typ2
  | Unbound_variable str -> fprintf fmt "Unbound_variable: %S" str
  | Arity_mismatch -> fprintf fmt "Arity_mismatch"
  | Undeclared_type str -> fprintf fmt "Undeclared_type: %S" str
  | Wrong_rec -> fprintf fmt "Wrong right value in rec"
  | Unsupported_operator op -> fprintf fmt "Operator %s is not supported" op
  | Incorrect_list_lengths -> fprintf fmt "Lists have unequal lengths"
;;
