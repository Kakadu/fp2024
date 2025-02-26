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

let binder_to_list args =
  let args = VarSet.elements args in
    Base.List.sort (Base.List.map args ~f:int_of_string) ~compare:Stdlib.Int.compare
  

let minimizer dargs =
  let counter = 0 in
  let coef = 0 in
  let m = Base.Map.empty (module Base.String) in
  Base.List.fold_left dargs ~init:(m, coef, counter) ~f:(fun (m, coef, counter) el ->
    let str =
      let rec build coef counter str =
        if coef = 0
        then str ^ Stdlib.Char.escaped (Stdlib.Char.chr (counter + 97))
        else
          build
            (coef - 1)
            counter
            (str ^ Stdlib.Char.escaped (Stdlib.Char.chr (counter + 97)))
      in
      build coef counter ""
    in
    let counter = counter + 1 in
    let coef = coef + (counter / 26) in
    let counter = counter mod 26 in
    let el = Stdlib.string_of_int el in
    Base.Map.set m ~key:el ~data:str, coef, counter)
;;

let rec pprint_type_tuple ?(m = Map.empty (module String)) fmt = function
  | [] -> ()
  | [ h ] ->
    (match h with
     | Type_arrow (_, _) -> fprintf fmt "(%a)" (pprint_type ~m) h
     | _ -> fprintf fmt "%a" (pprint_type ~m) h)
  | h :: tl ->
    (match h with
     | Type_arrow (_, _) ->
       fprintf fmt "(%a) * %a" (pprint_type ~m) h (pprint_type_tuple ~m) tl
     | _ -> fprintf fmt "%a * %a" (pprint_type ~m) h (pprint_type_tuple ~m) tl)

and pprint_type ?(m = Map.empty (module String)) fmt = function
  | Type_var num -> 
    (* let _ =  Base.Map.iteri m ~f:(fun ~key ~data ->
      Format.fprintf fmt "Key: %s, Value: %s\n" key data) in *)
      (* let _ = printf  "Key %s\n" num  in *)
    (match Map.find m num with
    | Some k ->  fprintf fmt "'%s" k
    | None -> fprintf fmt "'%s" num )
   
  | Type_arrow (ty1, ty2) ->
    (match ty1, ty2 with
     | Type_arrow (_, _), _ ->
       fprintf fmt "(%a) -> %a" (pprint_type ~m) ty1 (pprint_type ~m) ty2
     | _ -> fprintf fmt "%a -> %a" (pprint_type ~m) ty1 (pprint_type ~m) ty2)
  | Type_tuple (t1, t2, ty_lst) ->
    fprintf fmt "%a" (pprint_type_tuple ~m) (t1 :: t2 :: ty_lst)
  | Type_construct (name, []) -> fprintf fmt "%s" name
  | Type_construct (name, ty_list) ->
    fprintf fmt "%a %s" (pprint_type_list_with_parens ~m) ty_list name

and pprint_type_list_with_parens ?(m = Map.empty (module String)) fmt ty_list =
  let rec print_types fmt = function
    | [] -> ()
    | [ ty ] -> (pprint_type_with_parens_if_tuple ~m) fmt ty
    | ty :: rest ->
      fprintf fmt "%a %a" (pprint_type_with_parens_if_tuple ~m) ty print_types rest
  in
  print_types fmt ty_list

and pprint_type_with_parens_if_tuple ?(m = Map.empty (module String)) fmt ty =
  match ty with
  | Type_tuple _ -> fprintf fmt "(%a)" (pprint_type ~m) ty
  | _ -> (pprint_type ~m) fmt ty
;;

(*errors*)

type error =
  [ `Occurs_check of string * t
  | `Unification_failed of t * t
  | `Wrong_exp
  | `Wrong_type
  | `Wrong_Const
  | `Wrong_stritem
  | `Unbound_adt_type of string
  | `Unbound_variable of string
  | `Pattern_matching_failed
  | `Arity_mismatch
  | `Undeclared_type of string
  | `Not_supported
  | `Wrong_rec
  ]

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
    | `Occurs_check (str, typ) -> str :: collect_type_vars typ 
    | `Unification_failed (t1, t2) -> collect_type_vars t1 @ collect_type_vars t2
    | _ -> []
  ;;

  let pp_inf_err fmt err =
    let type_vars = collect_vars_from_error err in
    let var_map, _, _ = minimizer (List.map type_vars ~f:Stdlib.int_of_string) in
    match err with
    | `Occurs_check (str, t) ->
      fprintf fmt "Occurs_check: %a and %a\n" (pprint_type ~m:var_map) (Type_var str) (pprint_type ~m:var_map) t
    | `Unification_failed (typ1, typ2) ->
      fprintf fmt "Unification_failed: %a # %a" (pprint_type ~m:var_map) typ1 (pprint_type ~m:var_map) typ2
    | `Wrong_exp -> fprintf fmt "Wrong_exp"
    | `Wrong_type -> fprintf fmt "Wrong_type"
    | `Wrong_Const -> fprintf fmt "Wrong_const"
    | `Wrong_stritem -> fprintf fmt "Wrong_stritem"
    | `Unbound_adt_type str -> fprintf fmt "Unbound_adt_type: %S" str
    | `Unbound_variable str -> fprintf fmt "Unbound_variable: %S" str
    | `Pattern_matching_failed -> fprintf fmt "Pattern_matching_failed"
    | `Arity_mismatch -> fprintf fmt "Arity_mismatch"
    | `Undeclared_type str -> fprintf fmt "Undeclared_type: %S" str
    | `Not_supported -> fprintf fmt "Not supported syntax"
    | `Wrong_rec -> fprintf fmt "Wrong rec"
  ;;
