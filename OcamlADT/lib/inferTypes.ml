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
    | [ ty ] -> pprint_type_with_parens_if_tuple fmt ty
    | ty :: rest ->
      fprintf fmt "%a %a" (pprint_type_with_parens_if_tuple ~m) ty print_types rest
  in
  print_types fmt ty_list

and pprint_type_with_parens_if_tuple ?(m = Map.empty (module String)) fmt ty =
  match ty with
  | Type_tuple _ -> fprintf fmt "(%a)" (pprint_type ~m) ty
  | _ -> pprint_type fmt ty
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

let pp_inf_err ?(m = Map.empty (module String))  fmt = function
  | `Occurs_check (string, t) ->
    fprintf fmt "Occurs_check: %s and %a\n" string (pprint_type ~m) t
  | `Unification_failed (typ1, typ2) ->
    fprintf fmt "Unification_failed: %a # %a" (pprint_type ~m) typ1 (pprint_type ~m) typ2
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
