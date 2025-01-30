open Format

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

(*maybe change name*)
type typ =
  | Typ_prim of string
  | Typ_var of binder
  | Typ_arrow of typ * typ
  | Typ_tuple of typ list
  | Typ_list of typ
  | Typ_unit
[@@deriving show { with_path = false }]

type scheme = Forall of binder_set * typ [@@deriving show { with_path = false }]

let int_typ = Typ_prim "int"
let ch_typ = Typ_prim "char"
let string_typ = Typ_prim "string"
let bool_typ = Typ_prim "bool"
let var_typ x = Typ_var x
let arrow_typ l r = Typ_arrow (l, r)
let ( @-> ) = arrow_typ
let list_typ typ = Typ_list typ
let tuple_typ typ_list = Typ_tuple typ_list

(*printers*)
(*maybe use pprinter ??*)
let rec pprint_type_tuple fmt = function
  | [] -> ()
  | [ h ] ->
    (match h with
     | Typ_arrow (_, _) -> fprintf fmt "(%a)" pprint_type h
     | _ -> fprintf fmt "%a" pprint_type h)
  | h :: tl ->
    (match h with
     | Typ_arrow (_, _) -> fprintf fmt "(%a) * %a" pprint_type h pprint_type_tuple tl
     | _ -> fprintf fmt "%a * %a" pprint_type h pprint_type_tuple tl)

and pprint_type fmt = function
  | Typ_var num -> fprintf fmt "'%d" num
  | Typ_prim str -> fprintf fmt "%s" str
  | Typ_arrow (ty1, ty2) ->
    (match ty1, ty2 with
     | Typ_arrow (_, _), _ -> fprintf fmt "(%a) -> %a" pprint_type ty1 pprint_type ty2
     | _ -> fprintf fmt "%a -> %a" pprint_type ty1 pprint_type ty2)
  | Typ_tuple ty_lst -> fprintf fmt "%a" pprint_type_tuple ty_lst
  | Typ_list ty1 -> fprintf fmt "%a list" pprint_type ty1
;;

(*errors*)

type error =
  [ `Occurs_check
  | `Unification_failed of typ * typ
  | `Wrong_exp
  | `Wrong_type
  | `Wrong_Const
  | `Wrong_stritem
  | `Unbound_adt_type of string
  | `Unbound_variable of string
  | `Pattern_matching_failed
  ]

let pp_inf_err fmt = function
  | `Occurs_check -> fprintf fmt "Occurs_check"
  | `Unification_failed (typ1, typ2) ->
    fprintf fmt "Unification_failed: %a # %a" pprint_type typ1 pprint_type typ2
  | `Wrong_exp -> fprintf fmt "Wrong_exp"
  | `Wrong_type -> fprintf fmt "Wrong_type"
  | `Wrong_Const -> fprintf fmt "Wrong_const"
  | `Wrong_stritem -> fprintf fmt "Wrong_stritem"
  | `Unbound_adt_type str -> fprintf fmt "Unbound_adt_type: %S" str
  | `Unbound_variable str -> fprintf fmt "Unbound_variable: %S" str
  | `Pattern_matching_failed -> fprintf fmt "Pattern_matching_failed"
;;
