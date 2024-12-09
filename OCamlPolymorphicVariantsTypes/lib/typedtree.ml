type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp fmt s =
    Format.fprintf fmt "[ ";
    iter (Format.fprintf fmt "%d; ") s;
    Format.fprintf fmt "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TPrim of string
  | TVar of binder
  | TArrow of ty * ty
  | TTuple of ty list
  | TList of ty
[@@deriving show { with_path = false }]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let ty_int = TPrim "int"
let ty_bool = TPrim "bool"
let ty_string = TPrim "string"
let ty_unit = TPrim "unit"
let ty_arrow l r = TArrow (l, r)
let ty_var v = TVar v
let ty_tuple t = TTuple t
let ty_list t = TList t

let rec pp_ty fmt = function
  | TPrim s -> Format.fprintf fmt "%S" s
  | TVar v -> Format.fprintf fmt "'%d" v
  | TArrow (l, r) -> Format.fprintf fmt "(%a -> %a)" pp_ty l pp_ty r
  | TTuple t ->
    Format.fprintf
      fmt
      "(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun _ _ -> Format.fprintf fmt " * ")
         (fun fmt ty ->
           match ty with
           | TArrow _ -> Format.fprintf fmt "(%a)" pp_ty ty
           | _ -> Format.fprintf fmt "%a" pp_ty ty))
      t
  | TList t ->
    Format.fprintf
      fmt
      "%a list"
      (fun ppf ty ->
        match ty with
        | TArrow _ -> Format.fprintf ppf "(%a)" pp_ty ty
        | _ -> Format.fprintf ppf "%a" pp_ty ty)
      t
;;
