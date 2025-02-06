open Ast

type type_var = int [@@deriving show { with_path = false }]

type base_type =
  | BInt
  | BBool
  | BUnit
  | BString
[@@deriving show { with_path = false }]

type typ =
  | TBase of base_type
  | TVar of type_var
  | TArrow of typ * typ
  | TTuple of typ * typ * typ list
  | TList of typ
[@@deriving show { with_path = false }]

let ( @-> ) a b = TArrow (a, b)

module TVarSet = Stdlib.Set.Make (Int)
module VarSet = Stdlib.Set.Make (String)

type error =
  | OccursCheckFailed
  | UnificationFailed of typ * typ
  | Unbound of id
  | PatternNameTwice of pattern
  | UnknownType of id
  | SomeError of string
[@@deriving show { with_path = false }]

type scheme = Scheme of TVarSet.t * typ

let tscheme x y = Scheme (x, y)
