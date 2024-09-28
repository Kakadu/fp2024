type constant =
  | Const_integer of int (** integer as [52] **)
  | Const_char of char (** char as ['w'] **)
  | Const_string of string (** string as ["Kakadu"] **)
[@@deriving eq, show { with_path = false }]
(* Generates fun's for equality check, string type output and removing the full path to module*)

type binop =
  | Add (* + *)
  | Sub (* - *)
  | Mul (* * *)
  | Div (* / *)
  | Eq (* = *)
  | And (* && *)
  | Or (* || *)
  | Cons (* :: *)
  | Neq (* <> *)
  | Les (* < *)
  | Leq (* <= *)
  | Gre (* > *)
  | Geq (* >= *)
[@@deriving eq, show { with_path = false }]

type decl_name =
  | Up_name of string (* vars / functions*)
  | Down_name of string (* types, constructors*)
[@@deriving eq, show { with_path = false }]

type pattern =
  | Pat_any
  | Pat_var of decl_name
  | Pat_alias of pattern * decl_name
  | Pat_constant of constant
  | Pat_interval of constant * constant
  | Pat_tuple of pattern list
  | Pat_construct of decl_name * pattern option (*wtf decl_name????????*)
[@@deriving eq, show { with_path = false }]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving eq, show { with_path = false }]

type decl_expr =
  | Exp_constant of constant (** Expressions constant such as [1], ['a'], ["true"]**)
  | Exp_emptyList
  | Exp_var of decl_name
  | Exp_tuple of decl_expr list (** can be changed to [expr*expr*(expr list)] **)
  | Exp_function of case list
  | Exp_fun of pattern list * decl_expr
  | Exp_apply of decl_expr * decl_expr
  | Exp_match of decl_expr * case list
  | Exp_try of decl_expr * case list
  | Exp_if of decl_expr * decl_expr * decl_expr option
  | Exp_let of rec_flag * value_binding list * decl_expr
  | Exp_binop of binop * decl_expr * decl_expr
  | Exp_construct of decl_expr option
[@@deriving eq, show { with_path = false }]

and value_binding =
  { pat : pattern
  ; expr : decl_expr
  }

and case =
  { left : pattern
  ; right : decl_expr
  }

type decl_type =
  | Type_int
  | Type_string
  | Type_bool
  | Type_fun of decl_type * decl_type
  | Type_var of string
  | Type_list of decl_type
  | Type_tuple of decl_type list
  | Type_variant of (decl_name * decl_type) list
[@@deriving eq, show { with_path = false }]
