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

type decl_type = 
| TInt
| TString
| TBool
| TFun of decl_type * decl_type
| TVar of string
| TList of decl_type
| TTuple of decl_type list
[@@deriving eq, show { with_path = false }]

type decl_name = 
| UpName of string (* vars / functions*)
| DownName of string (* types, constructors*)
[@@deriving eq, show { with_path = false }]

type pattern = 
| Ppat_any
| Ppat_var of string decl_name
| Ppat_alias of pattern * string decl_name
| Ppat_constant of constant
| Ppat_interval of constant * constant
| Ppat_tuple of pattern list
| Ppat_construct (* Do we need it?*)
[@@deriving eq, show { with_path = false }]

type rec_flag = 
|	Nonrecursive
|	Recursive

type value_binding = 
  {
    pat: pattern;
    expr: decl_expr;
  }

type case =
{
  left: pattern;
  right: decl_expr;
}

type decl_expr = 
| Exp_constant of constant (** Expressions constant such as [1], ['a'], ["true"]**)
| ExpEmptyList
| ExpVar of decl_name
| Exp_tuple of decl_expr list (** can be changed to [expr*expr*(expr list)] **)
| Exp_function of case list
| Exp_fun of pattern list * decl_expr
| Exp_apply of decl_expr * decl_expr 
| Exp_match of decl_expr * case list
| Exp_try of decl_expr * case list
| ExpIf of decl_expr * decl_expr * decl_expr option
| Exp_Let of rec_flag * value_binding list * decl_expr
| ExpBinOp of binop * decl_expr * decl_expr
[@@deriving eq, show { with_path = false }]

(* I guess ?? should be replaced with smth near to pattern... *)

type decl_rec (* TODO *)
[@@deriving eq, show { with_path = false }]

(* ADT specific *)
type type_decl = decl_name * (decl_name * decl_type) list
[@@deriving eq, show { with_path = false }]