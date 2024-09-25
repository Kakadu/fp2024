type constant =
  | Const_integer of int (** integer as [52] **)
  | Const_char of char (** char as ['w'] **)
  | Const_string of string (** string as ["Kakadu"] **)

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

(**Misha we can use  https://github.com/ocaml-ppx/ppx_deriving for generating this code if we are nerds**)
let bop_add = Add
let bop_sub = Sub
let bop_mul = Mul
let bop_div = Div
let bop_eq = Eq
let bop_and = And 
let bop_or = Or
let bop_cons = Cons
let bop_neq = Neq
let bio_les = Les
let bio_leq = Leq
let bio_gre = Gre
let bio_geq = Geq

type decl_type = 
| TInt
| TString
| TBool
| TFun of decl_type * decl_type
| TVar of string
| TList of decl_type
| TTuple of decl_type list

let tint = TInt
let tstring = TString
let tbool = TBool
let tfun = TFun
let tvar = TVar (*drozhashiaia*)
let tlist = TList
let ttuple = TTuple

type decl_name = 
| UpName of string (* vars / functions*)
| DownName of string (* types, constructors*)

let upname str = UpName str
let downname str = DownName str

type pattern = 
| Ppat_any
| Ppat_var of string decl_name
| Ppat_alias of pattern * string decl_name
| Ppat_constant of constant
| Ppat_interval of constant * constant
| Ppat_tuple of pattern list
| Ppat_construct (* Do we need it?*)

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

let expint num = ExpInt num
let expstring str = ExpString str
let expbool bool = ExpBool bool
let expemptylist = ExpEmptyList
let expvar decl_name = ExpVar (decl_name)
let exptuple declaration_expression = ExpTuple declaration_expression
let expfun ??
let expmatch decl_exp (match_pattern * decl_exp) list = ExpMatch (decl_exp, match_pattern, decl_exp) (* Matched value, pattern, expresion after pattern match*)
let expif decl_exp1 decl_exp2 decl_exp3 = ExpIf (decl_exp1, decl_exp2, decl_exp3) (* Statement is also a expression *)
let explet decl_name decl_exp1 decl_exp2 = ExpLet (decl_name, decl_exp1, decl_exp2)
let expbinop binop decl_exp decl_exp = ExpBinOp (binop, decl_exp, decl_exp)

(* I guess ?? should be replaced with smth near to pattern... *)

type decl_rec (* TODO *)

(* ADT specific *)
type type_decl = decl_name * (decl_name * decl_type) list