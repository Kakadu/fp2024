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
| TVar of decl_type
| TList of decl_type
| TTuple of decl_type * decl_type (*list of decl_type, im dumb*)

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

type match_pattern (* TODO *)

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
| ExpIf of decl_expr * decl_expr * decl_expr 
| Exp_Let of rec_flag * value_binding list * decl_expr
| ExpBinOp of binop * decl_expr * decl_expr

let expint num = ExpInt num
let expstring str = ExpString str
let expbool bool = ExpBool bool
let expemptylist = ExpEmptyList
let expvar decl_name = ExpVar (decl_name)
let exptuple declaration_expression = ExpTuple declaration_expression
let expfun ??
let expmatch decl_exp (match_pattern * decl_exp) list = ExpMatch (decl_exp, match_pattern, decl_exp)
let expif decl_exp1 decl_exp2 decl_exp3 = ExpIf (decl_exp1, decl_exp2, decl_exp3) (* Statement is also a expression *)
let explet decl_name decl_exp = ExpLet (decl_name, decl_exp)
let expbinop binop decl_exp decl_exp = ExpBinOp (binop, decl_exp, decl_exp)

(* I guess ?? should be replaced with smth near to pattern... *)

type decl_rec (* TODO *)
type decl_type (* TODO *)