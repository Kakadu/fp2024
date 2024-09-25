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

type decl_expr = 
| ExpInt of int
| ExpString of string
| ExpBool of bool
| ExpEmptyList
| ExpVar of decl_name
| ExpTuple of expr list
| ExpFun of ?? * declaration_expr
| ExpMatch of decl_expr * (match_pattern * decl_expr)
| ExpIf of ?? * decl_expr * decl_expr (* describe the statement*)
| ExpLet of decl_name * decl_expr (* *)
| ExpBinOp of binop * decl_expr * decl_expr

let expint num = ExpInt num
let expstring str = ExpString str
let expbool bool = ExpBool bool
let expemptylist = ExpEmptyList
let expvar decl_name = ExpVar (decl_name)
let exptuple declaration_expression = ExpTuple declaration_expression
let expfun ?? decl_expr = ExpFun (??, decl_expr)
let expmatch decl_exp (match_pattern * decl_expr) list = ExpMatch (decl_exp, match_pattern, decl_exp)
let expif decl_exp1 decl_exp2 decl_exp3 = ExpIf (decl_exp1, decl_exp2, decl_exp3) (* Statement is also a expression *)
let explet decl_name decl_exp = ExpLet (decl_name, decl_exp)
let expbinop binop decl_exp decl_exp = ExpBinOp (binop, decl_exp, decl_exp)

(* I guess ?? should be replaced with smth near to pattern... *)

type decl_rec (* TODO *)
type decl_type (* TODO *)