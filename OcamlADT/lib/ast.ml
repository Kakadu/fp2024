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
| EInt of int
| EString of string
| EBool of bool
| EEmptyList
| EVar of decl_name
| ETuple of expr list
| EFun of ?? * declaration_expr
| EMatch of decl_expr * (match_pattern * decl_expr)
| EIf of decl_expr * decl_expr * decl_expr (* здесь бы всунуть стэйтмент*)
| ELet of decl_name * decl_expr (* *)
| EBinOp of binop * decl_expr * decl_expr

let eint int = EInt int
let estring string = EString string
let ebool bool = EBool bool
let eemptylist = EEmptyList
let evar var = EVar var
let etuple declaration_expression = ETuple declaration_expression
let efun match_pattern decl_expr = EFun (match_pattern, decl_expr)
let ematch (* TODO *)
let eif (* TODO *)
let elet (* TODO *)
let ebinop binop decl_exp decl_exp = EBinOp (binop, decl_exp, decl_exp)


type decl_rec (* TODO *)
type decl_type (* TODO *)