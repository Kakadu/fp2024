type const =
  | Int of int (*18*)
  | Bool of bool (*True*)
  | Unit (* () *)

type 'a maybe =
  | Nothing (* Nothing *)
  | Just of 'a (* Just 5 *)

type tp =
  (* explicit types *)
  | TUnit (* () *)
  | TInt (* Int *)
  | TBool (* Bool*)
  | ListParam of tp (* [Int] *)
  | TupleParams of tp * tp * tp list (* (Int, Bool) *)
  | FunctionType of tp * tp list (*Int -> Bool -> (Int,Bool)*)

type binop =
  | Plus (* + *)
  | Minus (* - *)
  | Divide (* `div` *)
  | Multiply (* * *)
  | Equality (* == *)
  | Pow (* ^ *)
  | Inequality (* /= *)
  | Less (* < *)
  | Greater (* > *)
  | EqualityOrLess (* <= *)
  | EqualityOrGreater (* >= *)

type unop =
  | Minus (* - *)
  | Not (* not *)

type ident =
  | Ident of
      string
      * tp option (* variable's / function's name and optionally its explicit type*)

type listpat =
  | Nil (* [] *)
  | PCons of pattern * pattern list (* x:xs *)
  | PEnum of pattern * pattern list (* [x, y, z] *)

and pattern = ident option * pat (* lst@(_:xs) *)

and pat =
  | PWildcard (* _ *)
  | PConst of const
  | PIdentificator of ident (* x *)
  | PList of listpat
  | PTuple of pattern * pattern * pattern list (* (x, y, z)*)
  | PJust of pattern (* Just x *)
  | PNothing (* Nothing *)
  | PNul (* $*)
  | PNode of pattern * pattern * pattern (* (x; y; z)*)

type comprehension =
  | Condition of expr (* x < 10 *)
  | Generator of (pattern * expr)
(* x <- [1 ..] *)

and ordinarylistbld =
  | ComprehensionList of
      expr
      * comprehension
      * comprehension list (* [x * y | x <- [1, 10, 100], y <- [2, 3]], y mod 2 == 0 *)
  | IncomprehensionlList of expr (* [1,2] *)

and listbld =
  | LazyList of
      expr * expr option * expr option (* [1.. ] or [1..10] or [1, 2 .. 10] or [1, 3..] *)
  | OrdList of ordinarylistbld

and binding =
  | VarsBind of
      pattern * bindingbody * binding list (* x = let y = 12 in y * z where z = 5 *)
  | FunBind of
      ident
      * pattern
      * pattern list
      * bindingbody
      * binding list (* f x y = x + y + z where z = 2 *)

and bindingbody =
  (* examples below are for function binding with due body *)
  | Guards of (expr * expr) * (expr * expr) list
  (* (condition, branch) pairs*)
  (* f x | x > 0 = x | otherwise = -1 *)
  | OrdBody of expr (* f x = if x > 0 then x else -1 *)

and binary_tree_bld =
  | Nul
  (* node that not exists*)
  (* $ *)
  | Node of expr * binary_tree_bld * binary_tree_bld
(* node is data and two 'sons' *)
(* (x^y; $; (2; $; $)) *)

and expr =
  | Const of const
  | Identificator of ident (* x *)
  | TupleBld of expr * expr * expr list (* (1+3, f x) *)
  | OptionBld of expr maybe (* Just (f x) *)
  | ListBld of expr list (* [(2 ^ 2 - 3) ..] *)
  | Binop of expr * binop * expr (* 1 > 0 *)
  | Unop of unop * expr (* (-1) *)
  | IfThenEsle of expr * expr * expr (* if x >= 0 then x else (-x) *)
  | FunctionApply of expr * expr * expr list
  (* sum 1 2 *)
  (* \x -> x + 1) 1 *)
  | Lambda of pattern * pattern list * expr (* \x y -> x + y *)
  | BinTreeBld of binary_tree_bld
  | Case of pattern * (pattern * expr) * (pattern * expr) list
  (* case l of
     (x:xs) -> x
     [] -> 0
  *)
  | InnerBindings of binding * binding list * expr
(*
   let
   x = 1
   y = 2
   in
   x + y
*)
