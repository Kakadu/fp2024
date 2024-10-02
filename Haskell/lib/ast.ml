type const =
  | Int of int (** e.g. 18 *)
  | Bool of bool (** e.g. True*)
  | Unit (** () *)

type 'a maybe =
  | Nothing (** Nothing *)
  | Just of 'a (** e.g. Just 5 *)

(** explicit type indication*)
type tp =
  | TUnit (** () *)
  | TInt (** Int *)
  | TBool (** Bool *)
  | TreeParam of tp (** e.g. {Int} *)
  | ListParam of tp (** e.g. "[Int]" *)
  | TupleParams of tp * tp * tp list (** e.g.  (Int, Bool) *)
  | FunctionType of tp * tp * tp list (** e.g. Int -> Bool -> (Int,Bool) *)

type binop =
  | Plus (** + *)
  | Minus (** - *)
  | Divide (** `div` *)
  | Multiply (** () * *)
  | Equality (** == *)
  | Pow (** ^ *)
  | Inequality (** /= *)
  | Less (** < *)
  | Greater (** > *)
  | EqualityOrLess (** <= *)
  | EqualityOrGreater (** >= *)

type unop =
  | Minus (** - *)
  | Not (** not *)

(** variable's / function's name*)
type ident = Ident of string

type listpat =
  | Nil (** "[]" *)
  | PCons of pattern * pattern list (** e.g. x:xs *)
  | PEnum of pattern * pattern list (** e.g.  "[x, y, z]" *)

(** e.g. "lst@(_:xs) :: [Int]" *)
and pattern = ident option * pat * tp option

and pat =
  | PWildcard (** _ *)
  | PConst of const
  | PIdentificator of ident (** e.g. x *)
  | PList of listpat
  | PTuple of pattern * pattern * pattern list (** e.g. (x, y, z)*)
  | PJust of pattern (** e.g. Just x *)
  | PNothing (** e.g. Nothing *)
  | PNul (** nul tree i.e. $ *)
  | PNode of pattern * pattern * pattern (** tree's node i.e (x; y; z)*)

type comprehension =
  | Condition of expr (** e.g. x < 10 *)
  | Generator of (pattern * expr) (** e.g. "x <- [1 ..]" *)

and ordinarylistbld =
  | ComprehensionList of expr * comprehension * comprehension list
  (** e.g "[x * y | x <- [1, 10, 100], y <- [2, 3]], y mod 2 == 0" *)
  | IncomprehensionlList of expr (**e.g. "[1,2]" *)

and listbld =
  | LazyList of expr * expr option * expr option
  (** e.g. "[1.. ] or [1..10] or [1, 2 .. 10] or [1, 3..]" *)
  | OrdList of ordinarylistbld

and binding =
  | VarsBind of pattern * bindingbody * binding list
  (** e.g x = let y = 12 in y * z where z = 5 *)
  | FunBind of ident * pattern * pattern list * bindingbody * binding list
  (** e.g f x y = x + y + z where z = 2 *)

(** examples below are for function binding with due body *)
and bindingbody =
  | Guards of (expr * expr) * (expr * expr) list
  (** (condition, branch) pairs e.g f x | x > 0 = x | otherwise = -1 *)
  | OrdBody of expr (** e.g f x = if x > 0 then x else -1 *)

and binary_tree_bld =
  | Nul (** node that not exists (notation: $) *)
  | Node of expr * binary_tree_bld * binary_tree_bld
  (** node is data and two 'sons' e.g  (x^y; $; (2; $; $)) *)

and expression =
  | Const of const
  | Identificator of ident (** e.g  x *)
  | TupleBld of expr * expr * expr list (** e.g  (1+3, f x) *)
  | OptionBld of expr maybe (** e.g  Just (f x) *)
  | ListBld of expr list (** e.g "[(2 ^ 2 - 3) ..]" *)
  | Binop of expr * binop * expr (** e.g 1 > 0 *)
  | Unop of unop * expr (** e.g (-1) *)
  | IfThenEsle of expr * expr * expr (** e.g if x >= 0 then x else (-x) *)
  | FunctionApply of expr * expr * expr list (** e.g. sum 1 2 or \x -> x + 1) 1 *)
  | Lambda of pattern * pattern list * expr (** e.g. \x y -> x + y *)
  | BinTreeBld of binary_tree_bld
  | Case of pattern * (pattern * expr) * (pattern * expr) list
  (** e.g case l of
      (x:xs) -> x
      [] -> 0 *)
  | InnerBindings of binding * binding list * expr (** e.g.
                                                       let
                                                       x = 1
                                                       y = 2
                                                       in
                                                       x + y *)

(** e.g. (x + 1) :: Int*)
and expr = expression * tp option
