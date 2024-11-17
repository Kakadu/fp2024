(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

type const =
  | Integer of Integer.nonnegative_integer [@gen gen_nonnegative_integer] (** e.g. [18] *)
  | Bool of bool (** e.g. [True] *)
  | Unit (** () *)
[@@deriving qcheck, show { with_path = false }]

type 'a maybe =
  | Nothing (** Nothing *)
  | Just of 'a (** e.g. [Just 5] *)
[@@deriving qcheck, show { with_path = false }]

type functype =
  | FuncT of tp * tp * tp list (** e.g. [Integer-> Bool -> (Integer,Bool)] *)
[@@deriving qcheck, show { with_path = false }]

(** explicit type indication*)
and tp =
  | TUnit (** () *)
  | TInteger (** Integer *)
  | TBool (** Bool *)
  | TreeParam of tp (** e.g. [{Integer}] *)
  | ListParam of tp (** e.g. [[Integer]] *)
  | TupleParams of tp * tp * tp list (** e.g. [(Integer, Bool)] *)
  | FunctionType of functype
[@@deriving qcheck, show { with_path = false }]

type binop =
  | And (** [&&]*)
  | Or (** [||] *)
  | Plus (** [+] *)
  | Minus (** [-] *)
  | Divide (** [`div`] *)
  | Mod (** [`mod`]*)
  | Cons (** [:] *)
  | Multiply (** [*] *)
  | Equality (** [==] *)
  | Pow (** [^] *)
  | Inequality (** [/=] *)
  | Less (** [<] *)
  | Greater (** [>] *)
  | EqualityOrLess (** [<=] *)
  | EqualityOrGreater (** [>=] *)
[@@deriving qcheck, show { with_path = false }]

(** variable's / function's name*)
type ident = Ident of string [@@deriving qcheck, show { with_path = false }]

(** e.g. [(a@my_list@lst@(_:xs) :: [Integer]) :: [Bool]] *)
type pattern = ident list * pat * tp list [@@deriving qcheck, show { with_path = false }]

and listpat =
  | PCons of pattern * pattern (** e.g. [x:xs] *)
  | PEnum of pattern list (** e.g. [[x, y, z]] *)
[@@deriving qcheck, show { with_path = false }]

and treepat =
  | PNul (** nul tree i.e. [$] *)
  | PNode of pattern * pattern * pattern (** tree's node e.g [(x; y; z)]*)
[@@deriving qcheck, show { with_path = false }]

and pconst =
  | OrdinaryPConst of const (** e.g [True]*)
  | NegativePInteger of Integer.nonnegative_integer (** e.g [-12]*)
[@@deriving qcheck, show { with_path = false }]

and pat =
  | PWildcard (** _ *)
  | PConst of pconst
  | PIdentificator of ident (** e.g. [x] *)
  | PList of listpat
  | PTuple of pattern * pattern * pattern list (** e.g. [(x, y, z)]*)
  | PMaybe of pattern maybe (** e.g. [Just x] *)
  | PTree of treepat
[@@deriving qcheck, show { with_path = false }]

type comprehension =
  | Condition of expr (** e.g. [x < 10] *)
  | Generator of (pattern * expr) (** e.g. [x <- [1 ..]] *)
[@@deriving qcheck, show { with_path = false }]

and ordinarylistbld =
  | ComprehensionList of expr * comprehension * comprehension list
  (** e.g [[x * y | x <- [1, 10, 100], y <- [2, 3], y `mod` 2 == 0]] *)
  | IncomprehensionlList of expr list (**e.g. [[1,2]] *)
[@@deriving qcheck, show { with_path = false }]

and listbld =
  | LazyList of expr * expr option * expr option
  (** e.g. [[1.. ] or [1..10] or [1, 2 .. 10] or [1, 3..]] *)
  | OrdList of ordinarylistbld
[@@deriving qcheck, show { with_path = false }]

and binding =
  | VarsDef of pattern * bindingbody * binding list
  (** e.g [x = let y = 12 in y * z where z = 5] *)
  | FunDef of ident * pattern * pattern list * bindingbody * binding list
  (** e.g [f x y = x + y + z where z = 2 ]*)
  | Decl of pattern * tp (** e.g [f :: Integer -> Integer]*)
[@@deriving qcheck, show { with_path = false }]

(** examples below are for function binding with due body *)
and bindingbody =
  | Guards of (expr * expr) * (expr * expr) list
  (** (condition, branch) pairs e.g [f x | x > 0 = x | otherwise = -1] *)
  | OrdBody of expr (** e.g [f x = if x > 0 then x else -1] *)
[@@deriving qcheck, show { with_path = false }]

and binary_tree_bld =
  | Nul (** node that not exists (notation: [$]) *)
  | Node of expr * expr * expr
  (** node is data and two 'sons' e.g  [(x^y; $; (2; $; $))] *)
[@@deriving qcheck, show { with_path = false }]

and expression =
  | Const of const
  | Identificator of ident (** e.g  [x] *)
  | TupleBld of expr * expr * expr list (** e.g  [(1+3, f x)] *)
  | OptionBld of expr maybe (** e.g  [Just (f x)] *)
  | ListBld of listbld (** e.g [[(2 ^ 2 - 3) ..]] *)
  | Binop of expr * binop * expr (** e.g [1 > 0] *)
  | Neg of expr (** e.g [(-1)] *)
  | IfThenEsle of expr * expr * expr (** e.g [if x >= 0 then x else (-x)] *)
  | FunctionApply of expr * expr * expr list (** e.g. [sum 1 2 or \x -> x + 1) 1] *)
  | Lambda of pattern * pattern list * expr (** e.g. [\x y -> x + y] *)
  | BinTreeBld of binary_tree_bld
  | Case of expr * (pattern * bindingbody) * (pattern * bindingbody) list
  (** e.g [case l of (x:xs) -> x; [] -> 0] *)
  | InnerBindings of binding * binding list * expr (** e.g. [let x = 1; y = 2 in x + y] *)
[@@deriving qcheck, show { with_path = false }]

(** e.g. [((x + 1) :: Integer  ) :: Bool]*)
and expr = expression * tp list [@@deriving qcheck, show { with_path = false }]
