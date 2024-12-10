(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

type const =
  | Int of (int[@gen QCheck.Gen.(0 -- Int.max_int)]) (** e.g. [18] *)
  | Bool of bool (** e.g. [True] *)
  | Unit (** () *)
[@@deriving qcheck, show { with_path = false }]

type 'a maybe =
  | Nothing (** Nothing *)
  | Just of 'a (** e.g. [Just 5] *)
[@@deriving qcheck, show { with_path = false }]

(** explicit type indication*)
type tp =
  | TUnit (** () *)
  | TInt (** Int *)
  | TBool (** Bool *)
  | MaybeParam of tp (** e.g. [Maybe Int]*)
  | TreeParam of tp (** e.g. [{Int}] *)
  | ListParam of tp (** e.g. [[Int]] *)
  | TupleParams of tp * tp * tp_list (** e.g. [(Int, Bool)] *)
  | FunctionType of functype
[@@deriving qcheck, show { with_path = false }]

and functype = FuncT of tp * tp * tp_list (** e.g. [Int-> Bool -> (Int,Bool)] *)

and tp_list =
  (tp list
  [@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 10))) (gen_tp_sized (n / 10)))])
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

let gen_first_symbol =
  QCheck.Gen.(
    map
      Char.chr
      (oneof [ int_range (Char.code 'a') (Char.code 'z'); return (Char.code '_') ]))
;;

let gen_char =
  QCheck.Gen.(
    map
      Char.chr
      (oneof
         [ int_range (Char.code 'a') (Char.code 'z')
         ; int_range (Char.code 'A') (Char.code 'Z')
         ; int_range (Char.code '0') (Char.code '9')
         ; return (Char.code '_')
         ; return (Char.code '\'')
         ]))
;;

let is_keyword_or_underscore = function
  | "case" | "of" | "if" | "then" | "else" | "let" | "in" | "where" | "_" -> true
  | _ -> false
;;

let varname =
  QCheck.Gen.(
    map2
      (fun x y -> Printf.sprintf "%c%s" x y)
      gen_first_symbol
      (string_size ~gen:gen_char (1 -- 7)))
;;

let correct_varname x = QCheck.Gen.map (fun y -> Printf.sprintf "%s%c" x y) gen_char

let gen_string =
  let open QCheck.Gen in
  let x = varname in
  map is_keyword_or_underscore x
  >>= fun y -> if y then map correct_varname x >>= fun y -> y else x
;;

(** variable's / function's name*)
type ident = Ident of (string[@gen gen_string])
[@@deriving qcheck, show { with_path = false }]
(** e.g. [(a@my_list@lst@(_:xs) :: [Int]) :: [Bool]] *)

type pconst =
  | OrdinaryPConst of const (** e.g [True]*)
  | NegativePInt of (int[@gen QCheck.Gen.(0 -- Int.max_int)]) (** e.g [-12]*)
[@@deriving qcheck, show { with_path = false }]

type pattern =
  (ident list[@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 7))) gen_ident)])
  * pat
  * tp_list
[@@deriving qcheck, show { with_path = false }]

and pattern_list =
  (pattern list
  [@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 7))) (gen_pattern_sized (n / 7)))])

and listpat =
  | PCons of pattern * pattern (** e.g. [x:xs] *)
  | PEnum of pattern_list (** e.g. [[x, y, z]] *)

and treepat =
  | PNul (** nul tree i.e. [$] *)
  | PNode of pattern * pattern * pattern (** tree's node e.g [(x; y; z)]*)

and pat =
  | PWildcard (** _ *)
  | PConst of pconst
  | PIdentificator of ident (** e.g. [x] *)
  | PList of listpat
  | PTuple of pattern * pattern * pattern_list (** e.g. [(x, y, z)]*)
  | PMaybe of pattern maybe (** e.g. [Just x] *)
  | PTree of treepat

type comprehension =
  | Condition of expr (** e.g. [x < 2] *)
  | Generator of (pattern * expr) (** e.g. [x <- [1 ..]] *)
[@@deriving qcheck, show { with_path = false }]

and comprehension_list =
  (comprehension list
  [@gen
    QCheck.Gen.(list_size (return (Int.min 2 (n / 7))) (gen_comprehension_sized (n / 7)))])

and ordinarylistbld =
  | ComprehensionList of expr * comprehension * comprehension_list
  (** e.g [[x * y | x <- [1, 2, 20], y <- [2, 3], y `mod` 2 == 0]] *)
  | IncomprehensionlList of expr_list (**e.g. [[1,2]] *)

and listbld =
  | LazyList of expr * expr option * expr option
  (** e.g. [[1.. ] or [1..2] or [1, 2 .. 2] or [1, 3..]] *)
  | OrdList of ordinarylistbld

and binding =
  | VarsDef of pattern * bindingbody * binding_list
  (** e.g [x = let y = 12 in y * z where z = 5] *)
  | FunDef of ident * pattern * pattern_list * bindingbody * binding_list
  (** e.g [f x y = x + y + z where z = 2 ]*)
  | Decl of ident * tp (** e.g [f :: Int -> Int]*)

(* had to do such a manual generator because of where shadowing*)
and binding_list =
  (binding list
  [@gen
    QCheck.Gen.(
      list_size
        (return (Int.min 2 (n / 7)))
        (map
           (function
             | VarsDef (p, b, _) -> VarsDef (p, b, [])
             | FunDef (i, p, pp, b, _) -> FunDef (i, p, pp, b, [])
             | x -> x)
           (gen_binding_sized (n / 7))))])

and pattern_bindinbody_list =
  ((pattern * bindingbody) list
  [@gen
    QCheck.Gen.(
      list_size
        (return (Int.min 2 (n / 7)))
        (pair (gen_pattern_sized (n / 7)) (gen_bindingbody_sized (n / 7))))])

(** examples below are for function binding with due body *)
and bindingbody =
  | Guards of (expr * expr) * expr_expr_list
  (** (condition, branch) pairs e.g [f x | x > 0 = x | otherwise = -1] *)
  | OrdBody of expr (** e.g [f x = if x > 0 then x else -1] *)

and binary_tree_bld =
  | Nul (** node that not exists (notation: [$]) *)
  | Node of expr * expr * expr
  (** node is data and two 'sons' e.g  [(x^y; $; (2; $; $))] *)

and expression =
  | Const of const
  | Identificator of ident (** e.g  [x] *)
  | TupleBld of expr * expr * expr_list (** e.g  [(1+3, f x)] *)
  | EJust (** Maybe constructor Just*)
  | ENothing (*Maybe constructor Nothing*)
  | ListBld of listbld (** e.g [[(2 ^ 2 - 3) ..]] *)
  | Binop of expr * binop * expr (** e.g [1 > 0] *)
  | Neg of expr (** e.g [(-1)] *)
  | IfThenEsle of expr * expr * expr (** e.g [if x >= 0 then x else (-x)] *)
  | FunctionApply of expr * expr * expr_list (** e.g. [sum 1 2 or \x -> x + 1) 1] *)
  | Lambda of pattern * pattern_list * expr (** e.g. [\x y -> x + y] *)
  | BinTreeBld of binary_tree_bld
  | Case of expr * (pattern * bindingbody) * pattern_bindinbody_list
  (** e.g [case l of (x:xs) -> x; [] -> 0] *)
  (* had to do such a manual generator because of where shadowing*)
  | InnerBindings of binding * (binding_list[@gen QCheck.Gen.return []]) * expr
  (** e.g. [let x = 1; y = 2 in x + y] *)

(** e.g. [((x + 1) :: Int  ) :: Bool]*)
and expr = expression * tp_list [@@deriving qcheck, show { with_path = false }]

and expr_list =
  (expr list
  [@gen QCheck.Gen.(list_size (return (Int.min 2 (n / 7))) (gen_expr_sized (n / 7)))])

and expr_expr_list =
  ((expr * expr) list
  [@gen
    QCheck.Gen.(
      list_size
        (return (Int.min 2 (n / 7)))
        (pair (gen_expr_sized (n / 7)) (gen_expr_sized (n / 7))))])
