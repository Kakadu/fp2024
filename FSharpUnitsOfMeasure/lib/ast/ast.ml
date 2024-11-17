(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

type integer_exp =
  | Pos_int_exp of int (** Positive integer exponent: [2] *)
  | Neg_int_exp of int (** Negative integer exponent: [-1] *)
[@@deriving qcheck, show { with_path = false }]

type measure =
  | Measure_ident of string (** Measure identificator: [m] *)
  | Measure_prod of measure * measure (** Measure product: [sec * h], [kg m] *)
  | Measure_div of measure * measure (** Measure division: [m / sec] *)
  | Measure_pow of measure * integer_exp (** Measure to the integer power: [cm^3] *)
  | Measure_paren of measure (** Parentheses around measure: [(kg / m^3)]*)
[@@deriving qcheck, show { with_path = false }]

type measure_num =
  | Mnum_int of int (** Integer numbers with units of measure *)
  | Mnum_float of float (** Real numbers with units of measure *)
[@@deriving qcheck, show { with_path = false }]

(** Units of measure: [1<m>], [9.8<kg m / s>], [0.3<kg^3>] etc. *)
type unit_of_measure = Unit_of_measure of measure_num * measure
[@@deriving qcheck, show { with_path = false }]

type constant =
  | Const_int of int (** Integer constants: [1] *)
  | Const_float of float (** Float constants: [3.14], [1e+5], [5.9E-3f] *)
  | Const_bool of bool (** Boolean constants [true] and [false] *)
  | Const_char of char (** Char constants: ['a'] *)
  | Const_string of string (** String constants: ["foo"] *)
  | Const_unit_of_measure of unit_of_measure
  (** Units of measure constants: [5.0<cm>], [3<kg>] *)
[@@deriving qcheck, show { with_path = false }]

type core_type =
  | Type_ident of string (** Type identificator, such as [int] *)
  | Type_func of core_type * core_type (** Function type: [T1 -> T2] *)
  | Type_tuple of core_type * core_type * core_type list
  (** [Type_tuple(T1, T2, [T3, ..., Tn])] represents:
      - [(T1 * T2)] when core_type list is []
      - [(T1 * T2 * T3 * ... * Tn)] when core_type list is (::) A B *)
[@@deriving qcheck, show { with_path = false }]

type pattern =
  | Pattern_ident of string (** Identificator patterns: [x] *)
  | Pattern_const of constant
  | Pattern_wild (** Wildcard patterns [ _ ] *)
  | Pattern_typed of pattern * core_type (** Typed pattern [x : int] *)
  (** Constant patterns: [1], ['a'], ["foo"], [3.14], [5.0<cm>] *)
  | Pattern_tuple of pattern * pattern * pattern list
  (** [Pattern_tuple(P1, P2, [P3, ..., Pn])] represents:
      - [(P1, P2)] when pattern list is []
      - [(P1, P2, P3, ..., Pn)] when pattern list is (::) *)
  | Pattern_list of pattern list (** List patterns: [P1, ..., Pn] *)
  | Pattern_or of pattern * pattern
  (** OR patterns represent multiple satisfying patterns in pattern matching: [P1 | P2] *)
[@@deriving qcheck, show { with_path = false }]

type rec_flag =
  | Nonrecursive (** For nonrecursive function declarations *)
  | Recursive (** For recursive function declarations *)
[@@deriving qcheck, show { with_path = false }]

(** [(P, E)] represents [let P = E] or [let rec P = E] *)
type val_binding = Bind of pattern * expression [@@deriving qcheck, show { with_path = false }]

(** [Rule(P, E)] represents [P -> E] in pattern matching *)
and rule = Rule of pattern * expression [@@deriving qcheck, show { with_path = false }]

and expression =
  | Expr_const of constant
  (** Constant expressions: [1], ['a'], ["foo"], [3.14], [true], [5.0<cm>] *)
  | Expr_ident_or_op of string (** Identificator or operation expressions: [x], [+] *)
  | Expr_tuple of expression * expression * expression list
  (** [Expr_tuple(E1, E2, [E3, ..., En])] represents:
      - [(E1, E2)] when pattern list is []
      - [(E1, E2, E3, ..., En)] when pattern list is (::) *)
  | Expr_list of expression list (** List expressions: [E1; ...; En] *)
  | Expr_fun of pattern * expression
  (** Anonimous functions: [Expr_fun(P, E)] represents [fun P -> E] *)
  | Expr_let of rec_flag * val_binding * val_binding list * expression
  (** [Expr_let(rec_flag, Bind(P1, E1), [Bind(P2, E2); ...; Bind(Pn, En)], E)] represents:
      - [let P1 = E1 in E] when val_binding list is [] and rec_flag is Nonrecursive
      - [let rec P1 = E1 in E] when val_binding list is [] and rec_flag is Recursive
      - [let P1 = E1 and ... and Pn = En in E] when val_binding list is (::) A B and rec_flag is Nonrecursive
      - [let rec P1 = E1 and ... and Pn = En in E] when val_binding list is (::) A B and rec_flag is Recursive *)
  | Expr_ifthenelse of expression * expression * expression option
  (** [if E1 then E2 else E3] *)
  | Expr_apply of expression * expression (** Application [E1 E2] *)
  | Expr_match of expression * rule * rule list
  (** [Expr_match(E, Rule(P1, E1), [Rule(P2, E2); ...; Rule(Pn, En)])] represents:
      - [match E with P1 -> E1] if rule list is []
      - [match E with P1 -> E1 | P2 -> E2 | ... | Pn -> En] if rule list is (::) A B *)
[@@deriving qcheck, show { with_path = false }]

type type_def =
  | Measure_type_def of string * measure option
  (** Measure type definition:
      - [[<Measure>] type I] when measure is None
      - [[<Measure>] type I = M] when measure is Some M *)
[@@deriving qcheck, show { with_path = false }]

type structure_item =
  | Str_item_eval of expression
  (** Structure item which is single expression: [E] or [do E] *)
  | Str_item_def of rec_flag * val_binding * val_binding list
  (** [Str_item_def(rec_flag, (P1, E1), [(P2, E2); ...; (Pn, En)])] represents:
      - [let P1 = E1] when val_binding list is [] and rec_flag is Nonrecursive
      - [let rec P1 = E1] when val_binding list is [] and rec_flag is Recursive
      - [let P1 = E1 and ... and Pn = En] when val_binding list is (::) A B and rec_flag is Nonrecursive
      - [let rec P1 = E1 and ... and Pn = En] when val_binding list is (::) A B and rec_flag is Recursive *)
  | Str_item_type_def of type_def (** Structure item which is type definition *)
[@@deriving qcheck, show { with_path = false }]

type program = structure_item list [@@deriving qcheck, show { with_path = false }]
