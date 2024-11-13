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
  | Const_bool of bool (** Boolean constants [true] and [false] *)
  | Const_int of int (** Integer constants: [1] *)
  | Const_char of char (** Char constants: ['a'] *)
  | Const_string of string (** String constants: ["foo"] *)
  | Const_float of float (** Float constants: [3.14], [1e+5], [5.9E-3f] *)
  | Const_unit_of_measure of unit_of_measure
  (** Units of measure constants: [5.0<cm>], [3<kg>] *)
[@@deriving qcheck, show { with_path = false }]

type pattern =
  | Pattern_wild (** Wildcard patterns [ _ ] *)
  | Pattern_ident of string (** Identificator patterns: [x] *)
  | Pattern_const of constant
  (** Constant patterns: [1], ['a'], ["foo"], [3.14], [5.0<cm>] *)
  | Pattern_tuple of pattern list
  (** Tuple patterns: [(P1; ..., Pn)]
      Invariant: [n >= 2] *)
  | Pattern_or of pattern * pattern (** WIP Or patterns: [P1 | P2] *)
  | Pattern_list of pattern list (** List patterns: [P1, ..., Pn] *)
[@@deriving qcheck, show { with_path = false }]

type rec_flag =
  | Nonrecursive (** For nonrecursive function declarations *)
  | Recursive (** For recursive function declarations *)
[@@deriving qcheck, show { with_path = false }]

type val_binding =
  | Binding of pattern * expression (** [Binding(P, E)] represents [let P = E] *)
[@@deriving qcheck, show { with_path = false }]

and expression =
  | Expr_const of constant
  (** Constant expressions: [1], ['a'], ["foo"], [3.14], [true], [5.0<cm>] *)
  | Expr_ident_or_op of string (** Identificator or operation expressions: [x], [+] *)
  | Expr_tuple of expression list
  (** Tuple expressions: [(E1, ..., En)]
      Invariant: [n >= 2] *)
  | Expr_fun of pattern * expression
  (** Anonimous functions: [Expr_fun(P, E)] represents [fun P -> E] *)
  | Expr_let of rec_flag * val_binding list * expression
  (** [Expr_let(rec_flag, [(P1, E1); ...; (Pn, En)], E)] represents:
      - [let P1 = E1 and ... and Pn = En in E] when rec_flag is Nonrecursive
      - [let rec P1 = E1 and ... and Pn = En in E] when rec_flag is Recursive
        Invariant: [n >= 1] *)
  | Expr_ifthenelse of expression * expression * expression option
  (** [if E1 then E2 else E3] *)
  | Expr_apply of expression * expression (** Application [E1 E2] *)
  | Expr_match of expression * (pattern * expression) list
  (** [match E with P1 -> E1 | ... | Pn -> En]
      Invariant: [n >= 1] *)
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
  | Str_item_def of rec_flag * val_binding list
  (** [Str_item_def(rec_flag, [(P1, E1); ...; (Pn, En)])] represents:
      - [let P1 = E1 and ... and Pn = En] when rec_flag is Nonrecursive
      - [let rec P1 = E1 and ... and Pn = En] when rec_flag is Recursive
        Invariant: [n >= 1] *)
  | Str_item_type_def of type_def (** Structure item which is type definition *)
[@@deriving qcheck, show { with_path = false }]

type program = structure_item list [@@deriving qcheck, show { with_path = false }]
