(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

type binary_operation =
  | Add (** 1 + 2 *)
  | Sub (** 1 - 2 *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | And (** && *)
  | Or (** || *)
  | Eq (** = *)
  | Neq (** <> *)
  | Less (** < *)
  | Gre (** > *)
  | Leq (** <= *)
  | Greq (** >= *)
[@@deriving eq, show { with_path = false }]

type constant =
  | Const_bool of bool (** Boolean constants [true] and [false] *)
  | Const_int of int (** Integer constant such as [1] *)
  | Const_char of char (** Char constant such as ['a'] *)
  | Const_string of string (** String constant such as ["foo"] *)
  | Const_float of float (** Float constants such as [3.14], [1e+5], [5.9E-3] *)
  | Measure_float of constant * measure_type (** 5.0<cm> *)
  
and measure_type =
  | Single_measure of string * pow (** single measure: <m>*)
  | Multiple_measure of measure_type * binary_operation * measure_type
  (** multiple measure: <m / sec * h ... >*)

and pow = Pow of constant (** ^ *) 
[@@deriving eq, show { with_path = false }]

type pattern =
  | Pattern_wild (** Wildcard patterns [_] *)
  | Pattern_ident of string (** Identificator name patterns such as [x] *)
  | Pattern_const of constant
  (** Constant patterns such as [1], ['a'], ["foo"], [3.14] *)
  | Pattern_tuple of pattern list
  (** Tuple patterns [(P1; ..., Pn)]

      Invariant: [n >= 2]
    *)
  | Pattern_or of pattern * pattern (** Pattern [P1 | P2] *)
[@@deriving eq, show { with_path = false }]

type rec_flag =
  | Nonrecursive
  | Recursive
[@@deriving show { with_path = false }]

type val_binding = Binding of pattern * expression
(** [Binding(P, E)] represents [let P = E] *)
[@@deriving show { with_path = false }]

and expression =
  | Expr_const of constant
  (** Constant expressions such as [1], ['a'], ["foo"], [3.14], [true] *)
  | Expr_ident of string
  (** Identificator name expressions such as [x] *)
  | Expr_binary of expression * binary_operation * expression 
  (** Expression with binary operation such as 302 * 228 *)
  | Expr_tuple of expression list
  (** Tuple expressions [(E1, ..., En)]
      Invariant: [n >= 2]
  *)
  | Expr_fun of pattern * expression (** [Exp_fun (P, E)] represents [fun P -> E] *)
  | Expr_let of rec_flag * val_binding list * expression
  (** [Expr_let(rec_flag, [(P1, E1); ...; (Pn, En)], E)] represents:
        - [let P1 = E1 and ... and Pn = En in E] when rec_flag is Nonrecursive
        - [let rec P1 = E1 and ... and Pn = En in E] when rec_flag is Recursive
  *)
  | Expr_ifthenelse of expression * expression * expression option
  (** [if E1 then E2 else E3] *)
  | Expr_apply of expression * expression
  (** Application [E1 E2]  *)
  | Expr_match of expression * (pattern * expression) list
  (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
  | Expr_measure of measure_type (** measure *)
[@@deriving show { with_path = false }]
