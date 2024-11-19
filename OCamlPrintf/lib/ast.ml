(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck.Gen

(* For the generator's speed. *)
let coef = 50

type 'a list_ = ('a list[@gen small_list gen_a])
[@@deriving show { with_path = false }, qcheck]

let gen_ident =
  small_string ~gen:(map Char.chr (int_range (Char.code 'a') (Char.code 'h')))
;;

type ident = (string[@gen gen_ident]) [@@deriving show { with_path = false }, qcheck]

type rec_flag =
  | Recursive
  | Nonrecursive
[@@deriving show { with_path = false }, qcheck]

type constant =
  | Const_integer of (int[@gen big_nat])
  | Const_char of (char[@gen printable])
  | Const_string of (string[@gen string_readable])
[@@deriving show { with_path = false }, qcheck]

type core_type =
  | Type_any
  | Type_char
  | Type_int
  | Type_string
  | Type_bool
  | Type_list of core_type
  | Type_tuple of
      (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type list[@gen small_list (gen_core_type_sized (n / coef))])
[@@deriving show { with_path = false }, qcheck]

type pattern =
  | Pat_any
  | Pat_var of ident
  | Pat_constant of constant
  | Pat_tuple of
      (pattern[@gen gen_pattern_sized (n / coef)])
      * (pattern[@gen gen_pattern_sized (n / coef)])
      * (pattern list[@gen small_list (gen_pattern_sized (n / coef))])
  | Pat_construct of ident * (pattern[@gen gen_pattern_sized (n / coef)]) option
  | Pat_constraint of (pattern[@gen gen_pattern_sized (n / coef)]) * core_type
[@@deriving show { with_path = false }, qcheck]

type 'exp value_binding =
  { pat : pattern
  ; exp : 'exp
  }
[@@deriving show { with_path = false }, qcheck]

type 'exp case =
  { left : pattern
  ; right : 'exp
  }
[@@deriving show { with_path = false }, qcheck]

module Expression = struct
  type value_binding_exp = expression value_binding
  and case_exp = expression case

  and expression =
    | Exp_ident of ident
    | Exp_constant of constant
    | Exp_let of
        rec_flag
        * (value_binding_exp
          [@gen
            map2
              (fun pat exp -> { pat; exp })
              gen_pattern
              (gen_expression_sized (n / coef))])
        * (value_binding_exp
          [@gen
            map2
              (fun pat exp -> { pat; exp })
              gen_pattern
              (gen_expression_sized (n / coef))])
            list_
        * (expression[@gen gen_expression_sized (n / coef)])
    | Exp_fun of
        (pattern list[@gen small_list gen_pattern])
        * (expression[@gen gen_expression_sized (n / coef)])
    | Exp_apply of
        (expression[@gen gen_expression_sized (n / coef)])
        * (expression[@gen gen_expression_sized (n / coef)])
        * (expression list[@gen small_list (gen_expression_sized (n / coef))])
    | Exp_match of
        (expression[@gen gen_expression_sized (n / coef)])
        * (case_exp
          [@gen
            map2
              (fun left right -> { left; right })
              gen_pattern
              (gen_expression_sized (n / coef))])
        * (case_exp
          [@gen
            map2
              (fun left right -> { left; right })
              gen_pattern
              (gen_expression_sized (n / coef))])
            list_
    | Exp_tuple of
        (expression[@gen gen_expression_sized (n / coef)])
        * (expression[@gen gen_expression_sized (n / coef)])
        * (expression list[@gen small_list (gen_expression_sized (n / coef))])
    | Exp_construct of ident * (expression[@gen gen_expression_sized (n / coef)]) option
    | Exp_ifthenelse of
        (expression[@gen gen_expression_sized (n / coef)])
        * (expression[@gen gen_expression_sized (n / coef)])
        * (expression[@gen gen_expression_sized (n / coef)]) option
    | Exp_sequence of
        (expression[@gen gen_expression_sized (n / coef)])
        * (expression[@gen gen_expression_sized (n / coef)])
    | Exp_constraint of (expression[@gen gen_expression_sized (n / coef)]) * core_type
  [@@deriving show { with_path = false }, qcheck]
end

type structure_item =
  | Struct_eval of Expression.expression
  | Struct_value of
      rec_flag
      * Expression.expression value_binding
      * (Expression.expression value_binding list
        [@gen small_list (gen_value_binding Expression.gen_expression)])
[@@deriving show { with_path = false }, qcheck]

type structure = (structure_item list[@gen small_list gen_structure_item])
[@@deriving show { with_path = false }, qcheck]

(*
   (** Identifier *)
type ident = string [@@deriving show { with_path = false }]

type rec_flag =
  | Recursive (** Recursive value binding *)
  | Nonrecursive (** Nonrecursive value binding *)
[@@deriving show { with_path = false }]

type constant =
  | Const_integer of int (** Integer constant such as [1] *)
  | Const_char of char (** Character such as ['a'] *)
  | Const_string of string (** Constant string such as ["constant"] *)
[@@deriving show { with_path = false }]

type core_type =
  | Type_any (** [_] *)
  | Type_char (** [char] *)
  | Type_int (** [int] *)
  | Type_string (** [string] *)
  | Type_bool (** [bool] *)
  | Type_list of core_type (** [T list] *)
  | Type_tuple of core_type * core_type * core_type list (** [T1 * ... * Tn] *)
[@@deriving show { with_path = false }]

type pattern =
  | Pat_any (** The pattern [_] *)
  | Pat_var of ident (** A variable pattern such as [x] *)
  | Pat_constant of constant (** Patterns such as [1], ['a'], ["true"] *)
  | Pat_tuple of pattern * pattern * pattern list (** Patterns [(P1, ... , Pn)] *)
  | Pat_construct of ident * pattern option
  (** [Pat_construct(C, args)] represents:
      - [C]   when [args] is [None],
      - [C P] when [args] is [Some P] *)
  | Pat_constraint of pattern * core_type (** Pattern [(P : T)] *)
[@@deriving show { with_path = false }]

(** [let pat = exp] *)
type value_binding =
  { pat : pattern
  ; exp : expression
  }
[@@deriving show { with_path = false }]

(** Values of type represents [(P -> E)] *)
and case =
  { left : pattern
  ; right : expression
  }
[@@deriving show { with_path = false }]

and expression =
  | Exp_ident of ident (** Identifier such as [x] *)
  | Exp_constant of constant (** Expressions constant such as [1], ['a'], ["true"] *)
  | Exp_let of rec_flag * value_binding * value_binding list * expression
  (** [Exp_let(flag, [(P1, E1); ... ; (Pn, En)], E)] represents:
      - [let     P1 = E1 and ... and Pn = En in E] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = En in E] when [flag] is [Recursive]. *)
  | Exp_fun of pattern list * expression
  (** [Exp_fun([P1; ... ; Pn], E)] represents [fun P1 ... Pn -> E] *)
  | Exp_apply of expression * expression * expression list
  (** [Exp_apply(E0, [E1; ... ; En])] represents [E0 E1 ... En] *)
  | Exp_match of expression * case * case list
  (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
  | Exp_tuple of expression * expression * expression list
  (** Expressions [(E1, ... , En)] *)
  | Exp_construct of ident * expression option
  (** [Exp_construct(C, exp)] represents:
      - [C]                when [exp] is [None],
      - [C E]              when [exp] is [Some E],
      - [C (E1, ... , En)] when [exp] is [Some (Exp_tuple[E1; ... ; En])] *)
  | Exp_ifthenelse of expression * expression * expression option
  (** [if E1 then E2 else E3] *)
  | Exp_sequence of expression * expression (** [E1; E2] *)
  | Exp_constraint of expression * core_type (** [(E : T)] *)
[@@deriving show { with_path = false }]

type structure_item =
  | Struct_eval of expression (** [E] *)
  | Struct_value of rec_flag * value_binding * value_binding list
  (** [Struct_value(flag, [(P1, E1); ... ; (Pn, En))])] represents:
      - [let     P1 = E1 and ... and Pn = En] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = En] when [flag] is [Recursive]. *)
[@@deriving show { with_path = false }]

type structure = structure_item list [@@deriving show { with_path = false }]
*)
