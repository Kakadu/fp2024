(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a list_ = 'a list

val pp_list_ : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list_ -> unit
val show_list_ : (Format.formatter -> 'a -> unit) -> 'a list_ -> string
val is_keyword : string -> bool

(** Identifier *)
type ident = string

val pp_ident : Format.formatter -> ident -> unit
val show_ident : ident -> string

type rec_flag =
  | Recursive (** Recursive value binding *)
  | Nonrecursive (** Nonrecursive value binding *)

val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type constant =
  | Const_integer of int (** Integer constant such as [1] *)
  | Const_char of char (** Character such as ['a'] *)
  | Const_string of string (** Constant string such as ["constant"] *)

val pp_constant : Format.formatter -> constant -> unit
val show_constant : constant -> string

type core_type =
  | Type_any (** [_] *)
  | Type_unit (** [unit] *)
  | Type_char (** [char] *)
  | Type_int (** [int] *)
  | Type_string (** [string] *)
  | Type_bool (** [bool] *)
  | Type_option of core_type (** [T option] *)
  | Type_name of ident (** ['a] *)
  | Type_list of core_type (** [T list] *)
  | Type_tuple of core_type * core_type * core_type list_ (** [T1 * ... * Tn] *)
  | Type_arrow of core_type * core_type (** [T1 -> T2]*)

val pp_core_type : Format.formatter -> core_type -> unit
val show_core_type : core_type -> string

type pattern =
  | Pat_any (** The pattern [_] *)
  | Pat_var of ident (** A variable pattern such as [x] *)
  | Pat_constant of constant (** Patterns such as [1], ['a'], ["true"] *)
  | Pat_tuple of pattern * pattern * pattern list_ (** Patterns [(P1, ... , Pn)] *)
  | Pat_construct of (ident * pattern option)
  (** [Pat_construct(C, args)] represents:
      - [C]   when [args] is [None],
      - [C P] when [args] is [Some P] *)
  | Pat_constraint of pattern * core_type (** Pattern [(P : T)] *)

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

(** [let pat = exp] *)
type 'exp value_binding =
  { pat : pattern
  ; exp : 'exp
  }

val pp_value_binding
  :  (Format.formatter -> 'exp -> unit)
  -> Format.formatter
  -> 'exp value_binding
  -> unit

val show_value_binding
  :  (Format.formatter -> 'exp -> unit)
  -> 'exp value_binding
  -> string

(** Values of type represents [(P -> E)] *)
type 'exp case =
  { left : pattern
  ; right : 'exp
  }

val pp_case : (Format.formatter -> 'exp -> unit) -> Format.formatter -> 'exp case -> unit
val show_case : (Format.formatter -> 'exp -> unit) -> 'exp case -> string

module Expression : sig
  type value_binding_exp = t value_binding
  and case_exp = t case

  and t =
    | Exp_ident of ident (** Identifier such as [x] *)
    | Exp_constant of constant (** ts constant such as [1], ['a'], ["true"] *)
    | Exp_let of rec_flag * value_binding_exp * value_binding_exp list_ * t
    (** [Exp_let(flag, [(P1, E1); ... ; (Pn, En)], E)] represents:
        - [let     P1 = E1 and ... and Pn = En in E] when [flag] is [Nonrecursive],
        - [let rec P1 = E1 and ... and Pn = En in E] when [flag] is [Recursive]. *)
    | Exp_fun of pattern * pattern list_ * t
    (** [Exp_fun([P1; ... ; Pn], E)] represents [fun P1 ... Pn -> E] *)
    | Exp_apply of (t * t) (** [Exp_apply(E0, E1)] represents [E0 E1] *)
    | Exp_function of case_exp * case_exp list_
    (** [function P1 -> E1 | ... | Pn -> En] *)
    | Exp_match of t * case_exp * case_exp list_
    (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
    | Exp_tuple of t * t * t list_ (** ts [(E1, ... , En)] *)
    | Exp_construct of (ident * t option)
    (** [Exp_construct(C, exp)] represents:
        - [C]                when [exp] is [None],
        - [C E]              when [exp] is [Some E],
        - [C (E1, ... , En)] when [exp] is [Some (Exp_tuple[E1; ... ; En])] *)
    | Exp_ifthenelse of t * t * t option (** [if E1 then E2 else E3] *)
    | Exp_sequence of t * t (** [E1; E2] *)
    | Exp_constraint of t * core_type (** [(E : T)] *)

  val pp : Format.formatter -> t -> unit
  val show : t -> ident
end

type structure_item =
  | Struct_eval of Expression.t (** [E] *)
  | Struct_value of
      rec_flag * Expression.value_binding_exp * Expression.value_binding_exp list_
  (** [Struct_value(flag, [(P1, E1); ... ; (Pn, En))])] represents:
      - [let     P1 = E1 and ... and Pn = En] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and ... and Pn = En] when [flag] is [Recursive]. *)

val pp_structure_item : Format.formatter -> structure_item -> unit
val show_structure_item : structure_item -> string

type structure = structure_item list_

val pp_structure : Format.formatter -> structure -> unit
val show_structure : structure -> string
val gen_structure : structure QCheck.Gen.t
val arb_structure : structure QCheck.arbitrary
