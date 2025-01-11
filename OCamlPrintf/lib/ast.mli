(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a list_ = 'a list

val pp_list_ : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list_ -> unit
val show_list_ : (Format.formatter -> 'a -> unit) -> 'a list_ -> string

(** Identifier *)
type ident = string

val pp_ident : Format.formatter -> ident -> unit
val show_ident : ident -> string
val un_op : ident
val bin_op_list : (ident * int) list
val is_keyword : ident -> bool

type rec_flag =
  | Recursive (** Recursive value binding. *)
  | Nonrecursive (** Nonrecursive value binding. *)

val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type constant =
  | Const_integer of int (** A constant integer such as [1]. *)
  | Const_char of char (** A constant character such as ['a']. *)
  | Const_string of string (** A constant string such as ["const"]. *)

val pp_constant : Format.formatter -> constant -> unit
val show_constant : constant -> string

type core_type =
  | Type_any (** The type [_]. *)
  | Type_unit (** The type [unit]. *)
  | Type_char (** The type [char]. *)
  | Type_int (** The type [int]. *)
  | Type_string (** The type [string]. *)
  | Type_bool (** The type [bool]. *)
  | Type_option of core_type (** [Type_option(T)] represents [T option]. *)
  | Type_name of ident
  | Type_list of core_type (** [Type_list(T)] represents [T list]. *)
  | Type_tuple of core_type * core_type * core_type list_
  (** [Type_tuple(T1, T2, [T3; ... ; Tn])] represents [T1 * ... * Tn]. *)
  | Type_arrow of core_type * core_type (** [Type_arrow(T1, T2)] represents [T1 -> T2]. *)

val pp_core_type : Format.formatter -> core_type -> unit
val show_core_type : core_type -> string

type pattern =
  | Pat_any (** The pattern [_]. *)
  | Pat_var of ident (** A variable pattern such as [x]. *)
  | Pat_constant of constant (** A pattern such as [1], ['a'], ["const"]. *)
  | Pat_tuple of pattern * pattern * pattern list_
  (** [Pat_tuple(P1, P2, [P3; ... ; Pn])] represents [(P1, ... , Pn)]. *)
  | Pat_construct of (ident * pattern option)
  (** [Pat_construct(I, pat)] represents
      - [()]                   when [I] is ["()"]    and [pat] is [None],
      - [false]                when [I] is ["false"] and [pat] is [None],
      - [true]                 when [I] is ["true"]  and [pat] is [None],
      - [None]                 when [I] is ["None"]  and [pat] is [None],
      - [Some P]               when [I] is ["Some"]  and [pat] is [Some P],
      - [[]]                   when [I] is ["[]"]    and [pat] is [None],
      - [[ P ]]                when [I] is ["::"]    and [pat] is
        [Some (Pat_tuple(P, Pat_construct("[]", None), []))],
      - [[ P1; P2; ... ]]      when [I] is ["::"]    and [pat] is
        [Some (Pat_tuple(P1, Pat_construct("::", Some (Pat_tuple(P2, Pat_construct("::", ...), []))), []))]. *)
  | Pat_constraint of pattern * core_type
  (** [Pat_constraint(P, T)] represents [P : T]. *)

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

(** [{P; E}] represents [let P = E]. *)
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

(** [{P; E}] represents [P -> E]. *)
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
    | Exp_ident of ident (** An identifier such as [x]. *)
    | Exp_constant of constant (** An expression such as [1], ['a'], ["true"]. *)
    | Exp_let of rec_flag * value_binding_exp * value_binding_exp list_ * t
    (** [Exp_let(flag, {P1; E1}, [{P2; E2}; ... ; {Pn; En}], E)] represents
        - [let     P1 = E1 and P2 = E2 and ... and Pn = En in E] when [flag] is [Nonrecursive],
        - [let rec P1 = E1 and P2 = E2 and ... and Pn = En in E] when [flag] is [Recursive]. *)
    | Exp_fun of pattern * pattern list_ * t
    (** [Exp_fun(P1, [P2; ... ; Pn], E)] represents [fun P1 ... Pn -> E] *)
    | Exp_apply of (t * t)
    (** [Exp_ident(I, Exp_apply(E1, E2))] represents [E1 I E2],
        [Exp_apply(Exp_ident(I), E)] represents [I E],
        [Exp_apply(Exp_ident(I), Exp_apply(E1, Exp_apply(E2, ...)))] represents [I E1 E2 ... ]. *)
    | Exp_function of case_exp * case_exp list_
    (** [Exp_function({P1; E1}, [{P2; E2}; ... ; {Pn; En}])] represents
        [function P1 -> E1 | P2 -> E2 | ... | Pn -> En]. *)
    | Exp_match of t * case_exp * case_exp list_
    (** [Exp_match(E, {P1; E1}, [{P2; E2}; ... ; {Pn; En}])] represents
        [match E with P1 -> E1 | P2 -> E2 | ... | Pn -> En]. *)
    | Exp_ifthenelse of t * t * t option
    (** [Exp_ifthenelse(E1, E2, opt)] represents
        - [if E1 then E2]         when [opt] is [None],
        - [if E1 then E2 else E3] when [opt] is [Some E3]. *)
    | Exp_tuple of t * t * t list_
    (** [Exp_tuple(E1, E2, [E3; ... ; En])] represents [(E1, ... , En)]. *)
    | Exp_construct of (ident * t option)
    (** [Exp_construct(I, exp)] represents
        - [()]                   when [I] is ["()"]    and [exp] is [None],
        - [false]                when [I] is ["false"] and [exp] is [None],
        - [true]                 when [I] is ["true"]  and [exp] is [None],
        - [None]                 when [I] is ["None"]  and [exp] is [None],
        - [Some E]               when [I] is ["Some"]  and [exp] is [Some E],
        - [[]]                   when [I] is ["[]"]    and [exp] is [None],
        - [[ E ]]                when [I] is ["::"]    and [exp] is
          [Some (Exp_tuple(E, Exp_construct("[]", None), []))],
        - [[ E1; E2; ... ]]      when [I] is ["::"]    and [exp] is
          [Some (Exp_tuple(E1, Exp_construct("::", Some (Exp_tuple(E2, Exp_construct("::", ...), []))), []))]. *)
    | Exp_sequence of t * t (** [Exp_sequence(E1, E2)] represents [E1; E2]. *)
    | Exp_constraint of t * core_type (** [Exp_constraint(E, T)] represents [(E : T)]. *)

  val pp : Format.formatter -> t -> unit
  val show : t -> ident
end

type structure_item =
  | Struct_eval of Expression.t (** [Struct_eval(E)] represents [E]. *)
  | Struct_value of
      rec_flag * Expression.value_binding_exp * Expression.value_binding_exp list_
  (** [Struct_value(flag, {P1; E1}, [{P2; E2}; ... ; {Pn; En}], E)] represents
      - [let     P1 = E1 and P2 = E2 and ... and Pn = En in E] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and P2 = E2 and ... and Pn = En in E] when [flag] is [Recursive]. *)

val pp_structure_item : Format.formatter -> structure_item -> unit
val show_structure_item : structure_item -> string

type structure = structure_item list_

val pp_structure : Format.formatter -> structure -> unit
val show_structure : structure -> string
val gen_structure : structure QCheck.Gen.t
val arb_structure : structure QCheck.arbitrary
