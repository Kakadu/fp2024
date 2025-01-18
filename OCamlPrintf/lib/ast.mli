(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a list_ = 'a list

val show_list_ : (Format.formatter -> 'a -> unit) -> 'a list_ -> string

(** Identifier *)
type ident = string

val show_ident : ident -> string
val un_op_list : (ident * int) list
val bin_op_list : (ident * int) list
val get_priority : string -> int
val is_operator : string -> bool
val is_negative_op : string -> bool
val is_keyword : ident -> bool

type rec_flag =
  | Recursive (** Recursive value binding. *)
  | Nonrecursive (** Nonrecursive value binding. *)

val show_rec_flag : rec_flag -> string

type constant =
  | Const_integer of int (** A constant integer such as [1]. *)
  | Const_char of char (** A constant character such as ['a']. *)
  | Const_string of string (** A constant string such as ["const"]. *)

val show_constant : constant -> string

type core_type =
  | Type_unit (** The type [unit]. *)
  | Type_char (** The type [char]. *)
  | Type_int (** The type [int]. *)
  | Type_string (** The type [string]. *)
  | Type_bool (** The type [bool]. *)
  | Type_option of core_type (** [Type_option(T)] represents [T option]. *)
  | Type_var of ident (** [Type_var(T)] represents [T](a variable type such as ['a]). *)
  | Type_list of core_type (** [Type_list(T)] represents [T list]. *)
  | Type_tuple of core_type * core_type * core_type list_
  (** [Type_tuple(T1, T2, [T3; ... ; Tn])] represents [T1 * ... * Tn]. *)
  | Type_arrow of core_type * core_type (** [Type_arrow(T1, T2)] represents [T1 -> T2]. *)

val show_core_type : core_type -> string

type pattern =
  | Pat_any (** [Pat_any] represents [_]. *)
  | Pat_var of ident (** [Pat_var(I)] represents [I](a variable pattern such as [x]). *)
  | Pat_constant of constant
  (** [Pat_constant(C)] represents [C](a pattern such as [1], ['a'], ["const"]). *)
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

val show_pattern : pattern -> string

(** [{P; E}] represents [let P = E]. *)
type 'exp value_binding =
  { pat : pattern
  ; exp : 'exp
  }

(** [{P; E}] represents [P -> E]. *)
type 'exp case =
  { left : pattern
  ; right : 'exp
  }

module Expression : sig
  type value_binding_exp = t value_binding
  and case_exp = t case

  and t =
    | Exp_ident of ident (** [Exp_ident(I)] represents [I](an identifier such as [x]). *)
    | Exp_constant of constant
    (** [Exp_constant(C)] represents [C](an expression such as [1], ['a'], ["const"]). *)
    | Exp_let of rec_flag * value_binding_exp * value_binding_exp list_ * t
    (** [Exp_let(flag, {P1; E1}, [{P2; E2}; ... ; {Pn; En}], E)] represents
        - [let     P1 = E1 and P2 = E2 and ... and Pn = En in E] when [flag] is [Nonrecursive],
        - [let rec P1 = E1 and P2 = E2 and ... and Pn = En in E] when [flag] is [Recursive]. *)
    | Exp_fun of pattern * pattern list_ * t
    (** [Exp_fun(P1, [P2; ... ; Pn], E)] represents [fun P1 ... Pn -> E] *)
    | Exp_apply of (t * t)
    (** [Exp_apply(Exp_ident(I), E)] represents [I E] when [I] from [un_op_list],
        [Exp_apply(Exp_ident(I), Exp_apply(E1, E2))] represents [E1 I E2] when [I] from [bin_op_list],
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
end

val show_value_binding : Expression.value_binding_exp -> string
val show_case : Expression.case_exp -> string
val show_expression : Expression.t -> ident

type structure_item =
  | Struct_eval of Expression.t (** [Struct_eval(E)] represents [E]. *)
  | Struct_value of
      rec_flag * Expression.value_binding_exp * Expression.value_binding_exp list_
  (** [Struct_value(flag, {P1; E1}, [{P2; E2}; ... ; {Pn; En}], E)] represents
      - [let     P1 = E1 and P2 = E2 and ... and Pn = En in E] when [flag] is [Nonrecursive],
      - [let rec P1 = E1 and P2 = E2 and ... and Pn = En in E] when [flag] is [Recursive]. *)

val show_structure_item : structure_item -> string

type structure = structure_item list_

val show_structure : structure -> string
val gen_structure : structure QCheck.Gen.t
val arb_structure : structure QCheck.arbitrary
