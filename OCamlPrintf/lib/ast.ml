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
  | Const_string of (string[@gen string_printable])
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
  type value_binding_exp = t value_binding
  and case_exp = t case

  and t =
    | Exp_ident of ident
    | Exp_constant of constant
    | Exp_let of
        rec_flag
        * (value_binding_exp
          [@gen map2 (fun pat exp -> { pat; exp }) gen_pattern (gen_sized (n / coef))])
        * (value_binding_exp
          [@gen map2 (fun pat exp -> { pat; exp }) gen_pattern (gen_sized (n / coef))])
            list_
        * (t[@gen gen_sized (n / coef)])
    | Exp_fun of
        (pattern list[@gen small_list gen_pattern]) * (t[@gen gen_sized (n / coef)])
    | Exp_apply of
        (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)])
        * (t list[@gen small_list (gen_sized (n / coef))])
    | Exp_match of
        (t[@gen gen_sized (n / coef)])
        * (case_exp
          [@gen
            map2 (fun left right -> { left; right }) gen_pattern (gen_sized (n / coef))])
        * (case_exp
          [@gen
            map2 (fun left right -> { left; right }) gen_pattern (gen_sized (n / coef))])
            list_
    | Exp_tuple of
        (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)])
        * (t list[@gen small_list (gen_sized (n / coef))])
    | Exp_construct of ident * (t[@gen gen_sized (n / coef)]) option
    | Exp_ifthenelse of
        (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)]) option
    | Exp_sequence of (t[@gen gen_sized (n / coef)]) * (t[@gen gen_sized (n / coef)])
    | Exp_constraint of (t[@gen gen_sized (n / coef)]) * core_type
  [@@deriving show { with_path = false }, qcheck]
end

type structure_item =
  | Struct_eval of Expression.t
  | Struct_value of
      rec_flag
      * Expression.t value_binding
      * (Expression.t value_binding list
        [@gen small_list (gen_value_binding Expression.gen)])
[@@deriving show { with_path = false }, qcheck]

type structure = (structure_item list[@gen small_list gen_structure_item])
[@@deriving show { with_path = false }, qcheck]
