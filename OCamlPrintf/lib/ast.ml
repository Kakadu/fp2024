(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck.Gen

let coef = 50 (* For the generator's speed. *)

type 'a list_ = ('a list[@gen small_list gen_a])
[@@deriving show { with_path = false }, qcheck]

let gen_char = oneof [ return '!'; char_range '#' '&'; char_range '(' '~' ]
(* Exception quotation marks. *)

let gen_ident =
  map2
    (fun start_ident rest_ident -> Base.Char.to_string start_ident ^ rest_ident)
    (oneof [ char_range 'a' 'z'; return '_' ])
    (small_string
       ~gen:
         (oneof
            [ char_range '0' '9'
            ; char_range 'A' 'Z'
            ; char_range 'a' 'z'
            ; return '_'
            ; return '\''
            ]))
;;

let gen_ident_construct =
  oneof
    [ return "::"
    ; return "[]"
    ; return "Some"
    ; return "None"
    ; return "true"
    ; return "false"
    ]
;;

type ident = (string[@gen gen_ident]) [@@deriving show { with_path = false }, qcheck]

type rec_flag =
  | Recursive
  | Nonrecursive
[@@deriving show { with_path = false }, qcheck]

type constant =
  | Const_integer of (int[@gen nat])
  | Const_char of (char[@gen gen_char])
  | Const_string of (string[@gen small_string ~gen:gen_char])
[@@deriving show { with_path = false }, qcheck]

type core_type =
  | Type_any
  | Type_char
  | Type_int
  | Type_string
  | Type_bool
  | Type_list of (core_type[@gen gen_core_type_sized (n / coef)])
  | Type_tuple of
      (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)]) list_
  | Type_arrow of
      (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)])
[@@deriving show { with_path = false }, qcheck]

type pattern =
  | Pat_any
  | Pat_var of ident
  | Pat_constant of constant
  | Pat_tuple of
      (pattern[@gen gen_pattern_sized (n / coef)])
      * (pattern[@gen gen_pattern_sized (n / coef)])
      * (pattern[@gen gen_pattern_sized (n / coef)]) list_
  | Pat_construct of
      (ident[@gen gen_ident_construct])
      * (pattern[@gen gen_pattern_sized (n / coef)]) option
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
    | Exp_fun of pattern list_ * (t[@gen gen_sized (n / coef)])
    | Exp_apply of
        (t
        [@gen
          oneof
            [ gen_sized (n / coef)
            ; oneofl
                [ Exp_ident "*"
                ; Exp_ident "/"
                ; Exp_ident "+"
                ; Exp_ident "-"
                ; Exp_ident ">="
                ; Exp_ident "<="
                ; Exp_ident "<>"
                ; Exp_ident "="
                ; Exp_ident ">"
                ; Exp_ident "<"
                ; Exp_ident "&&"
                ; Exp_ident "||"
                ]
            ]])
        * (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)]) list_
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
        * (t[@gen gen_sized (n / coef)]) list_
    | Exp_construct of
        (ident[@gen gen_ident_construct]) * (t[@gen gen_sized (n / coef)]) option
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
      rec_flag * Expression.t value_binding * Expression.t value_binding list_
[@@deriving show { with_path = false }, qcheck]

type structure = structure_item list_ [@@deriving show { with_path = false }, qcheck]
