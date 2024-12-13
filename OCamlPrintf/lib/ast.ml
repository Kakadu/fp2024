(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck.Gen

let coef = 50 (* For the generator's speed. *)
let min_range = int_range 0 10 (* For the generator's speed. *)
let gen_string gen = string_size min_range ~gen
let gen_list gen = list_size min_range gen
let gen_operand gen = list_size (int_range 1 1) gen

type 'a list_ = ('a list[@gen gen_list gen_a])
[@@deriving show { with_path = false }, qcheck]

let gen_char =
  (* Exception quotation marks and backslash. *)
  oneof [ return '!'; char_range '#' '&'; char_range '(' '['; char_range ']' '~' ]
;;

let is_keyword = function
  | "and"
  | "else"
  | "false"
  | "fun"
  | "if"
  | "in"
  | "let"
  | "match"
  | "rec"
  | "then"
  | "true"
  | "with"
  | "Some"
  | "None" -> true
  | _ -> false
;;

let gen_ident =
  let gen_var =
    map2
      (fun start_ident rest_ident ->
        match Base.Char.to_string start_ident ^ rest_ident with
        | "_" -> "id"
        | id -> id)
      (oneof [ char_range 'a' 'z'; return '_' ])
      (gen_string
         (oneof
            [ char_range '0' '9'
            ; char_range 'A' 'Z'
            ; char_range 'a' 'z'
            ; return '_'
            ; return '\''
            ]))
  in
  gen_var >>= fun name -> if is_keyword name then gen_var else return name
;;

type ident = (string[@gen gen_ident]) [@@deriving show { with_path = false }, qcheck]

type rec_flag =
  | Recursive
  | Nonrecursive
[@@deriving show { with_path = false }, qcheck]

type constant =
  | Const_integer of (int[@gen nat])
  | Const_char of (char[@gen gen_char])
  | Const_string of (string[@gen gen_string gen_char])
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
      ((ident * pattern option)
      [@gen
        oneof
          [ (let rec gen_list_pat n =
               if n = 0
               then return ("[]", None)
               else (
                 let element = gen_pattern_sized 0 in
                 let tail = gen_list_pat (n / coef) in
                 map2
                   (fun e t -> "::", Some (Pat_tuple (e, Pat_construct t, [])))
                   element
                   tail)
             in
             gen_list_pat n)
          ; return ("true", None)
          ; return ("false", None)
          ; map (fun i -> "Some", Some i) (gen_pattern_sized (n / coef))
          ; return ("None", None)
          ]])
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
    | Exp_fun of pattern * pattern list_ * (t[@gen gen_sized (n / coef)])
    | Exp_apply of
        ((t * t * t list)
        [@gen
          oneof
            [ map3
                (fun exp first_exp exp_list -> exp, first_exp, exp_list)
                (gen_sized 0)
                (gen_sized (n / coef))
                (gen_list (gen_sized (n / coef)))
            ; map3
                (fun op exp1 exp2 -> op, exp1, exp2)
                (oneofl
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
                   ])
                (gen_sized (n / coef))
                (gen_operand (gen_sized (n / coef)))
            ]])
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
        ((ident * t option)
        [@gen
          oneof
            [ (let rec gen_list_exp n =
                 if n = 0
                 then return ("[]", None)
                 else (
                   let element = gen_sized 0 in
                   let tail = gen_list_exp (n / coef) in
                   map2
                     (fun e t -> "::", Some (Exp_tuple (e, Exp_construct t, [])))
                     element
                     tail)
               in
               gen_list_exp n)
            ; return ("true", None)
            ; return ("false", None)
            ; map (fun i -> "Some", Some i) (gen_sized (n / coef))
            ; return ("None", None)
            ]])
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
