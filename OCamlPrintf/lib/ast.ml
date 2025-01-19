(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck.Gen

let coef = 10 (* For the generator's speed. *)
let min_range = int_range 0 10 (* For the generator's speed. *)
let gen_string gen = string_size min_range ~gen
let gen_list gen = list_size min_range gen

type 'a list_ = ('a list[@gen gen_list gen_a])
[@@deriving show { with_path = false }, qcheck]

let gen_char =
  (* Exception quotation marks and backslash. *)
  oneof [ return '!'; char_range '#' '&'; char_range '(' '['; char_range ']' '~' ]
;;

let un_op_list = [ "~-", 1 ]

let bin_op_list =
  [ "*", 1
  ; "/", 1
  ; "+", 2
  ; "-", 2
  ; "^", 3
  ; ">=", 4
  ; "<=", 4
  ; "<>", 4
  ; "=", 4
  ; ">", 4
  ; "<", 4
  ; "&&", 5
  ; "||", 6
  ]
;;

let is_operator opr = List.exists (fun (str, _) -> str = opr) bin_op_list
let is_negative_op opr = List.exists (fun (str, _) -> str = opr) un_op_list
let get_priority opr = List.assoc opr bin_op_list

let is_keyword = function
  | "and"
  | "else"
  | "false"
  | "fun"
  | "if"
  | "in"
  | "let"
  | "function"
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
  let gen_id =
    map2
      (fun fst_char rest_str ->
        match Base.Char.to_string fst_char ^ rest_str with
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
  gen_id >>= fun id -> if is_keyword id then gen_id else return id
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

let gen_type_var =
  let gen_type_var =
    map3
      (fun fst_char snd_char rest_str ->
        Printf.sprintf "%c%c%s" fst_char snd_char rest_str)
      (oneof [ char_range 'a' 'z' ])
      (oneof [ char_range '0' '9'; char_range 'A' 'Z'; char_range 'a' 'z'; return '_' ])
      (gen_string
         (oneof
            [ char_range '0' '9'
            ; char_range 'A' 'Z'
            ; char_range 'a' 'z'
            ; return '_'
            ; return '\''
            ]))
  in
  gen_type_var
  >>= fun type_var -> if is_keyword type_var then gen_type_var else return ("'" ^ type_var)
;;

type core_type =
  | Type_unit
  | Type_char
  | Type_int
  | Type_string
  | Type_bool
  | Type_option of (core_type[@gen gen_core_type_sized (n / coef)])
  | Type_var of (ident[@gen gen_type_var])
  | Type_list of (core_type[@gen gen_core_type_sized (n / coef)])
  | Type_tuple of
      (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)]) list_
  | Type_arrow of
      (core_type[@gen gen_core_type_sized (n / coef)])
      * (core_type[@gen gen_core_type_sized (n / coef)])
[@@deriving show { with_path = false }, qcheck]

let gen_construct gen n tuple construct =
  oneof
    [ return ("()", None)
    ; return ("true", None)
    ; return ("false", None)
    ; return ("None", None)
    ; map (fun i -> "Some", Some i) (gen (n / coef))
    ; (let rec gen_list n =
         if n = 0
         then return ("[]", None)
         else (
           let element = gen 0 in
           let tail = gen_list (n / coef) in
           map2 (fun e t -> "::", Some (tuple (e, construct t, []))) element tail)
       in
       gen_list n)
    ]
;;

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
        gen_construct
          gen_pattern_sized
          n
          (fun (fst_pat, snd_pat, pat_list) -> Pat_tuple (fst_pat, snd_pat, pat_list))
          (fun (id, pat_opt) -> Pat_construct (id, pat_opt))])
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
  let gen_value_binding gen n fix_exp_fun =
    oneof
      [ map2 (fun var exp -> { pat = Pat_var var; exp }) gen_ident (gen (n / coef))
      ; map3
          (fun id type' exp -> { pat = Pat_constraint (Pat_var id, type'); exp })
          gen_ident
          gen_core_type
          (gen (n / coef))
      ; map2 (fun pat exp -> { pat; exp = fix_exp_fun exp }) gen_pattern (gen (n / coef))
      ]
  ;;

  let gen_exp_apply gen n exp_ident exp_apply =
    oneof
      [ map2 (fun id arg -> exp_ident id, arg) gen_ident (gen (n / coef))
      ; map2
          (fun opr opn -> opr, opn)
          (oneofl (List.map (fun (opr, _) -> exp_ident opr) un_op_list))
          (gen (n / coef))
      ; map3
          (fun opr opn1 opn2 -> opr, exp_apply (opn1, opn2))
          (oneofl (List.map (fun (opr, _) -> exp_ident opr) bin_op_list))
          (gen (n / coef))
          (gen (n / coef))
      ]
  ;;

  type value_binding_exp =
    (t value_binding
    [@gen
      gen_value_binding
        gen_sized
        n
        (let rec fix_exp_fun = function
           | Exp_fun (_, _, exp) -> fix_exp_fun exp
           | Exp_function ({ left = _; right = exp }, _) -> fix_exp_fun exp
           | Exp_constraint (exp, type') -> Exp_constraint (fix_exp_fun exp, type')
           | exp -> exp
         in
         fix_exp_fun)])

  and case_exp =
    (t case
    [@gen map2 (fun left right -> { left; right }) gen_pattern (gen_sized (n / coef))])

  and t =
    | Exp_ident of ident
    | Exp_constant of constant
    | Exp_let of
        rec_flag
        * value_binding_exp
        * value_binding_exp list_
        * (t[@gen gen_sized (n / coef)])
    | Exp_fun of pattern * pattern list_ * (t[@gen gen_sized (n / coef)])
    | Exp_apply of
        ((t * t)
        [@gen
          gen_exp_apply
            gen_sized
            n
            (fun id -> Exp_ident id)
            (fun (opn1, opn2) -> Exp_apply (opn1, opn2))])
    | Exp_function of case_exp * case_exp list_
    | Exp_match of (t[@gen gen_sized (n / coef)]) * case_exp * case_exp list_
    | Exp_ifthenelse of
        (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)]) option
    | Exp_tuple of
        (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)])
        * (t[@gen gen_sized (n / coef)]) list_
    | Exp_construct of
        ((ident * t option)
        [@gen
          gen_construct
            gen_sized
            n
            (fun (fst_exp, snd_exp, exp_list) -> Exp_tuple (fst_exp, snd_exp, exp_list))
            (fun (id, exp_opt) -> Exp_construct (id, exp_opt))])
    | Exp_sequence of (t[@gen gen_sized (n / coef)]) * (t[@gen gen_sized (n / coef)])
    | Exp_constraint of (t[@gen gen_sized (n / coef)]) * core_type
  [@@deriving show { with_path = false }, qcheck]
end

let show_value_binding = Expression.show_value_binding_exp
let show_case = Expression.show_case_exp
let show_expression = Expression.show

type structure_item =
  | Struct_eval of Expression.t
  | Struct_value of
      rec_flag * Expression.value_binding_exp * Expression.value_binding_exp list_
[@@deriving show { with_path = false }, qcheck]

type structure = structure_item list_ [@@deriving show { with_path = false }, qcheck]
