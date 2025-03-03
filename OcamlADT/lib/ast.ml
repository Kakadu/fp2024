(** Copyright 2024, Rodion Suvorov, Mikhail Gavrilenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Base
open Gen
open Stdlib

type ident = string [@@deriving eq, show { with_path = false }]

let gen_charc = map Char.chr (int_range (Char.code 'a') (Char.code 'z'))

let is_not_keyword = function
  | "let"
  | "if"
  | "then"
  | "else"
  | "in"
  | "fun"
  | "true"
  | "false"
  | "rec"
  | "and"
  | "function"
  | "match"
  | "with"
  | "type"
  | "of" -> false
  | _ -> true
;;

let rec gen_filtered_ident base_gen =
  let open QCheck.Gen in
  base_gen
  >>= fun ident ->
  if is_not_keyword ident then return ident else gen_filtered_ident base_gen
;;

let gen_ident =
  let base_gen =
    map2
      (fun start_sym rest_sym -> Base.Char.to_string start_sym ^ rest_sym)
      (oneof [ char_range 'A' 'Z'; char_range 'a' 'z'; return '_' ])
      (small_string
         ~gen:
           (oneof
              [ char_range 'A' 'Z'; char_range 'a' 'z'; char_range '0' '9'; return '_' ]))
  in
  gen_filtered_ident base_gen
;;

let gen_ident_uc =
  let base_gen =
    map2
      (fun start_sym rest_sym -> Base.Char.to_string start_sym ^ rest_sym)
      (char_range 'A' 'Z')
      (small_string
         ~gen:
           (oneof
              [ char_range 'A' 'Z'; char_range 'a' 'z'; char_range '0' '9'; return '_' ]))
  in
  gen_filtered_ident base_gen
;;

let gen_ident_lc include_us =
  let start_sym =
    if include_us then oneof [ char_range 'a' 'z'; return '_' ] else char_range 'a' 'z'
  in
  let base_gen =
    map2
      (fun start_sym rest_sym -> Base.Char.to_string start_sym ^ rest_sym)
      start_sym
      (small_string
         ~gen:
           (oneof
              [ char_range 'A' 'Z'; char_range 'a' 'z'; char_range '0' '9'; return '_' ]))
  in
  gen_filtered_ident base_gen
;;

module List1 = struct
  type 'a t = 'a * ('a list[@gen list_size (int_bound 5) gen_a])
  [@@deriving eq, show { with_path = false }, qcheck]
end

module List2 = struct
  type 'a t = 'a * 'a * ('a list[@gen list_size (int_bound 5) gen_a])
  [@@deriving eq, show { with_path = false }, qcheck]
end

module Constant = struct
  type t =
    | Const_integer of (int[@gen small_nat]) (** integer as [52] *)
    | Const_char of (char[@gen gen_charc]) (** char as ['w'] *)
    | Const_string of (string[@gen small_string ~gen:gen_charc])
    (** string as ["Kakadu"] *)
  [@@deriving eq, show { with_path = false }, qcheck]
end

module TypeExpr = struct
  type t =
    | Type_arrow of t * t (** [Type_arrow(T1, T2)] represents:
                              [T1 -> T2] *)
    | Type_var of (ident[@gen gen_ident])
    | Type_tuple of t List2.t (** [Type_tuple([T1, T2, ... Tn])] *)
    | Type_construct of ident * t list
    (** [Type_construct(lident, l)] represents:
        - [tconstr]               when [l=[]],
        - [T tconstr]             when [l=[T]],
        - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]]. *)
  [@@deriving eq, show { with_path = false }, qcheck]
end

module Pattern = struct
  type t =
    | Pat_constraint of t * (TypeExpr.t[@gen TypeExpr.gen_sized (n / 2)])
    (** Pattern [(P : T)] *)
    | Pat_any (** The pattern [_]. *)
    | Pat_var of (ident[@gen gen_ident_lc false]) (** A variable pattern such as [x] *)
    | Pat_constant of Constant.t (** Patterns such as [52], ['w'], ["uwu"] *)
    | Pat_tuple of t List2.t (** Patterns [(P1, ..., Pn)]. *)
    | Pat_construct of (ident[@gen gen_ident_uc]) * t option
    (** [Pat_construct(C, args)] represents:
        - [C]               when [args] is [None],
        - [C P]             when [args] is [Some (P)]
        - [C (P1, ..., Pn)] when [args] is
          [Some (Pat_tuple [P1; ...; Pn])] *)
  [@@deriving eq, show { with_path = false }, qcheck]
end

module Expression = struct
  type rec_flag =
    | Nonrecursive
    | Recursive
  [@@deriving eq, show { with_path = false }, qcheck]

  type 'expr value_binding =
    { pat : Pattern.t
    ; expr : 'expr
    }
  [@@deriving eq, show { with_path = false }]

  let gen_value_binding gen_expr n =
    map2 (fun pat expr -> { pat; expr }) (Pattern.gen_sized (n / 2)) (gen_expr (n / 2))
  ;;

  type 'expr case =
    { first : Pattern.t
    ; second : 'expr
    }
  [@@deriving eq, show { with_path = false }]

  let gen_case gen_expr n =
    map2
      (fun first second -> { first; second })
      (Pattern.gen_sized (n / 2))
      (gen_expr (n / 2))
  ;;

  type t =
    | Exp_ident of (ident[@gen gen_ident_lc true]) (** Identifiers such as [x] *)
    | Exp_constant of Constant.t (** Expressions constant such as [1], ['a'], ["true"]**)
    | Exp_tuple of t List2.t (** Expressions [(E1, E2, ..., En)] *)
    | Exp_function of (t case[@gen gen_case gen_sized (n / 2)]) List1.t
    (** [Exp_function (P1, [P2; ...; Pn])] represents
        [function P1 | ... | Pn] *)
    | Exp_fun of (Pattern.t[@gen Pattern.gen_sized (n / 2)]) List1.t * t
    (**[Exp_fun (P1, [P2; ...; Pn], E)] represents:
       [fun P1 ... Pn -> E] *)
    | Exp_apply of t * t (** [Pexp_apply(E0, E1)]
                             represents [E0 E1]*)
    | Exp_match of t * (t case[@gen gen_case gen_sized (n / 2)]) List1.t
    (** [match E0 with P1 -> E1 || Pn -> En] *)
    | Exp_constraint of t * (TypeExpr.t[@gen TypeExpr.gen_sized (n / 2)]) (** [(E : T)] *)
    | Exp_if of t * t * t option (** [if E1 then E2 else E3] *)
    | Exp_let of
        rec_flag * (t value_binding[@gen gen_value_binding gen_sized (n / 2)]) List1.t * t
    (** [Exp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
        - [let P1 = E1 and ... and Pn = EN in E]
          when [flag] is [Nonrecursive],
        - [let rec P1 = E1 and ... and Pn = EN in E]
          when [flag] is [Recursive]. *)
    | Exp_construct of (ident[@gen gen_ident_uc]) * t option
    (** [Exp_construct(C, exp)] represents:
        - [C]               when [exp] is [None],
        - [C E]             when [exp] is [Some E],
        - [C (E1, ..., En)] when [exp] is [Some (Exp_tuple[E1;...;En])] *)
  [@@deriving eq, show { with_path = false }, qcheck]
end

module Structure = struct
  type structure_item =
    | Str_eval of Expression.t
    | Str_value of Expression.rec_flag * Expression.t Expression.value_binding List1.t
    (** [Str_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
        - [let P1 = E1 and ... and Pn = EN]
          when [rec] is [Nonrecursive],
        - [let rec P1 = E1 and ... and Pn = EN ]
          when [rec] is [Recursiv e ee]. *)
    | Str_adt of ident list * ident * (ident * TypeExpr.t option) List1.t
    (** [Str_type(C0, [(C1, [(T11; T12; ... ; T1n_1)]); (C2, [(T21;T22; ... ; T2n_2)]); ... ;
      (Cm, [(Tm1;Tm2; ... ; Tmn_n)]) ])] represents:

        [type C0 =
      | C1 of T11 * ... * T1n_1
      | ...
      | Cm of Tm1 * ... * Tmn_n
      ]

        n_i: [n_i >= 0]
        Invariant: [m > 0] *)
  [@@deriving eq, show { with_path = false }]

  let gen_structure_item n =
    frequency
      [ 0, map (fun expr -> Str_eval expr) (Expression.gen_sized (n / 2))
      ; ( 0
        , let* rec_flag =
            oneof [ return Expression.Nonrecursive; return Expression.Recursive ]
          in
          let* bind1 = Expression.gen_value_binding Expression.gen_sized (n / 2) in
          let* bindl =
            small_list (Expression.gen_value_binding Expression.gen_sized (n / 2))
          in
          return (Str_value (rec_flag, (bind1, bindl))) )
      ; ( 1
        , let* tparam = small_list (gen_ident_lc true) in
          let* idt = gen_ident_lc true in
          let* cons1 = Gen.pair gen_ident_uc (Gen.option (TypeExpr.gen_sized (n / 20))) in
          let* consl =
            small_list (Gen.pair gen_ident_uc (Gen.option (TypeExpr.gen_sized (n / 20))))
          in
          return (Str_adt (tparam, idt, (cons1, consl))) )
      ]
  ;;
end

type program = Structure.structure_item list [@@deriving eq, show { with_path = false }]

module Program = struct
  let gen_program n = list_size (int_bound 6) (Structure.gen_structure_item (n / 2))
end
