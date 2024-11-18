(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(* Only for debug *)
let coef = 50

module TestQCheckManual = struct
  open QCheck.Gen

  let gen_char = map Char.chr (int_range (Char.code 'a') (Char.code 'h'))
  let gen_int = int_range 0 10000

  let gen_bin_op =
    oneofl
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
  ;;

  let gen_ident = string_size (int_range 1 3) ~gen:gen_char

  let gen_constant =
    frequency
      [ 1, map (fun i -> Const_integer i) gen_int
      ; 1, map (fun c -> Const_char c) gen_char
      ; 1, map (fun s -> Const_string s) gen_ident
      ]
  ;;

  let gen_pattern =
    sized
    @@ fix (fun self ->
         function
         | 0 ->
           frequency
             [ 1, return Pat_any
             ; 1, map (fun i -> Pat_var i) gen_ident
             ; 1, map (fun c -> Pat_constant c) gen_constant
             ]
         | n ->
           frequency
             [ ( 1
               , map3
                   (fun f s l -> Pat_tuple (f, s, l))
                   (self (n / coef))
                   (self (n / coef))
                   (list_size (int_range 0 5) (self (n / coef))) )
             ; 1, return (Pat_construct ("[]", None))
             ; ( 1
               , let rec gen_list n =
                   if n = 0
                   then return (Pat_construct ("[]", None))
                   else (
                     let element = self 0 in
                     let tail = gen_list (n - 1) in
                     map2
                       (fun e t -> Pat_construct ("::", Some (Pat_tuple (e, t, []))))
                       element
                       tail)
                 in
                 gen_list (Random.int 5) )
             ])
  ;;

  let gen_expression =
    sized
    @@ fix (fun self ->
         function
         | 0 ->
           frequency
             [ 1, map (fun i -> Exp_ident i) gen_ident
             ; 1, map (fun c -> Exp_constant c) gen_constant
             ]
         | n ->
           frequency
             [ ( 1
               , map3
                   (fun rec_fl first_value_binding value_binding_list exp ->
                     Exp_let (rec_fl, first_value_binding, value_binding_list, exp))
                   (frequency [ 1, return Nonrecursive; 1, return Recursive ])
                   (map2 (fun pat exp -> { pat; exp }) gen_pattern (self (Random.int 3)))
                   (list_size
                      (int_range 1 3)
                      (map2
                         (fun pat exp -> { pat; exp })
                         gen_pattern
                         (self (Random.int 3))))
                 <*> self (n / coef) )
             ; ( 1
               , map2
                   (fun pat exp -> Exp_fun (pat, exp))
                   (list_size (int_range 1 3) gen_pattern)
                   (self (n / coef)) )
             ; ( 1
               , map3
                   (fun exp first_exp exp_list -> Exp_apply (exp, first_exp, exp_list))
                   (self 0)
                   (self (n / coef))
                   (list_size (int_range 1 3) (self (n / coef))) )
             ; ( 1
               , map3
                   (fun op exp1 exp2 -> Exp_apply (op, exp1, exp2))
                   gen_bin_op
                   (self (n / coef))
                   (list_size (int_range 1 1) (self (n / coef))) )
             ; ( 1
               , map3
                   (fun exp first_case case_list ->
                     Exp_match (exp, first_case, case_list))
                   (self (n / coef))
                   (map2
                      (fun left right -> { left; right })
                      gen_pattern
                      (self (Random.int 3)))
                   (list_size
                      (int_range 1 3)
                      (map2
                         (fun left right -> { left; right })
                         gen_pattern
                         (self (Random.int 3)))) )
             ; ( 1
               , map3
                   (fun first second list -> Exp_tuple (first, second, list))
                   (self (n / coef))
                   (self (n / coef))
                   (list_size (int_range 0 5) (self (n / coef))) )
             ; 1, return (Exp_construct ("[]", None))
             ; ( 1
               , let rec gen_list n =
                   if n = 0
                   then return (Exp_construct ("[]", None))
                   else (
                     let element = self 0 in
                     let tail = gen_list (n - 1) in
                     map2
                       (fun e t -> Exp_construct ("::", Some (Exp_tuple (e, t, []))))
                       element
                       tail)
                 in
                 gen_list (Random.int 5) )
             ; ( 1
               , map3
                   (fun i t e -> Exp_ifthenelse (i, t, e))
                   (self (n / coef))
                   (self (n / coef))
                   (option (self (n / coef))) )
             ; ( 1
               , map2
                   (fun exp1 exp2 -> Exp_sequence (exp1, exp2))
                   (self (n / coef))
                   (self (n / coef)) )
             ])
  ;;

  let gen_value_binding = map2 (fun pat exp -> { pat; exp }) gen_pattern gen_expression

  let gen_structure_item =
    frequency
      [ 1, map (fun exp -> Struct_eval exp) gen_expression
      ; ( 1
        , map3
            (fun rec_flag first_value_binding value_binding_list ->
              Struct_value (rec_flag, first_value_binding, value_binding_list))
            (frequency [ 1, return Nonrecursive; 1, return Recursive ])
            gen_value_binding
            (list_size (int_range 1 3) gen_value_binding) )
      ]
  ;;

  let gen_structure = list_size (int_range 1 1) gen_structure_item

  let arbitrary_lam_manual =
    QCheck.make
      gen_structure
      ~print:(Format.asprintf "%a" pp_structure)
      ~shrink:Shrinker.shrink_structure
  ;;

  let run_manual () =
    QCheck_base_runner.run_tests
      [ QCheck.(
          Test.make arbitrary_lam_manual (fun str ->
            Format.printf "%a \n" Pprinter.pp_structure str;
            Result.ok str = Parser.parse (Format.asprintf "%a" Pprinter.pp_structure str)))
      ]
  ;;
end

module TestQCheckAuto = struct
  open QCheck.Gen

  type 'a list_ = ('a list[@gen small_list gen_a]) [@@deriving qcheck]

  let gen_ident =
    small_string ~gen:(map Char.chr (int_range (Char.code 'a') (Char.code 'h')))
  ;;

  type ident = (Ast.ident[@gen gen_ident]) [@@deriving qcheck]

  type rec_flag = Ast.rec_flag =
    | Recursive
    | Nonrecursive
  [@@deriving qcheck]

  type constant = Ast.constant =
    | Const_integer of int
    | Const_char of char
    | Const_string of (string[@gen small_string])
  [@@deriving qcheck]

  type core_type = Ast.core_type =
    | Type_any
    | Type_char
    | Type_int
    | Type_string
    | Type_bool
    | Type_list of core_type
    | Type_tuple of core_type list_
  [@@deriving qcheck]

  type patt = Ast.pattern =
    | Pat_any
    | Pat_var of ident
    | Pat_constant of constant
    | Pat_tuple of
        (patt[@gen gen_patt_sized (n / coef)])
        * (patt[@gen gen_patt_sized (n / coef)])
        * (patt list[@gen small_list (gen_patt_sized (n / coef))])
    | Pat_construct of ident * (patt[@gen gen_patt_sized (n / coef)]) option
    | Pat_constraint of (patt[@gen gen_patt_sized (n / coef)]) * core_type
  [@@deriving qcheck]

  type expr = Ast.expression =
    | Exp_ident of ident
    | Exp_constant of constant
    | Exp_let of
        rec_flag
        * (value_binding
          [@gen map2 (fun pat exp -> { pat; exp }) gen_patt (gen_expr_sized (n / coef))])
        * (value_binding
          [@gen map2 (fun pat exp -> { pat; exp }) gen_patt (gen_expr_sized (n / coef))])
            list_
        * (expr[@gen gen_expr_sized (n / coef)])
    | Exp_fun of
        (patt list[@gen small_list gen_patt]) * (expr[@gen gen_expr_sized (n / coef)])
    | Exp_apply of
        (expr[@gen gen_expr_sized (n / coef)])
        * (expr[@gen gen_expr_sized (n / coef)])
        * (expr list[@gen small_list (gen_expr_sized (n / coef))])
    | Exp_match of
        (expr[@gen gen_expr_sized (n / coef)])
        * (case
          [@gen
            map2 (fun left right -> { left; right }) gen_patt (gen_expr_sized (n / coef))])
        * (case
          [@gen
            map2 (fun left right -> { left; right }) gen_patt (gen_expr_sized (n / coef))])
            list_
    | Exp_tuple of
        (expr[@gen gen_expr_sized (n / coef)])
        * (expr[@gen gen_expr_sized (n / coef)])
        * (expr list[@gen small_list (gen_expr_sized (n / coef))])
    | Exp_construct of ident * (expr[@gen gen_expr_sized (n / coef)]) option
    | Exp_ifthenelse of
        (expr[@gen gen_expr_sized (n / coef)])
        * (expr[@gen gen_expr_sized (n / coef)])
        * (expr[@gen gen_expr_sized (n / coef)]) option
    | Exp_sequence of
        (expr[@gen gen_expr_sized (n / coef)]) * (expr[@gen gen_expr_sized (n / coef)])
    | Exp_constraint of (expr[@gen gen_expr_sized (n / coef)]) * core_type
  [@@deriving qcheck]

  type value_binding = Ast.value_binding =
    { pat : patt
    ; exp : expr
    }
  [@@deriving qcheck]

  type case = Ast.case =
    { left : patt
    ; right : expr
    }
  [@@deriving qcheck]

  type structure_item = Ast.structure_item =
    | Struct_eval of expr
    | Struct_value of
        rec_flag * value_binding * (value_binding list[@gen small_list gen_value_binding])
  [@@deriving qcheck]

  type structure = (structure_item list[@gen small_list gen_structure_item])
  [@@deriving qcheck]

  let arbitrary_lam_auto =
    QCheck.make
      gen_structure
      ~print:(Format.asprintf "%a" Pprinter.pp_structure)
      ~shrink:Shrinker.shrink_structure
  ;;

  let run_auto () =
    QCheck_base_runner.run_tests
      [ QCheck.(
          Test.make ~count:1 arbitrary_lam_auto (fun str ->
            Format.printf "%a \n" Pprinter.pp_structure str;
            match Parser.parse (Format.asprintf "%a" Pprinter.pp_structure str) with
            | Result.Ok after when after = str -> true
            | Result.Ok _after -> false
            | Result.Error _ -> false))
      ]
  ;;
end
