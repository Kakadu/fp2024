(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter

module TestQCheck = struct
  open QCheck.Gen

  (* Manual generator *)

  let gen_char = map Char.chr (int_range (Char.code 'a') (Char.code 'z'))
  let gen_int = int_range 0 10000
  let gen_ident = string_size (int_range 1 2) ~gen:gen_char

  let gen_constant =
    frequency
      [ 1, map (fun i -> Const_integer i) gen_int
      ; 1, map (fun c -> Const_char c) gen_char
      ; 1, map (fun s -> Const_string s) gen_ident
      ]
  ;;

  let gen_pattern =
    sized
    @@ fix (fun self n ->
      match n with
      | 0 ->
        frequency
          [ (* 1, return Pat_any
               ; *)
            1, map (fun i -> Pat_var i) gen_ident
          ; 1, map (fun c -> Pat_constant c) gen_constant
          ]
      | n ->
        frequency
          [ 1, map (fun t -> Pat_tuple t) (list_size (int_range 2 2) (self (n / 10)))
            (* ; ( 1
               , map2
               (fun name pat_opt -> Pat_construct (name, pat_opt))
               gen_ident
               (option (self (n / 10))) ) *)
          ])
  ;;

  let gen_expression =
    sized
    @@ fix (fun self n ->
      match n with
      | 0 ->
        frequency
          [ 1, map (fun i -> Exp_ident i) gen_ident
          ; 1, map (fun c -> Exp_constant c) gen_constant
          ]
      | n ->
        frequency
          [ (* ( 1
               , map3
               (fun rec_fl value exp -> Exp_let (rec_fl, value, exp))
               (frequency [ 1, return Nonrecursive; 1, return Recursive ])
               (list_size (int_range 1 2) gen_value_binding)
               (self (n / 2)) )
               ; *)
            ( 1
            , map2
                (fun pat exp -> Exp_fun (pat, exp))
                (list_size (int_range 1 2) gen_pattern)
                (self (n / 10)) )
          ; ( 1
            , map2
                (fun exp exp_list -> Exp_apply (exp, exp_list))
                (self (n / 10))
                (list_size (int_range 1 2) (self (n / 10))) )
            (* ; ( 1
               , map2
               (fun exp case -> Exp_match (exp, case))
               (self (n / 2))
               (list_size (int_range 1 2) gen_case) ) *)
            (* ; 1, map (fun exp -> Exp_tuple exp) (list_size (int_range 1 2) (self (n / 10))) *)
            (* ; ( 1
               , map2
               (fun name pat_opt -> Exp_construct (name, pat_opt))
               gen_ident
               (option (self (n / 10))) ) *)
          ; ( 1
            , map3
                (fun i t e -> Exp_ifthenelse (i, t, e))
                (self (n / 10))
                (self (n / 10))
                (option (self (n / 10))) )
          ; ( 1
            , map2
                (fun exp1 exp2 -> Exp_sequence (exp1, exp2))
                (self (n / 10))
                (self (n / 10)) )
          ])
  ;;

  let gen_value_binding = map2 (fun pat exp -> { pat; exp }) gen_pattern gen_expression
  (* and gen_case = map2 (fun left right -> { left; right }) gen_pattern gen_expression *)

  let gen_structure_item =
    frequency
      [ 1, map (fun exp -> Struct_eval exp) gen_expression
      ; ( 1
        , map2
            (fun rec_flag value -> Struct_value (rec_flag, value))
            (frequency [ 1, return Nonrecursive; 1, return Recursive ])
            (list_size (int_range 1 2) gen_value_binding) )
      ]
  ;;

  let gen_structure = list_size (int_range 1 1) gen_structure_item

  let arbitrary_lam_manual =
    QCheck.make gen_structure ~print:(Format.asprintf "%a" pp_structure)
  ;;

  let run_manual () =
    QCheck_runner.run_tests
      [ QCheck.(
          Test.make arbitrary_lam_manual (fun l ->
            Result.ok l = Parser.parse (Format.asprintf "%a" Pprinter.pp_structure l)))
      ]
  ;;
end
