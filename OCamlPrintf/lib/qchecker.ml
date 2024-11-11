(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter

(* Only for debug *)
let coefficient = 50

module TestQCheck = struct
  open QCheck.Gen

  (* Manual generator *)

  let gen_char = map Char.chr (int_range (Char.code 'a') (Char.code 'h'))
  let gen_int = int_range 0 10000
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
               , map
                   (fun t -> Pat_tuple t)
                   (list_size (int_range 2 5) (self (n / coefficient))) )
             ; 1, return (Pat_construct ("[]", None))
             ; ( 1
               , let rec gen_list n =
                   if n = 0
                   then return (Pat_construct ("[]", None))
                   else (
                     let element = self 0 in
                     let tail = gen_list (n - 1) in
                     map2
                       (fun e t -> Pat_construct ("::", Some (Pat_tuple [ e; t ])))
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
                   (fun rec_fl value_binding_list exp ->
                     Exp_let (rec_fl, value_binding_list, exp))
                   (frequency [ 1, return Nonrecursive; 1, return Recursive ])
                   (list_size
                      (int_range 1 3)
                      (map2
                         (fun pat exp -> { pat; exp })
                         gen_pattern
                         (self (Random.int 3))))
                   (self (n / coefficient)) )
             ; ( 1
               , map2
                   (fun pat exp -> Exp_fun (pat, exp))
                   (list_size (int_range 1 3) gen_pattern)
                   (self (n / coefficient)) )
             ; ( 1
               , map2
                   (fun exp exp_list -> Exp_apply (exp, exp_list))
                   (self 0)
                   (list_size (int_range 1 3) (self (n / coefficient))) )
             ; ( 1
               , map2
                   (fun exp case_list -> Exp_match (exp, case_list))
                   (self (n / coefficient))
                   (list_size
                      (int_range 1 3)
                      (map2
                         (fun left right -> { left; right })
                         gen_pattern
                         (self (Random.int 3)))) )
             ; ( 1
               , map
                   (fun exp_list -> Exp_tuple exp_list)
                   (list_size (int_range 2 5) (self 0)) )
             ; 1, return (Exp_construct ("[]", None))
             ; ( 1
               , let rec gen_list n =
                   if n = 0
                   then return (Exp_construct ("[]", None))
                   else (
                     let element = self 0 in
                     let tail = gen_list (n - 1) in
                     map2
                       (fun e t -> Exp_construct ("::", Some (Exp_tuple [ e; t ])))
                       element
                       tail)
                 in
                 gen_list (Random.int 5) )
             ; ( 1
               , map3
                   (fun i t e -> Exp_ifthenelse (i, t, e))
                   (self (n / coefficient))
                   (self (n / coefficient))
                   (option (self (n / coefficient))) )
             ; ( 1
               , map2
                   (fun exp1 exp2 -> Exp_sequence (exp1, exp2))
                   (self (n / coefficient))
                   (self (n / coefficient)) )
             ])
  ;;

  let gen_value_binding = map2 (fun pat exp -> { pat; exp }) gen_pattern gen_expression

  let gen_structure_item =
    frequency
      [ 1, map (fun exp -> Struct_eval exp) gen_expression
      ; ( 1
        , map2
            (fun rec_flag value_binding_list ->
              Struct_value (rec_flag, value_binding_list))
            (frequency [ 1, return Nonrecursive; 1, return Recursive ])
            (list_size (int_range 1 3) gen_value_binding) )
      ]
  ;;

  let gen_structure = list_size (int_range 1 1) gen_structure_item

  let arbitrary_lam_manual =
    QCheck.make gen_structure ~print:(Format.asprintf "%a" pp_structure)
  ;;

  let run_manual () =
    QCheck.Test.check_exn
      QCheck.(
        Test.make arbitrary_lam_manual (fun str ->
          Format.printf "%a \n" Pprinter.pp_structure str;
          Result.ok str = Parser.parse (Format.asprintf "%a" Pprinter.pp_structure str)))
  ;;
end
