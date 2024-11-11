(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter

module ShrinkQCheck = struct
  open QCheck.Iter
  open QCheck.Shrink

  let rec shrink_pattern = function
    | Pat_any -> empty
    | Pat_var id -> string id >|= fun id' -> Pat_var id'
    | Pat_constant const ->
      (match const with
       | Const_integer i -> int i >|= fun i' -> Pat_constant (Const_integer i')
       | Const_char ch -> char ch >|= fun ch' -> Pat_constant (Const_char ch')
       | Const_string str -> string str >|= fun str' -> Pat_constant (Const_string str'))
    | Pat_tuple pat_list ->
      list ~shrink:shrink_pattern pat_list >|= fun pat_list' -> Pat_tuple pat_list'
    | Pat_construct (_, None) -> empty
    | Pat_construct (id, Some pat) ->
      shrink_pattern pat >|= fun pat' -> Pat_construct (id, Some pat')
  ;;

  let rec shrink_expression = function
    | Exp_ident id -> string id >|= fun id' -> Exp_ident id'
    | Exp_constant const ->
      (match const with
       | Const_integer i -> int i >|= fun i' -> Exp_constant (Const_integer i')
       | Const_char ch -> char ch >|= fun ch' -> Exp_constant (Const_char ch')
       | Const_string str -> string str >|= fun str' -> Exp_constant (Const_string str'))
    | Exp_let (rec_flag, value_binding_list, exp) -> empty
    | Exp_fun (pat_list, exp) ->
      list ~shrink:shrink_pattern pat_list
      >|= (fun pat_list' -> Exp_fun (pat_list', exp))
      <+> (shrink_expression exp >|= fun exp' -> Exp_fun (pat_list, exp'))
    | Exp_apply (exp, exp_list) ->
      list ~shrink:shrink_expression exp_list
      >|= (fun exp_list' -> Exp_apply (exp, exp_list'))
      <+> (shrink_expression exp >|= fun exp' -> Exp_apply (exp', exp_list))
    | Exp_match (exp, case_list) -> empty
    | Exp_tuple exp_list ->
      list ~shrink:shrink_expression exp_list >|= fun exp_list' -> Exp_tuple exp_list'
    | Exp_construct (_, None) -> empty
    | Exp_construct (id, Some exp) ->
      shrink_expression exp >|= fun exp' -> Exp_construct (id, Some exp')
    | Exp_ifthenelse (if_exp, then_exp, None) ->
      of_list [ if_exp; then_exp ]
      <+> (shrink_expression if_exp
           >|= fun if_exp' -> Exp_ifthenelse (if_exp', then_exp, None))
      <+> (shrink_expression then_exp
           >|= fun then_exp' -> Exp_ifthenelse (if_exp, then_exp', None))
    | Exp_ifthenelse (if_exp, then_exp, Some else_exp) ->
      of_list [ if_exp; then_exp; else_exp ]
      <+> (shrink_expression if_exp
           >|= fun if_exp' -> Exp_ifthenelse (if_exp', then_exp, Some else_exp))
      <+> (shrink_expression then_exp
           >|= fun then_exp' -> Exp_ifthenelse (if_exp, then_exp', Some else_exp))
      <+> (shrink_expression else_exp
           >|= fun else_exp' -> Exp_ifthenelse (if_exp, then_exp, Some else_exp'))
    | Exp_sequence (exp1, exp2) ->
      of_list [ exp1; exp2 ]
      <+> (shrink_expression exp1 >|= fun exp1' -> Exp_sequence (exp1', exp2))
      <+> (shrink_expression exp2 >|= fun exp2' -> Exp_sequence (exp1, exp2'))
  ;;

  let shrink_value_binding =
    map2 (fun pat exp -> { pat; exp }) shrink_pattern shrink_expression
  ;;

  let shrink_structure_item = function
    | Struct_eval exp -> shrink_expression exp >|= fun exp' -> Struct_eval exp'
    | Struct_value (rec_flag, value_binding_list) ->
      list ~shrink:shrink_value_binding value_binding_list
      >|= fun value_binding_list' -> Struct_value (rec_flag, value_binding_list')
  ;;

  let shrink_structure items = list ~shrink:shrink_structure_item items
end

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
    QCheck.make
      gen_structure
      ~print:(Format.asprintf "%a" pp_structure)
      ~shrink:ShrinkQCheck.shrink_structure
  ;;

  let run_manual () =
    QCheck_runner.run_tests
      [ QCheck.(
          Test.make arbitrary_lam_manual (fun str ->
            Format.printf "%a \n" Pprinter.pp_structure str;
            Result.ok str = Parser.parse (Format.asprintf "%a" Pprinter.pp_structure str)))
      ]
  ;;
end
