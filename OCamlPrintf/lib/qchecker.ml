(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter

module ShrinkQCheck = struct
  open QCheck.Iter
  open QCheck.Shrink

  let rec shrink_pattern = function
    | Pat_any -> empty
    | Pat_var id -> string ~shrink:char id >|= fun id' -> Pat_var id'
    | Pat_constant const ->
      (match const with
       | Const_integer i -> int i >|= fun i' -> Pat_constant (Const_integer i')
       | Const_char ch -> char ch >|= fun ch' -> Pat_constant (Const_char ch')
       | Const_string str ->
         string ~shrink:char str >|= fun str' -> Pat_constant (Const_string str'))
    | Pat_tuple (first_pat, second_pat, pat_list) ->
      shrink_pattern first_pat
      >|= (fun first_pat' -> Pat_tuple (first_pat', second_pat, pat_list))
      <+> shrink_pattern second_pat
      >|= (fun second_pat' -> Pat_tuple (first_pat, second_pat', pat_list))
      <+> (list ~shrink:shrink_pattern pat_list
           >|= fun pat_list' -> Pat_tuple (first_pat, second_pat, pat_list'))
    | Pat_construct (_, None) -> return (Pat_construct ("::", None))
    | Pat_construct (id, Some pat) ->
      shrink_pattern pat >|= fun pat' -> Pat_construct (id, Some pat')
    | Pat_constraint (pat, core_type) ->
      shrink_pattern pat >|= fun pat' -> Pat_constraint (pat', core_type)
  ;;

  let rec shrink_expression = function
    | Exp_ident id -> string ~shrink:char id >|= fun id' -> Exp_ident id'
    | Exp_constant const ->
      (match const with
       | Const_integer i -> int i >|= fun i' -> Exp_constant (Const_integer i')
       | Const_char ch -> char ch >|= fun ch' -> Exp_constant (Const_char ch')
       | Const_string str ->
         string ~shrink:char str >|= fun str' -> Exp_constant (Const_string str'))
    | Exp_let (rec_flag, first_value_binding, value_bindings, exp) ->
      shrink_value_binding first_value_binding
      >|= (fun first_value_binding' ->
            Exp_let (rec_flag, first_value_binding', value_bindings, exp))
      <+> (list ~shrink:shrink_value_binding value_bindings
           >|= fun value_bindings' ->
           Exp_let (rec_flag, first_value_binding, value_bindings', exp))
      <+> (shrink_expression exp
           >|= fun exp' -> Exp_let (rec_flag, first_value_binding, value_bindings, exp'))
    | Exp_fun (pat_list, exp) ->
      list ~shrink:shrink_pattern pat_list
      >|= (fun pat_list' -> Exp_fun (pat_list', exp))
      <+> (shrink_expression exp >|= fun exp' -> Exp_fun (pat_list, exp'))
    | Exp_apply (exp, first_exp, exp_list) ->
      shrink_expression exp
      >|= (fun exp' -> Exp_apply (exp', first_exp, exp_list))
      <+> (shrink_expression first_exp
           >|= fun first_exp' -> Exp_apply (exp, first_exp', exp_list))
      <+> (list ~shrink:shrink_expression exp_list
           >|= fun exp_list' -> Exp_apply (exp, first_exp, exp_list'))
    | Exp_match (exp, first_case, case_list) ->
      shrink_expression exp
      >|= (fun exp' -> Exp_match (exp', first_case, case_list))
      <+> (shrink_case first_case
           >|= fun first_case' -> Exp_match (exp, first_case', case_list))
      <+> (list ~shrink:shrink_case case_list
           >|= fun case_list' -> Exp_match (exp, first_case, case_list'))
    | Exp_tuple (first_exp, second_exp, exp_list) ->
      shrink_expression first_exp
      >|= (fun first_exp' -> Exp_tuple (first_exp', second_exp, exp_list))
      <+> shrink_expression second_exp
      >|= (fun second_exp' -> Exp_tuple (first_exp, second_exp', exp_list))
      <+> (list ~shrink:shrink_expression exp_list
           >|= fun exp_list' -> Exp_tuple (first_exp, second_exp, exp_list'))
    | Exp_construct (_, None) -> return (Exp_construct ("::", None))
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
    | Exp_constraint (exp, core_type) ->
      shrink_expression exp >|= fun exp' -> Exp_constraint (exp', core_type)

  and shrink_value_binding value_binding =
    shrink_pattern value_binding.pat
    >|= (fun pat' -> { value_binding with pat = pat' })
    <+> (shrink_expression value_binding.exp
         >|= fun exp' -> { value_binding with exp = exp' })

  and shrink_case case =
    shrink_pattern case.left
    >|= (fun left' -> { case with left = left' })
    <+> (shrink_expression case.right >|= fun right' -> { case with right = right' })
  ;;

  let shrink_structure_item = function
    | Struct_eval exp -> shrink_expression exp >|= fun exp' -> Struct_eval exp'
    | Struct_value (rec_flag, first_value_binding, value_binding_list) ->
      shrink_value_binding first_value_binding
      >|= (fun first_value_binding' ->
            Struct_value (rec_flag, first_value_binding', value_binding_list))
      <+> (list ~shrink:shrink_value_binding value_binding_list
           >|= fun value_binding_list' ->
           Struct_value (rec_flag, first_value_binding, value_binding_list'))
  ;;

  let shrink_structure = list ~shrink:shrink_structure_item
end

(* Only for debug *)
let coefficient = 50

module TestQCheck = struct
  open QCheck.Gen

  (* Manual generator *)

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
                   (self (n / coefficient))
                   (self (n / coefficient))
                   (list_size (int_range 0 5) (self (n / coefficient))) )
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
                 <*> self (n / coefficient) )
             ; ( 1
               , map2
                   (fun pat exp -> Exp_fun (pat, exp))
                   (list_size (int_range 1 3) gen_pattern)
                   (self (n / coefficient)) )
             ; ( 1
               , map3
                   (fun exp first_exp exp_list -> Exp_apply (exp, first_exp, exp_list))
                   (self 0)
                   (self (n / coefficient))
                   (list_size (int_range 1 3) (self (n / coefficient))) )
             ; ( 1
               , map3
                   (fun op exp1 exp2 -> Exp_apply (op, exp1, exp2))
                   gen_bin_op
                   (self (n / coefficient))
                   (list_size (int_range 1 1) (self (n / coefficient))) )
             ; ( 1
               , map3
                   (fun exp first_case case_list ->
                     Exp_match (exp, first_case, case_list))
                   (self (n / coefficient))
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
                   (self (n / coefficient))
                   (self (n / coefficient))
                   (list_size (int_range 0 5) (self (n / coefficient))) )
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
      ~shrink:ShrinkQCheck.shrink_structure
  ;;

  let arbitrary_lam_auto =
    QCheck.make
      gen_structure
      ~print:(Format.asprintf "%a" pp_structure)
      ~shrink:ShrinkQCheck.shrink_structure
  ;;

  let run_manual () =
    QCheck_base_runner.run_tests
      [ QCheck.(
          Test.make arbitrary_lam_manual (fun str ->
            Format.printf "%a \n" Pprinter.pp_structure str;
            Result.ok str = Parser.parse (Format.asprintf "%a" Pprinter.pp_structure str)))
      ]
  ;;

  let run_auto () =
    QCheck_base_runner.run_tests
      [ QCheck.(
          Test.make arbitrary_lam_auto (fun str ->
            match Parser.parse (Format.asprintf "%a" Pprinter.pp_structure str) with
            | Result.Ok after when after = str -> true
            | Result.Ok _after -> false
            | Result.Error _ -> false))
      ]
  ;;
end
