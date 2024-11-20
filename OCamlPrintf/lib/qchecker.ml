(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ast.Expression

module TestQCheckManual = struct
  open QCheck.Gen

  let coef = 50 (* For the generator's speed. *)

  let gen_char =
    (* Exception quotation marks and backslash. *)
    oneof [ return '!'; char_range '#' '&'; char_range '(' '['; char_range ']' '~' ]
  ;;

  let gen_int = nat

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

  let gen_ident =
    map2
      (fun start_ident rest_ident ->
        match Base.Char.to_string start_ident ^ rest_ident with
        | "_" -> "id"
        | id -> id)
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

  let gen_constant =
    oneof
      [ map (fun i -> Const_integer i) gen_int
      ; map (fun c -> Const_char c) gen_char
      ; map (fun s -> Const_string s) gen_ident
      ]
  ;;

  let gen_core_type =
    sized
    @@ fix (fun self ->
         function
         | 0 -> oneofl [ Type_any; Type_char; Type_int; Type_string; Type_bool ]
         | n ->
           oneof
             [ return Type_any
             ; return Type_char
             ; return Type_int
             ; return Type_string
             ; return Type_bool
             ; map (fun t -> Type_list t) (self (n / coef))
             ; map3
                 (fun t1 t2 t3 -> Type_tuple (t1, t2, t3))
                 (self (n / coef))
                 (self (n / coef))
                 (list_size (int_range 0 5) (self (n / coef)))
             ; map2 (fun t1 t2 -> Type_arrow (t1, t2)) (self (n / coef)) (self (n / coef))
             ])
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
           oneof
             [ map3
                 (fun f s l -> Pat_tuple (f, s, l))
                 (self (n / coef))
                 (self (n / coef))
                 (list_size (int_range 0 5) (self (n / coef)))
             ; return (Pat_construct ("[]", None))
             ; (let rec gen_list n =
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
                gen_list (Random.int 5))
             ; return (Pat_construct ("true", None))
             ; return (Pat_construct ("false", None))
             ; map (fun i -> Pat_construct ("Some", Some i)) (self (n / coef))
             ; return (Pat_construct ("[]", None))
             ; map2 (fun pat t -> Pat_constraint (pat, t)) (self (n / coef)) gen_core_type
             ])
  ;;

  let gen_expression =
    sized
    @@ fix (fun self ->
         function
         | 0 ->
           oneof
             [ map (fun i -> Exp_ident i) gen_ident
             ; map (fun c -> Exp_constant c) gen_constant
             ]
         | n ->
           oneof
             [ map3
                 (fun rec_fl first_value_binding value_binding_list exp ->
                   Exp_let (rec_fl, first_value_binding, value_binding_list, exp))
                 (frequency [ 1, return Nonrecursive; 1, return Recursive ])
                 (map2 (fun pat exp -> { pat; exp }) gen_pattern (self (Random.int 3)))
                 (list_size
                    (int_range 1 3)
                    (map2 (fun pat exp -> { pat; exp }) gen_pattern (self (Random.int 3))))
               <*> self (n / coef)
             ; map2
                 (fun pat exp -> Exp_fun (pat, exp))
                 (list_size (int_range 1 3) gen_pattern)
                 (self (n / coef))
             ; map3
                 (fun exp first_exp exp_list -> Exp_apply (exp, first_exp, exp_list))
                 (self 0)
                 (self (n / coef))
                 (list_size (int_range 1 3) (self (n / coef)))
             ; map3
                 (fun op exp1 exp2 -> Exp_apply (op, exp1, exp2))
                 gen_bin_op
                 (self (n / coef))
                 (list_size (int_range 1 1) (self (n / coef)))
             ; map3
                 (fun exp first_case case_list -> Exp_match (exp, first_case, case_list))
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
                       (self (Random.int 3))))
             ; map3
                 (fun first second list -> Exp_tuple (first, second, list))
                 (self (n / coef))
                 (self (n / coef))
                 (list_size (int_range 0 5) (self (n / coef)))
             ; return (Exp_construct ("[]", None))
             ; (let rec gen_list n =
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
                gen_list (Random.int 5))
             ; return (Exp_construct ("true", None))
             ; return (Exp_construct ("false", None))
             ; map (fun i -> Exp_construct ("Some", Some i)) (self (n / coef))
             ; return (Exp_construct ("None", None))
             ; map3
                 (fun i t e -> Exp_ifthenelse (i, t, e))
                 (self (n / coef))
                 (self (n / coef))
                 (option (self (n / coef)))
             ; map2
                 (fun exp1 exp2 -> Exp_sequence (exp1, exp2))
                 (self (n / coef))
                 (self (n / coef))
             ; map2 (fun exp t -> Exp_constraint (exp, t)) (self (n / coef)) gen_core_type
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
end

let run_gen type_gen =
  let gen =
    QCheck.make
      type_gen
      ~print:(Format.asprintf "%a" Pprinter.pp_structure)
      ~shrink:Shrinker.shrink_structure
  in
  QCheck_base_runner.run_tests
    [ QCheck.Test.make ~name:"the auto generator" ~count:1 gen (fun ast ->
        match Parser.parse (Format.asprintf "%a" Pprinter.pp_structure ast) with
        | Ok ast_parsed ->
          if ast = ast_parsed
          then (
            Format.printf " *** PPrinter *** \n%a\n\n" Pprinter.pp_structure ast;
            true)
          else (
            Format.printf " ***  Parser  *** \n%s\n" (show_structure ast_parsed);
            Format.printf " ***   AST    *** \n%s\n\n" (show_structure ast);
            false)
        | Error _ ->
          Format.printf " *** PPrinter *** \n%a\n" Pprinter.pp_structure ast;
          Format.printf " ***   AST    *** \n%s\n\n" (show_structure ast);
          false)
    ]
;;
