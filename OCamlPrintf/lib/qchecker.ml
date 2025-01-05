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

  let min_range = int_range 0 10
  let gen_int = nat
  let gen_string gen = string_size min_range ~gen
  let gen_list gen = list_size (int_range 0 5) gen

  (** [gen_list] without zero size *)
  let gen_list_nat gen = list_size (int_range 1 3) gen

  let gen_bin_opr =
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

  let gen_constant =
    oneof
      [ map (fun i -> Const_integer i) gen_int
      ; map (fun c -> Const_char c) gen_char
      ; map (fun s -> Const_string s) (gen_string gen_char)
      ]
  ;;

  let gen_core_type =
    sized
    @@ fix (fun self ->
         function
         | 0 ->
           oneofl [ Type_any; Type_unit; Type_char; Type_int; Type_string; Type_bool ]
         | n ->
           oneof
             [ return Type_any
             ; return Type_unit
             ; return Type_char
             ; return Type_int
             ; return Type_string
             ; return Type_bool
             ; map (fun t -> Type_option t) (self (n / coef))
             ; map
                 (fun id -> Type_name id)
                 (map
                    (fun id ->
                      if String.length id > 1
                      then (
                        match String.get id 1 with
                        | '\'' -> "_" ^ id
                        | _ -> id)
                      else id)
                    gen_ident)
             ; map (fun t -> Type_list t) (self (n / coef))
             ; map3
                 (fun t1 t2 t3 -> Type_tuple (t1, t2, t3))
                 (self (n / coef))
                 (self (n / coef))
                 (gen_list (self (n / coef)))
             ; map2 (fun t1 t2 -> Type_arrow (t1, t2)) (self (n / coef)) (self (n / coef))
             ])
  ;;

  let gen_pattern =
    sized
    @@ fix (fun self ->
         function
         | 0 ->
           oneof
             [ return Pat_any
             ; map (fun i -> Pat_var i) gen_ident
             ; map (fun c -> Pat_constant c) gen_constant
             ]
         | n ->
           oneof
             [ return Pat_any
             ; map (fun i -> Pat_var i) gen_ident
             ; map (fun c -> Pat_constant c) gen_constant
             ; map3
                 (fun f s l -> Pat_tuple (f, s, l))
                 (self (n / coef))
                 (self (n / coef))
                 (gen_list (self (n / coef)))
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

  let rec fix_exp_fun = function
    | Exp_fun (_, _, exp) -> fix_exp_fun exp
    | Exp_function ({ left = _; right = exp }, _) -> fix_exp_fun exp
    | exp -> exp
  ;;

  let gen_vb_without_flag gen =
    oneof
      [ map2 (fun id exp -> { pat = Pat_var id; exp }) gen_ident gen
      ; map2 (fun pat exp -> { pat; exp = fix_exp_fun exp }) gen_pattern gen
      ]
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
             [ map (fun i -> Exp_ident i) gen_ident
             ; map (fun c -> Exp_constant c) gen_constant
             ; map3
                 (fun rec_fl first_value_binding value_binding_list exp ->
                   Exp_let (rec_fl, first_value_binding, value_binding_list, exp))
                 (oneof [ return Nonrecursive; return Recursive ])
                 (gen_vb_without_flag (self (n / coef)))
                 (gen_list (gen_vb_without_flag (self (n / coef))))
               <*> self (n / coef)
             ; map3
                 (fun first_pat pat_list exp -> Exp_fun (first_pat, pat_list, exp))
                 gen_pattern
                 (gen_list_nat gen_pattern)
                 (self (n / coef))
             ; map2 (fun exp_fn exp -> Exp_apply (exp_fn, exp)) (self 0) (self (n / coef))
             ; map (fun exp -> Exp_apply (Exp_ident "~-", exp)) (self (n / coef))
             ; map3
                 (fun opr opn1 opn2 -> Exp_apply (opr, Exp_apply (opn1, opn2)))
                 gen_bin_opr
                 (self (n / coef))
                 (self (n / coef))
             ; map2
                 (fun first_case case_list -> Exp_function (first_case, case_list))
                 (map2 (fun left right -> { left; right }) gen_pattern (self (n / coef)))
                 (gen_list
                    (map2
                       (fun left right -> { left; right })
                       gen_pattern
                       (self (n / coef))))
             ; map3
                 (fun exp first_case case_list -> Exp_match (exp, first_case, case_list))
                 (self (n / coef))
                 (map2 (fun left right -> { left; right }) gen_pattern (self (n / coef)))
                 (gen_list
                    (map2
                       (fun left right -> { left; right })
                       gen_pattern
                       (self (n / coef))))
             ; map3
                 (fun first second list -> Exp_tuple (first, second, list))
                 (self (n / coef))
                 (self (n / coef))
                 (gen_list (self (n / coef)))
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

  let gen_structure_item =
    oneof
      [ map (fun exp -> Struct_eval exp) gen_expression
      ; map3
          (fun rec_flag first_value_binding value_binding_list ->
            Struct_value (rec_flag, first_value_binding, value_binding_list))
          (oneof [ return Nonrecursive; return Recursive ])
          (gen_vb_without_flag gen_expression)
          (gen_list_nat (gen_vb_without_flag gen_expression))
      ]
  ;;

  let gen_structure = gen_list_nat gen_structure_item
end

let failure ast =
  Format.asprintf
    {|

*** PPrinter ***
%a

***   AST    ***
%s

***  Parser  ***
%s

  |}
    Pprinter.pp_structure
    ast
    (show_structure ast)
    (match Parser.parse (Format.asprintf "%a" Pprinter.pp_structure ast) with
     | Ok ast_parsed -> show_structure ast_parsed
     | Error error -> error)
;;

let rule_gen ?(show_passed = false) ?(show_shrinker = false) ast =
  match Parser.parse (Format.asprintf "%a" Pprinter.pp_structure ast) with
  | Ok ast_parsed ->
    if ast = ast_parsed
    then (
      if show_passed
      then Format.printf "\n*** PPrinter ***\n%a\n" Pprinter.pp_structure ast;
      true)
    else (
      if show_shrinker
      then Format.printf "\n*** Shrinker ***\n%a\n" Pprinter.pp_structure ast;
      false)
  | Error _ ->
    if show_shrinker
    then Format.printf "\n*** Shrinker ***\n%a\n" Pprinter.pp_structure ast;
    false
;;

let run_gen ?(show_passed = false) ?(show_shrinker = false) ?(count = 10) =
  let gen type_gen =
    QCheck.make type_gen ~print:failure ~shrink:Shrinker.shrink_structure
  in
  QCheck_base_runner.run_tests_main
    [ QCheck.Test.make
        ~count
        ~name:"the manual generator"
        (gen TestQCheckManual.gen_structure)
        (fun ast -> rule_gen ~show_passed ~show_shrinker ast)
    ; QCheck.Test.make
        ~count
        ~name:"the auto generator"
        (gen Ast.gen_structure)
        (fun ast -> rule_gen ~show_passed ~show_shrinker ast)
    ]
;;
