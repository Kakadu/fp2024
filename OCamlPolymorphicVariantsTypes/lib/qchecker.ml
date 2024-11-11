(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter
open Parser
open Parser_utility

module QChecker = struct
  open QCheck.Gen

  let max_var_len = 16
  let max_tuple_len = 4
  let max_pattern_depth = 2
  let max_petterns_len = 2
  let max_expr_block_len = 4
  let max_expr_depth = 1
  let max_value_binding_len = 2
  let max_program_len = 2
  let gen_integer = int_range 0 Int.max_int
  let gen_boolean = oneof [ return true; return false ]

  let gen_identifier =
    let gen_var_char is_first =
      frequency
        [ 3, map Char.chr (int_range (int_of_char 'a') (int_of_char 'z'))
        ; 1, return '_'
        ; (if is_first then 0 else 1), return '\''
        ; ( (if is_first then 0 else 2)
          , map Char.chr (int_range (int_of_char '0') (int_of_char '9')) )
        ]
    in
    let first_char_gen = gen_var_char true in
    let tail_gen start_length =
      string_size
        (int_range start_length (max_var_len - start_length))
        ~gen:(gen_var_char false)
    in
    let params = map2 (fun first tail -> first, tail) first_char_gen (tail_gen 0) in
    let result =
      params
      >>= fun (first, tail) ->
      let id = Format.sprintf "%c%s" first tail in
      if is_keyword id
      then tail_gen 1 >>= fun tl -> return (Format.sprintf "%s%s" id tl)
      else return id
    in
    result
  ;;

  let gen_literal =
    oneof
      [ map (fun i -> IntLiteral i) gen_integer
      ; map (fun b -> BoolLiteral b) gen_boolean
      ; return UnitLiteral
      ]
  ;;

  let gen_pattern =
    let rec gen_patterns_seq depth = list_size (int_range 2 max_tuple_len) (helper depth)
    and gen_ptuple depth =
      map
        (fun patters ->
          match patters with
          | [] -> PUnit
          | p :: [] -> p
          | _ :: _ -> PTuple patters)
        (gen_patterns_seq (depth - 1))
    and helper depth =
      if depth <= 1
      then oneof [ return PUnit; map (fun i -> PVar i) gen_identifier ]
      else gen_ptuple depth
    in
    helper max_pattern_depth
  ;;

  let gen_patterns = list_size (int_range 1 max_petterns_len) gen_pattern
  let gen_recursive_type = oneof [ return Recursive; return Nonrecursive ]

  let gen_value_binding gen_expression =
    map2 (fun pattern expr -> pattern, expr) gen_pattern gen_expression
  ;;

  let gen_definition gen_expression =
    tup2
      gen_recursive_type
      (list_size (int_range 1 max_value_binding_len) (gen_value_binding gen_expression))
  ;;

  let gen_variable = map (fun i -> Variable i) gen_identifier
  let gen_const = map (fun c -> Const c) gen_literal

  let rec gen_unary inapply depth =
    let gen_unary_operator = oneof [ return Negate; return Positive ] in
    map2
      (fun operator expr -> Unary (operator, expr))
      gen_unary_operator
      (gen_expr inapply (depth - 1))

  and gen_binary inapply depth =
    let gen_binary_operator =
      oneof
        [ return Add
        ; return Subtract
        ; return Multiply
        ; return Division
        ; return Equals
        ; return Unequals
        ; return Gt
        ; return Lt
        ; return Gte
        ; return Lte
        ; return And
        ; return Or
        ]
    in
    map3
      (fun left operator right -> Binary (left, operator, right))
      (gen_expr inapply (depth - 1))
      gen_binary_operator
      (gen_expr inapply (depth - 1))

  and gen_tuple depth =
    map
      (fun exprs -> Tuple exprs)
      (list_size (int_range 2 max_tuple_len) (gen_expr false (depth - 1)))

  and gen_expr_block inapply depth =
    let tail = list_size (int_range 1 max_expr_block_len) (gen_expr false (depth - 1)) in
    map2
      (fun exprs expr ->
        match exprs with
        | [] -> expr
        | _ -> ExpressionBlock (exprs @ [ expr ]))
      tail
      (gen_expr inapply (depth - 1))

  and gen_if_expr inapply depth =
    let gen_else_branch = option (gen_expr inapply (depth - 1)) in
    map3
      (fun ex1 ex2 ex3 -> If (ex1, ex2, ex3))
      (gen_expr false (depth - 1))
      (gen_expr inapply (depth - 1))
      gen_else_branch

  and gen_lambda depth =
    map2
      (fun patterns expr -> Lambda (patterns, expr))
      gen_patterns
      (gen_expr false (depth - 1))

  and gen_applyable_expr depth =
    if depth = 0
    then gen_variable
    else oneof [ gen_expr true (depth - 1); gen_lambda depth ]

  and gen_apply_expr depth =
    map2
      (fun expr exprs -> Apply (expr, exprs))
      (gen_applyable_expr depth)
      (list_size (int_range 1 max_petterns_len) (gen_expr false (depth - 1)))

  and gen_define_expr inapply depth =
    map2
      (fun name expr -> Define (name, expr))
      (gen_definition (gen_expr false max_expr_depth))
      (gen_expr inapply (depth - 1))

  and gen_expr inapply depth =
    if depth = 0
    then if inapply then gen_variable else oneof [ gen_variable; gen_const ]
    else
      oneof
        [ gen_unary inapply depth
        ; gen_binary inapply depth
        ; gen_expr_block inapply depth
        ; gen_if_expr inapply depth (* ; gen_define_expr inapply depth *)
        ; gen_lambda depth
        ; gen_apply_expr depth
        ; gen_tuple depth
        ]
  ;;

  let gen_struct_item =
    oneof
      [ map (fun def -> DefineItem def) (gen_definition (gen_expr false max_expr_depth))
      ; map (fun ex -> EvalItem ex) (gen_expr false max_expr_depth)
      ]
  ;;

  let gen_program = list_size (int_range 1 max_program_len) gen_struct_item

  let arbitrary_lam_manual =
    QCheck.make gen_program ~print:(Format.asprintf "%a" pp_program)
  ;;

  let run_manual () =
    QCheck_runner.run_tests
      [ QCheck.(
          Test.make arbitrary_lam_manual (fun l ->
            (* Format.printf "\nTree:\n%s\n" (show_program l);
               Format.printf "PPrinting:\n%a\n----------------\n" pp_program l; *)
            Format.printf "%a\n" pp_program l;
            let r = parse program_parser (Format.asprintf "%a" pp_program l) in
            match r with
            | ParseSuccess (r, _) -> l = r
            | _ -> false))
      ]
  ;;
end
