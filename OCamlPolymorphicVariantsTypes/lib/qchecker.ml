(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter

module QChecker = struct
  open QCheck.Gen

  let positive_number =
    let p = Random.float 1.0 in
    if p < 0.5
    then int_range 1 100
    else if p < 0.75
    then int_range 100 1000
    else int_range 1000 10000
  ;;

  let gen_integer = oneof [ positive_number; return 0 ]
  let gen_boolean = oneof [ return true; return false ]
  let tuple_size = int_range 2 4
  let definition_size = int_range 1 10
  let patterns_size = int_range 1 6
  let pattern_depth = int_range 1 2
  let expr_depth = int_range 1 6
  let apply_size = int_range 1 6
  let expr_block_size = int_range 2 8
  let program_size = int_range 1 20

  let gen_identifier =
    let helper =
      string_size
        (int_range 1 10)
        ~gen:(oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; return '_' ])
    in
    helper
    >>= fun s ->
    if Parser.is_keyword s
    then helper >>= fun addicted -> return (Format.sprintf "%s%s" s addicted)
    else return s
  ;;

  let gen_pattern =
    let gen_ptuple subpattern_gen =
      map (fun l -> PTuple l) (list_size tuple_size subpattern_gen)
    in
    sized_size pattern_depth
    @@ fix (fun self ->
         function
         | 0 -> oneof [ map (fun i -> PVar i) gen_identifier; return PUnit ]
         | depth -> gen_ptuple (self (depth - 1)))
  ;;

  let gen_recursive_type = oneof [ return Recursive; return Nonrecursive ]
  let gen_value_binding expr_gen = map2 (fun p expr -> p, expr) gen_pattern expr_gen

  let gen_definition expr_gen =
    map2
      (fun r vbl -> r, vbl)
      gen_recursive_type
      (list_size definition_size (gen_value_binding expr_gen))
  ;;

  let gen_literal =
    oneof
      [ map (fun i -> IntLiteral i) gen_integer
      ; map (fun b -> BoolLiteral b) gen_boolean
      ; return UnitLiteral
      ]
  ;;

  let gen_variable = map (fun i -> Variable i) gen_identifier
  let gen_const = map (fun c -> Const c) gen_literal

  let gen_expr =
    let gen_define_expr expr_gen =
      map2 (fun def ex -> Define (def, ex)) (gen_definition expr_gen) expr_gen
    in
    let gen_unary_expr subexpr_gen =
      map2
        (fun op ex -> Unary (op, ex))
        (oneof [ return Positive; return Negate ])
        subexpr_gen
    in
    let gen_binary_expr subexpr_gen =
      map3
        (fun left op right -> Binary (left, op, right))
        subexpr_gen
        (oneof
           [ return Add
           ; return Subtract
           ; return Multiply
           ; return Division
           ; return And
           ; return Or
           ; return Equals
           ; return Unequals
           ; return Gt
           ; return Lt
           ; return Gte
           ; return Lte
           ])
        subexpr_gen
    in
    let gen_tuple_expr subexpr_gen =
      map (fun l -> Tuple l) (list_size tuple_size subexpr_gen)
    in
    let gen_if_expr subexpr_gen block_gen =
      let helper = oneof [ return None; map (fun ex -> Some ex) block_gen ] in
      map3
        (fun ex then_ex else_ex -> If (ex, then_ex, else_ex))
        subexpr_gen
        block_gen
        helper
    in
    let gen_lambda subexpr_gen =
      map2
        (fun pl ex -> Lambda (pl, ex))
        (list_size patterns_size gen_pattern)
        subexpr_gen
    in
    let gen_apply subexpr_gen =
      map2 (fun ex l -> Apply (ex, l)) subexpr_gen (list_size apply_size subexpr_gen)
    in
    let gen_expr_block subexpr_gen =
      map (fun l -> ExpressionBlock l) (list_size expr_block_size subexpr_gen)
    in
    sized_size expr_depth
    @@ fix (fun self ->
         function
         | 0 -> frequency [ 1, gen_const; 1, gen_variable ]
         | depth ->
           frequency
             [ 1, gen_unary_expr (self (depth - 1))
             ; 1, gen_binary_expr (self (depth - 1))
             ; 1, gen_tuple_expr (self (depth - 1))
             ; 1, gen_define_expr (self (depth - 1))
             ; 1, gen_if_expr (self (depth / 2)) (self (depth - 1))
             ; 1, gen_lambda (self (depth - 1))
             ; 1, gen_apply (self (depth - 1))
             ; 1, gen_expr_block (self (depth - 1))
             ])
  ;;

  let gen_struct_item =
    oneof
      [ map (fun def -> DefineItem def) (gen_definition gen_expr)
      ; map (fun ex -> EvalItem ex) gen_expr
      ]
  ;;

  let gen_program = list_size program_size gen_struct_item

  let arbitrary_lam_manual =
    QCheck.make gen_program ~print:(Format.asprintf "%a" pp_program)
  ;;

  let run_manual () =
    let open Parser in
    let open Parser_utility in
    QCheck_runner.run_tests
      [ QCheck.(
          Test.make (* ~count:100 *) arbitrary_lam_manual (fun p ->
            let r = parse program_parser (Format.asprintf "%a" pp_program p) in
            match r with
            | ParseSuccess (r, _) ->
              let res = p = r in
              if not res
              then (
                Format.printf "IntputTree:\n%s\n" (show_program p);
                Format.printf "Result:\n%s\n" (show_program r));
              res
            | _ ->
              Format.printf "Intput:\n%a\n" pp_program p;
              Format.printf "Result:\n%s\n" (string_of_parse_result show_program r);
              false))
      ]
  ;;
end
