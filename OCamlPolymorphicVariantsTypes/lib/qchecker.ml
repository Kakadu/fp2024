(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter

module QChecker = struct
  open QCheck.Gen

  let max_number_value = 64_000_000

  let positive_number =
    match Random.float 1. with
    | p when p < 0.25 -> int_range 0 (max_number_value / 16)
    | p when p < 0.5 -> int_range (max_number_value / 16) (max_number_value / 8)
    | p when p < 0.75 -> int_range (max_number_value / 8) (max_number_value / 4)
    | p when p < 0.875 -> int_range (max_number_value / 4) (max_number_value / 2)
    | _ -> int_range (max_number_value / 2) max_number_value
  ;;

  let gen_integer = oneof [ positive_number; return 0 ]
  let gen_boolean = oneof [ return true; return false ]
  let identifier_size = int_range 1 5
  let addected_tuple_size = int_range 0 2
  let elist_size = int_range 0 2
  let definition_size = int_range 1 2
  let patterns_size = int_range 1 2
  let core_type_size = int_range 1 3
  let pattern_depth = int_range 1 1
  let expr_depth = int_range 1 2
  let apply_size = int_range 1 2
  let expr_block_size = int_range 2 2
  let program_size = int_range 1 1

  let gen_identifier is_constructor_name =
    let helper =
      let first_char =
        if is_constructor_name
        then oneof [ char_range 'A' 'Z' ]
        else oneof [ char_range 'a' 'z'; return '_' ]
      in
      first_char
      >>= fun c ->
      string_size
        identifier_size
        ~gen:
          (oneof
             [ char_range 'a' 'z'
             ; char_range 'A' 'Z'
             ; char_range '0' '9'
             ; (if is_constructor_name
                then return '_'
                else oneof [ return '\''; return '_' ])
             ])
      >>= fun res -> return (Format.sprintf "%c%s" c res)
    in
    helper
    >>= fun s ->
    if Parser.is_keyword s
    then helper >>= fun addicted -> return (Format.sprintf "%s%s" s addicted)
    else return s
  ;;

  let gen_core_type =
    let gen_ttuple subtypes_gen =
      map3
        (fun p1 p2 pl -> TupleType (p1, p2, pl))
        subtypes_gen
        subtypes_gen
        (list_size addected_tuple_size subtypes_gen)
    in
    let type_identifier = map (fun i -> TypeIdentifier i) (gen_identifier false) in
    sized_size core_type_size
    @@ fix (fun self ->
         function
         | 0 -> type_identifier
         | depth ->
           let subtype_gen = self (depth - 1) in
           frequency
             [ ( 1
               , map2
                   (fun it t2 -> TypeConstructor (it, t2))
                   (gen_identifier false)
                   subtype_gen )
             ; 1, gen_ttuple subtype_gen
             ; 1, map2 (fun t1 t2 -> ArrowType (t1, t2)) subtype_gen subtype_gen
             ])
  ;;

  let gen_pattern =
    let gen_ptuple subpattern_gen =
      map3
        (fun p1 p2 pl -> PTuple (p1, p2, pl))
        subpattern_gen
        subpattern_gen
        (list_size addected_tuple_size subpattern_gen)
    in
    sized_size pattern_depth
    @@ fix (fun self ->
         function
         | 0 -> oneof [ map (fun i -> PVar i) (gen_identifier false); return PUnit ]
         | depth ->
           let subpattern_gen = self (depth - 1) in
           frequency
             [ 1, map2 (fun p1 p2 -> PConstrain (p1, p2)) subpattern_gen gen_core_type
             ; 1, gen_ptuple subpattern_gen
             ])
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

  let gen_variable = map (fun i -> Variable i) (gen_identifier false)
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
      map3
        (fun ex1 ex2 l -> Tuple (ex1, ex2, l))
        subexpr_gen
        subexpr_gen
        (list_size addected_tuple_size subexpr_gen)
    in
    let gen_list_expr subexpr_gen =
      map (fun l -> ExpressionsList l) (list_size elist_size subexpr_gen)
    in
    let gen_if_expr subexpr_gen =
      let helper = oneof [ return None; map (fun ex -> Some ex) subexpr_gen ] in
      map3
        (fun ex then_ex else_ex -> If (ex, then_ex, else_ex))
        subexpr_gen
        subexpr_gen
        helper
    in
    let gen_lambda subexpr_gen =
      map2
        (fun pl ex -> Lambda (pl, ex))
        (list_size patterns_size gen_pattern)
        subexpr_gen
    in
    let gen_apply subexpr_gen =
      map3
        (fun ex aex l -> Apply (ex, aex, l))
        subexpr_gen
        subexpr_gen
        (list_size apply_size subexpr_gen)
    in
    let gen_expr_block subexpr_gen =
      map3
        (fun e1 e2 l -> ExpressionBlock (e1, e2, l))
        subexpr_gen
        subexpr_gen
        (list_size expr_block_size subexpr_gen)
    in
    let constructor_expr subexpr_gen =
      map2
        (fun id opt_expr -> Construct (id, opt_expr))
        (gen_identifier true)
        (oneof [ return None; map (fun ex -> Some ex) subexpr_gen ])
    in
    let gen_case subexpr_gen =
      map3
        (fun p f res -> { pattern = p; filter = f; result = res })
        gen_pattern
        (oneof [ return None; map (fun ex -> Some ex) subexpr_gen ])
        subexpr_gen
    in
    let gen_match_with_expr subexpr_gen =
      map3
        (fun ex c cases -> Match (ex, c, cases))
        subexpr_gen
        (gen_case subexpr_gen)
        (list_size definition_size (gen_case subexpr_gen))
    in
    let gen_function_expr subexpr_gen =
      map2
        (fun c cases -> Func (c, cases))
        (gen_case subexpr_gen)
        (list_size definition_size (gen_case subexpr_gen))
    in
    sized_size expr_depth
    @@ fix (fun self ->
         function
         | 0 -> frequency [ 1, gen_const; 1, gen_variable ]
         | depth ->
           let subexpr_gen = self (depth - 1) in
           frequency
             [ 1, gen_unary_expr subexpr_gen
             ; 1, gen_binary_expr subexpr_gen
             ; 1, gen_tuple_expr subexpr_gen
             ; 1, gen_list_expr subexpr_gen
             ; 1, gen_define_expr subexpr_gen
             ; 1, gen_if_expr subexpr_gen
             ; 1, constructor_expr subexpr_gen
             ; 1, gen_lambda subexpr_gen
             ; 1, gen_apply subexpr_gen
             ; 1, gen_expr_block subexpr_gen
             ; 1, gen_match_with_expr subexpr_gen
             ; 1, gen_function_expr subexpr_gen
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
          Test.make arbitrary_lam_manual (fun p ->
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
              Format.printf "IntputTree:\n%s\n" (show_program p);
              Format.printf "Intput:\n%a\n" pp_program p;
              Format.printf "Result:\n%s\n" (string_of_parse_result show_program r);
              false))
      ]
  ;;
end
