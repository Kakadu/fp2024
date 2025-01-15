(** Copyright 2024, Rodion Suvorov, Mikhail Gavrilenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck.Iter
open QCheck.Shrink
open Ocamladt_lib.Ast

module ShrinkQCheck = struct
  (* Shrinker for Patterns *)

  let filter predicate iter =
    iter >>= fun x -> if predicate x then QCheck.Iter.return x else QCheck.Iter.empty
  ;;

  let shrink_list1 ~shrink_head ~shrink_tail (head, tail) =
    Printf.printf "shrink list1\n";
    match tail with
    | [] -> shrink_head head >|= fun head' -> head', tail
    | _ ->
      let open QCheck.Iter in
      shrink_head head
      >|= (fun head' -> head', tail)
      <+> (QCheck.Shrink.list ~shrink:shrink_tail tail >|= fun tail' -> head, tail')
      |> filter (fun (_, t) ->
        match t with
        | [] -> false
        | _ -> true)
  ;;

  let shrink_list2 ~shrink_first ~shrink_second ~shrink_tail (first, second, tail) =
    Printf.printf "shrink list2\n";
    match tail with
    | [] ->
      let open QCheck.Iter in
      shrink_first first
      >|= (fun first' -> first', second, tail)
      <+> (shrink_second second >|= fun second' -> first, second', tail)
    | _ ->
      let open QCheck.Iter in
      shrink_first first
      >|= (fun first' -> first', second, tail)
      <+> (shrink_second second >|= fun second' -> first, second', tail)
      <+> (QCheck.Shrink.list ~shrink:shrink_tail tail
           >|= fun tail' -> first, second, tail')
      |> filter (fun (_, _, t) ->
        match t with
        | [] -> false
        | _ -> true)
  ;;

  let rec shrink_pattern =
    Printf.printf "shrink pattern\n";
    function
    | Pattern.Pat_any -> QCheck.Iter.return Pattern.Pat_any
    | Pattern.Pat_var id -> string ~shrink:char id >|= fun id' -> Pattern.Pat_var id'
    | Pattern.Pat_constant const ->
      (match const with
       | Constant.Const_integer i ->
         int i >|= fun i' -> Pattern.Pat_constant (Constant.Const_integer i')
       | Constant.Const_char ch ->
         char ch >|= fun ch' -> Pattern.Pat_constant (Constant.Const_char ch')
       | Constant.Const_string str ->
         string ~shrink:char str
         >|= fun str' -> Pattern.Pat_constant (Constant.Const_string str'))
    | Pattern.Pat_tuple pats ->
      shrink_list2
        ~shrink_first:shrink_pattern
        ~shrink_second:shrink_pattern
        ~shrink_tail:shrink_pattern
        pats
      >|= fun pats' -> Pattern.Pat_tuple pats'
    | Pattern.Pat_construct (x, None) ->
      QCheck.Iter.return (Pattern.Pat_construct (x, None))
    | Pattern.Pat_construct (id, Some pat) ->
      shrink_pattern pat >|= fun pat' -> Pattern.Pat_construct (id, Some pat')
    | Pattern.Pat_constraint (pat, core_type) ->
      shrink_pattern pat >|= fun pat' -> Pattern.Pat_constraint (pat', core_type)
  (* <+> shrink_type_expr core_type
     >|= fun core_type' -> Pattern.Pat_constraint (pat, core_type') *)

  and shrink_expression =
    Printf.printf "shrink expression\n";
    function
    | Expression.Exp_ident id ->
      string ~shrink:char id >|= fun id' -> Expression.Exp_ident id'
    | Expression.Exp_constant const ->
      (match const with
       | Constant.Const_integer i ->
         int i >|= fun i' -> Expression.Exp_constant (Constant.Const_integer i')
       | Constant.Const_char ch ->
         char ch >|= fun ch' -> Expression.Exp_constant (Constant.Const_char ch')
       | Constant.Const_string str ->
         string ~shrink:char str
         >|= fun str' -> Expression.Exp_constant (Constant.Const_string str'))
    | Expression.Exp_tuple pats ->
      shrink_list2
        ~shrink_first:shrink_expression
        ~shrink_second:shrink_expression
        ~shrink_tail:shrink_expression
        pats
      >|= fun pats' -> Expression.Exp_tuple pats'
    | Expression.Exp_function cases ->
      shrink_list1 ~shrink_head:shrink_case ~shrink_tail:shrink_case cases
      >|= fun cases' -> Expression.Exp_function cases'
    | Expression.Exp_fun (patterns, exp) ->
      shrink_list1 ~shrink_head:shrink_pattern ~shrink_tail:shrink_pattern patterns
      >|= (fun patterns' -> Expression.Exp_fun (patterns', exp))
      <+> shrink_expression exp
      >|= fun exp' -> Expression.Exp_fun (patterns, exp')
    | Expression.Exp_apply (exp1, exp2) ->
      shrink_expression exp1
      >|= (fun exp1' -> Expression.Exp_apply (exp1', exp2))
      <+> shrink_expression exp2
      >|= fun exp2' -> Expression.Exp_apply (exp1, exp2')
    | Expression.Exp_match (exp, cases) ->
      shrink_expression exp
      >>= fun exp' ->
      shrink_list1 ~shrink_head:shrink_case ~shrink_tail:shrink_case cases
      >>= fun cases' -> return (Expression.Exp_match (exp', cases'))
    | Expression.Exp_let (rec_flag, bindings, exp) ->
      shrink_list1
        ~shrink_head:shrink_value_binding
        ~shrink_tail:shrink_value_binding
        bindings
      >|= (fun bindings' -> Expression.Exp_let (rec_flag, bindings', exp))
      <+> shrink_expression exp
      >|= fun exp' -> Expression.Exp_let (rec_flag, bindings, exp')
    | Expression.Exp_construct (_, None) -> empty
    | Expression.Exp_construct (id, Some exp) ->
      shrink_expression exp >|= fun exp' -> Expression.Exp_construct (id, Some exp')
    | Expression.Exp_constraint (exp, core_type) ->
      shrink_expression exp >|= fun exp' -> Expression.Exp_constraint (exp', core_type)
    | Expression.Exp_if (cond, then_exp, None) ->
      shrink_expression cond
      >|= (fun cond' -> Expression.Exp_if (cond', then_exp, None))
      <+> shrink_expression then_exp
      >|= fun then_exp' -> Expression.Exp_if (cond, then_exp', None)
    | Expression.Exp_if (cond, then_exp, Some else_exp) ->
      shrink_expression cond
      >|= (fun cond' -> Expression.Exp_if (cond', then_exp, Some else_exp))
      <+> shrink_expression then_exp
      >|= (fun then_exp' -> Expression.Exp_if (cond, then_exp', Some else_exp))
      <+> shrink_expression else_exp
      >|= fun else_exp' -> Expression.Exp_if (cond, then_exp, Some else_exp')

  and shrink_value_binding value_binding =
    Printf.printf "shrink value_binding\n";
    let open Expression in
    shrink_pattern value_binding.Expression.pat
    >>= fun pat' ->
    shrink_expression value_binding.Expression.expr
    >>= fun expr' -> return { pat = pat'; expr = expr' }

  and shrink_case case =
    Printf.printf "shrink case\n";
    let open Expression in
    shrink_pattern case.Expression.first
    >>= fun first' ->
    shrink_expression case.Expression.second
    >>= fun second' -> return { first = first'; second = second' }
  ;;

  let shrink_structure_item =
    Printf.printf "shrink str item\n";
    function
    | Structure.Str_eval expr ->
      shrink_expression expr >|= fun expr' -> Structure.Str_eval expr'
    | Structure.Str_value (rec_flag, bindings) ->
      shrink_list1
        ~shrink_head:shrink_value_binding
        ~shrink_tail:shrink_value_binding
        bindings
      >|= fun (head', tail') -> Structure.Str_value (rec_flag, (head', tail'))
  ;;

  let shrink_structure = list ~shrink:shrink_structure_item
end
