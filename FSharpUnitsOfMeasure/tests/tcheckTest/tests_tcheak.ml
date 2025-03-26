(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Typecheck.Inference
open Typecheck.Types
open Ast

let reset_typevar_counter () =
  typevar_counter := 0

  let builtins = [
  ("=", Scheme([0], TFun(TVar 0, TFun(TVar 0, TBool))));
  ("-", Scheme([], TFun(TInt, TFun(TInt, TInt))));
  ("*", Scheme([], TFun(TInt, TFun(TInt, TInt))));
  ("+", Scheme([], TFun(TInt, TFun(TInt, TInt))));
  ("<", Scheme([], TFun(TInt, TFun(TInt, TBool))));
]

let env = builtins @ initial_env

let id_expr = Expr_lam (Pattern_ident_or_op "x", Expr_ident_or_op "x")
let (_sId, tId) = infer_expr env id_expr

let%expect_test "Identity function" =
  reset_typevar_counter();
  Printf.printf "Inferred type for identity: %s\n" (string_of_ty tId);
  [%expect {|
    Inferred type for identity: ('a0 -> 'a0)
  |}];;

let const_expr = Expr_lam (Pattern_ident_or_op "x", Expr_const (Const_int 1))
let (_sConst, tConst) = infer_expr env const_expr

let%expect_test "Constant function" =
  reset_typevar_counter();
  Printf.printf "Inferred type for constant function: %s\n" (string_of_ty tConst);
  [%expect {|
    Inferred type for constant function: ('a0 -> int)
  |}];;

let fact_expr =
  Expr_let (Recursive,
    Bind (Pattern_ident_or_op "fact",
      Expr_lam (Pattern_ident_or_op "n",
        Expr_ifthenelse (
          (* if n < 1 then 1 *)
          Expr_apply (
            Expr_apply (Expr_ident_or_op "<", Expr_ident_or_op "n"),
            Expr_const (Const_int 1)
          ),
          Expr_const (Const_int 1),
          Some (
            (* else n * fact (n - 1) *)
            Expr_apply (
              Expr_apply (Expr_ident_or_op "*", Expr_ident_or_op "n"),
              Expr_apply (
                Expr_ident_or_op "fact",
                Expr_apply (
                  Expr_apply (Expr_ident_or_op "-", Expr_ident_or_op "n"),
                  Expr_const (Const_int 1)
                )
              )
            )
          )
        )
      )
    ),
    [],
    Expr_ident_or_op "fact"
  )
let (_sFact, tFact) = infer_expr env fact_expr

let%expect_test "Factorial function" =
  reset_typevar_counter();
  Printf.printf "Inferred type for factorial: %s\n" (string_of_ty tFact);
  [%expect {|
    Inferred type for factorial: (int -> int)
  |}];;

(* let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib) *)
let fib_expr =
  Expr_let (Recursive,
    Ast.Bind (Pattern_ident_or_op "fib",
      Expr_lam (Pattern_ident_or_op "n",
        Expr_ifthenelse (
          (* if n < 2 then n *)
          Expr_apply (
            Expr_apply (Expr_ident_or_op "<", Expr_ident_or_op "n"),
            Expr_const (Const_int 2)
          ),
          Expr_ident_or_op "n",
          Some (
            (* else fib (n - 1) + fib (n - 2) *)
            Expr_apply (
              Expr_apply (Expr_ident_or_op "+",
                Expr_apply (Expr_ident_or_op "fib",
                  Expr_apply (
                    Expr_apply (Expr_ident_or_op "-", Expr_ident_or_op "n"),
                    Expr_const (Const_int 1)
                  )
                )
              ),
              Expr_apply (Expr_ident_or_op "fib",
                Expr_apply (
                  Expr_apply (Expr_ident_or_op "-", Expr_ident_or_op "n"),
                  Expr_const (Const_int 2)
                )
              )
            )
          )
        )
      )
    ),
    [],
    Expr_ident_or_op "fib"
  )
let (_sFib, tFib) = infer_expr env fib_expr

let%expect_test "Fibonacci function" =
  reset_typevar_counter();
  Printf.printf "Inferred type for fibonacci: %s\n" (string_of_ty tFib);
  [%expect {| Inferred type for fibonacci: (int -> int)
    |}];;

let unit_expr =
  Expr_const (Const_unit_of_measure (Unit_of_measure (Mnum_float 5.0, Measure_ident "cm")))
let (_sUnit, tUnit) = infer_expr env unit_expr

let%expect_test "Unit of measure constant" =
  reset_typevar_counter();
  Printf.printf "Inferred type for unit constant: %s\n" (string_of_ty tUnit);
  [%expect {|
    Inferred type for unit constant: float<cm>
  |}];;

let list_expr =
  Expr_list [Expr_const (Const_int 1); Expr_const (Const_int 2); Expr_const (Const_int 3)]
let (_sList, tList) = infer_expr env list_expr

let%expect_test "List literal" =
  reset_typevar_counter();
  Printf.printf "Inferred type for list: %s\n" (string_of_ty tList);
  [%expect {|
    Inferred type for list: list<int>
  |}];;

(* let x = 1 and y = 2 in x *)
let let_expr =
  Expr_let (Nonrecursive,
    Ast.Bind (Pattern_ident_or_op "x", Expr_const (Const_int 1)),
    [Ast.Bind (Pattern_ident_or_op "y", Expr_const (Const_int 2))],
    Expr_ident_or_op "x"
  )
let (_sLet, tLet) = infer_expr env let_expr

let%expect_test "Non-recursive let binding" =
  reset_typevar_counter();
  Printf.printf "Inferred type for non-recursive let: %s\n" (string_of_ty tLet);
  [%expect {|
    Inferred type for non-recursive let: int
  |}];;

let%expect_test "Typed pattern in lambda" =
reset_typevar_counter ();
let expr =
  Expr_lam(
    Pattern_typed(Pattern_ident_or_op "x", Type_ident "int"),
    Expr_apply(
      Expr_apply(Expr_ident_or_op "+", Expr_ident_or_op "x"),
      Expr_const(Const_int 1)
    )
  )
in
let (_s, t) = infer_expr env expr in
let t' = apply_subst _s t in
Printf.printf "Type for typed-lambda: %s\n" (string_of_ty t');
[%expect {|
  Type for typed-lambda: (int -> int)
|}];;

let%expect_test "Pattern match on list with different lengths" =
    reset_typevar_counter ();
    (* fun xs ->
         match xs with
         | [] -> 0
         | [x] -> x
         | [x; y] -> x + y
    *)
    let expr =
      Expr_lam(
        Pattern_ident_or_op "xs",
        Expr_match(
          Expr_ident_or_op "xs",
          Rule(Pattern_list [], Expr_const (Const_int 0)),
          [
            Rule(Pattern_list [Pattern_ident_or_op "x"], Expr_ident_or_op "x");
            Rule(Pattern_list [Pattern_ident_or_op "x"; Pattern_ident_or_op "y"],
                 Expr_apply(
                   Expr_apply(Expr_ident_or_op "+", Expr_ident_or_op "x"),
                   Expr_ident_or_op "y"
                 )
            )
          ]
        )
      )
    in
    let (_s, t) = infer_expr env expr in
    let t' = apply_subst _s t in
    Printf.printf "Match on list yields type: %s\n" (string_of_ty t');
    [%expect {|
      Match on list yields type: (list<int> -> int)
    |}];;

let%expect_test "Add measure mismatch" =
reset_typevar_counter ();
let expr =
  Expr_let(Nonrecursive,
    Bind(Pattern_ident_or_op "x",
          Expr_const(Const_unit_of_measure (Unit_of_measure(Mnum_int 5, Measure_ident "cm")))),
    [],
    Expr_apply(
      Expr_apply(Expr_ident_or_op "+", Expr_ident_or_op "x"),
      Expr_const(Const_int 3)
    )
  )
in
  begin
    try
      let (_s, t) = infer_expr env expr in
      let t' = apply_subst _s t in
      Printf.printf "Inferred type: %s\n" (string_of_ty t')
    with
    | TypeError msg ->
      Printf.printf "Type error: %s\n" msg
    | Typecheck.Unification.UnificationError msg ->
      Printf.printf "Unification error: %s\n" msg
  end;
    [%expect {|
      Unification error: Cannot unify types: int and int<cm>
    |}];;

let%expect_test "Sum of list with pattern-list" =    
    reset_typevar_counter ();  
      let sumList_expr =
      Expr_let (Recursive,        Bind (Pattern_ident_or_op "sumList",
          Expr_lam (Pattern_ident_or_op "xs",            Expr_match(
              Expr_ident_or_op "xs",              Rule(Pattern_list [], Expr_const (Const_int 0)),
              [                Rule(Pattern_list [Pattern_ident_or_op "x"], Expr_ident_or_op "x");
                Rule(Pattern_list [Pattern_ident_or_op "x"; Pattern_ident_or_op "y"],                  Expr_apply(
                    Expr_apply(Expr_ident_or_op "+", Expr_ident_or_op "x"),                    Expr_ident_or_op "y"
                  )                )
              ]            )
          )        ),
        [],        Expr_ident_or_op "sumList"
      )    in
    let (_sSum, tSum) = infer_expr env sumList_expr in    let tSum' = apply_subst _sSum tSum in
    Printf.printf "Inferred type for sumList: %s\n" (string_of_ty tSum');    
    [%expect {| Inferred type for sumList: (list<int> -> int)    |}];


