(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  [ `Unfound_var of string
  | `Unexpected_error
  | `Division_by_zero
  ]

let pp_error ppf : error -> _ = function
  | `Unfound_var name ->
    Format.fprintf
      ppf
      "Interpreter Error: couldn't find \"%a\""
      Format.pp_print_string
      name
  | `Unexpected_error -> Format.fprintf ppf "Interpreter Error: unexpected error"
  | `Division_by_zero -> Format.fprintf ppf "Interpreter Error: division by zero"
;;

module R : sig
  type 'a t = ('a, error) Result.t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end = struct
  type 'a t = ('a, error) Result.t

  let ( >>= ) e1 e2 =
    match e1 with
    | Result.Error x -> Result.Error x
    | Ok a -> e2 a
  ;;

  let return x = Result.Ok x
  let fail e = Result.Error e
  let bind x ~f = x >>= f

  let ( >>| ) e f =
    match e with
    | Result.Error x -> Result.Error x
    | Ok a -> Result.Ok (f a)
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end
end

module Interpret = struct
  open R
  open R.Syntax

  module Env : sig
    include Map.S with type key = string

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    val find : 'a t -> key -> 'a R.t
  end = struct
    include Map.Make (String)

    let find map name =
      match find_opt name map with
      | Some v -> return v
      | None -> fail (`Unfound_var name)
    ;;

    let pp pp_v ppf m =
      Format.(iter (fun k -> fprintf ppf "@[%a=%a@] " pp_print_string k pp_v) m)
    ;;
  end

  type value =
    | VConst of expr
    | VClosure of string option * value Env.t * expr
    | VRecClosure of string option * value Env.t * string list * expr
    | VDeclaration of (string * expr) list
  [@@deriving show { with_path = false }]

  let eval =
    let rec helper env = function
      | EInt n -> return (VConst (EInt n))
      | EBool b -> return (VConst (EBool b))
      | EString s -> return (VConst (EString s))
      | EBinOp (op, e1, e2) ->
        let* e1 = helper env e1 in
        let* e2 = helper env e2 in
        (match e1, e2 with
         | VConst e1', VConst e2' -> eval_binop (op, e1', e2')
         | _ -> fail `Unexpected_error)
      | EVar (name, _) -> Env.find env name
      | EFun (pattern, body) -> return (VClosure (None, env, EFun (pattern, body)))
      | ELet (NonRecursive, bindings, body) ->
        let* env_with_bindings =
          List.fold_left
            (fun acc (PVar (x, _), e) ->
              let* acc' = acc in
              let* v = helper acc' e in
              return (Env.add x v acc'))
            (return env)
            bindings
        in
        (match body with
         | Some e -> helper env_with_bindings e
         | None ->
           let* list_let =
             List.fold_left
               (fun acc (PVar (name, _), _) ->
                 let* acc' = acc in
                 let* v = Env.find env_with_bindings name in
                 match v with
                 | VConst e -> return ((name, e) :: acc')
                 | VClosure (_, _, b) -> return ((name, b) :: acc')
                 | _ -> fail `Unexpected_error)
               (return [])
               bindings
           in
           return (VDeclaration (List.rev list_let)))
      | ELet (Recursive, bindings, body) ->
        let func_list = List.map (fun (PVar (name, _), _) -> name) bindings in
        let* placeholder_env =
          List.fold_left
            (fun acc (PVar (name, _), e) ->
              let* acc' = acc in
              let* v =
                match e with
                | EFun (_, _) as ex -> return (VRecClosure (None, env, func_list, ex))
                | _ -> helper acc' e
              in
              return (Env.add name v acc'))
            (return env)
            bindings
        in
        (match body with
         | Some expr -> helper placeholder_env expr
         | None ->
           let* list_let =
             List.fold_left
               (fun acc (PVar (name, _), _) ->
                 let* acc' = acc in
                 let* v = Env.find placeholder_env name in
                 match v with
                 | VConst e -> return ((name, e) :: acc')
                 | VRecClosure (_, _, _, b) -> return ((name, b) :: acc')
                 | _ -> fail `Unexpected_error)
               (return [])
               bindings
           in
           return (VDeclaration (List.rev list_let)))
      | EApp (fn, arg) ->
        let* arg_val = helper env arg in
        helper env fn
        >>= (function
         | VClosure (_, closure_env, EFun (PVar (par, _), body)) ->
           let updated_env = Env.add par arg_val closure_env in
           helper updated_env body
         | VRecClosure (_, closure_env, recs_list, EFun (PVar (par, _), body)) ->
           let updated_env = Env.add par arg_val closure_env in
           let* final_env =
             List.fold_left
               (fun acc s ->
                 let* acc' = acc in
                 let* v = Env.find env s in
                 return (Env.add s v acc'))
               (return updated_env)
               recs_list
           in
           helper final_env body
         | _ -> fail `Unexpected_error)
      | EIf (cond, then_expr, else_expr) ->
        let* cond_val = helper env cond in
        (match cond_val with
         | VConst (EBool true) -> helper env then_expr
         | VConst (EBool false) -> helper env else_expr
         | _ -> fail `Unexpected_error)
      | ETuple ex ->
        let* ex_list =
          List.fold_left
            (fun acc e ->
              let* acc' = acc in
              let* v = helper env e in
              match v with
              | VConst x -> return (x :: acc')
              | VClosure (_, _, ex) -> return (ex :: acc')
              | VRecClosure (_, _, _, ex) -> return (ex :: acc')
              | _ -> fail `Unexpected_error)
            (return [])
            ex
        in
        return (VConst (ETuple ex_list))
      | EList ex ->
        let* ex_list =
          List.fold_left
            (fun acc e ->
              let* acc' = acc in
              let* v = helper env e in
              match v with
              | VConst x -> return (x :: acc')
              | VClosure (_, _, ex) -> return (ex :: acc')
              | VRecClosure (_, _, _, ex) -> return (ex :: acc')
              | _ -> fail `Unexpected_error)
            (return [])
            ex
        in
        return (VConst (EList ex_list))
      | ESome ex ->
        let* v = helper env ex in
        let* ex =
          match v with
          | VConst x -> return x
          | VClosure (_, _, ex) -> return ex
          | VRecClosure (_, _, _, ex) -> return ex
          | _ -> fail `Unexpected_error
        in
        return (VConst (ESome ex))
      | ENone -> return (VConst ENone)
    and eval_binop = function
      | Add, EInt i1, EInt i2 -> return (VConst (EInt (i1 + i2)))
      | Sub, EInt i1, EInt i2 -> return (VConst (EInt (i1 - i2)))
      | Mul, EInt i1, EInt i2 -> return (VConst (EInt (i1 * i2)))
      | Div, EInt i1, EInt i2 ->
        (match i2 with
         | 0 -> fail `Division_by_zero
         | _ -> return (VConst (EInt (i1 / i2))))
      | And, EBool i1, EBool i2 -> return (VConst (EBool (i1 && i2)))
      | Or, EBool i1, EBool i2 -> return (VConst (EBool (i1 || i2)))
      | Eq, EInt i1, EInt i2 -> return (VConst (EBool (i1 = i2)))
      | Eq, EBool i1, EBool i2 -> return (VConst (EBool (i1 = i2)))
      | Neq, EInt i1, EInt i2 -> return (VConst (EBool (i1 <> i2)))
      | Neq, EBool i1, EBool i2 -> return (VConst (EBool (i1 <> i2)))
      | Lt, EInt i1, EInt i2 -> return (VConst (EBool (i1 < i2)))
      | Lt, EBool i1, EBool i2 -> return (VConst (EBool (i1 < i2)))
      | Gt, EInt i1, EInt i2 -> return (VConst (EBool (i1 > i2)))
      | Gt, EBool i1, EBool i2 -> return (VConst (EBool (i1 > i2)))
      | Le, EInt i1, EInt i2 -> return (VConst (EBool (i1 <= i2)))
      | Le, EBool i1, EBool i2 -> return (VConst (EBool (i1 <= i2)))
      | Ge, EInt i1, EInt i2 -> return (VConst (EBool (i1 >= i2)))
      | Ge, EBool i1, EBool i2 -> return (VConst (EBool (i1 >= i2)))
      | _ -> fail `Unexpected_error
    in
    helper
  ;;
end

open Interpret

let interpret ex = eval Env.empty ex

let test_inter e =
  match eval Env.empty e with
  | Ok v -> print_endline (show_value v)
  | Error e -> pp_error Format.std_formatter e
;;

let test_inter_wenv env e =
  match eval env e with
  | Ok v -> print_endline (show_value v)
  | Error e -> pp_error Format.std_formatter e
;;

let%expect_test "infer: int" =
  test_inter (EInt 5);
  [%expect {| (VConst (EInt 5)) |}]
;;

let%expect_test "infer: int" =
  test_inter (EBool true);
  [%expect {| (VConst (EBool true)) |}]
;;

let%expect_test "infer: int" =
  test_inter (EString "hello");
  [%expect {| (VConst (EString "hello")) |}]
;;

(* Binon tests *)
let%expect_test "eval: addition" =
  test_inter (EBinOp (Add, EInt 3, EInt 4));
  [%expect {| (VConst (EInt 7)) |}]
;;

let%expect_test "eval: subtraction" =
  test_inter (EBinOp (Sub, EInt 10, EInt 4));
  [%expect {| (VConst (EInt 6)) |}]
;;

let%expect_test "eval: multiplication" =
  test_inter (EBinOp (Mul, EInt 2, EInt 5));
  [%expect {| (VConst (EInt 10)) |}]
;;

let%expect_test "eval: division" =
  test_inter (EBinOp (Div, EInt 10, EInt 2));
  [%expect {| (VConst (EInt 5)) |}]
;;

let%expect_test "eval: division by zero" =
  test_inter (EBinOp (Div, EInt 10, EInt 0));
  [%expect {| Interpreter Error: division by zero |}]
;;

let%expect_test "eval: logical and" =
  test_inter (EBinOp (And, EBool true, EBool false));
  [%expect {| (VConst (EBool false)) |}]
;;

let%expect_test "eval: logical or" =
  test_inter (EBinOp (Or, EBool true, EBool false));
  [%expect {| (VConst (EBool true)) |}]
;;

let%expect_test "eval: equality (integers)" =
  test_inter (EBinOp (Eq, EInt 3, EInt 3));
  [%expect {| (VConst (EBool true)) |}]
;;

let%expect_test "eval: inequality (booleans)" =
  test_inter (EBinOp (Neq, EBool true, EBool false));
  [%expect {| (VConst (EBool true)) |}]
;;

let%expect_test "eval: less than" =
  test_inter (EBinOp (Lt, EInt 2, EInt 5));
  [%expect {| (VConst (EBool true)) |}]
;;

let%expect_test "eval: greater than" =
  test_inter (EBinOp (Gt, EInt 5, EInt 3));
  [%expect {| (VConst (EBool true)) |}]
;;

let%expect_test "eval: less than or equal" =
  test_inter (EBinOp (Le, EInt 4, EInt 4));
  [%expect {| (VConst (EBool true)) |}]
;;

let%expect_test "eval: greater than or equal" =
  test_inter (EBinOp (Ge, EInt 6, EInt 6));
  [%expect {| (VConst (EBool true)) |}]
;;

(* Evar test *)
let%expect_test "EVar: unfound variable" =
  test_inter_wenv Env.empty (EVar ("x", None));
  [%expect {| Interpreter Error: couldn't find "x" |}]
;;

let%expect_test "EVar: found variable" =
  let env = Env.add "x" (VConst (EInt 42)) Env.empty in
  test_inter_wenv env (EVar ("x", None));
  [%expect {| (VConst (EInt 42)) |}]
;;

(* EFun test *)
let%expect_test "eval: fun" =
  test_inter (EFun (PVar ("x", None), EInt 42));
  [%expect {| (VClosure (None, , (EFun ((PVar ("x", None)), (EInt 42))))) |}]
;;

(* EApp test *)
let%expect_test "EApp: function application" =
  let env =
    Env.add
      "f"
      (VClosure
         ( Some "f"
         , Env.empty
         , EFun (PVar ("x", Some TInt), EBinOp (Add, EVar ("x", None), EInt 1)) ))
      Env.empty
  in
  test_inter_wenv env (EApp (EVar ("f", None), EInt 5));
  [%expect {| (VConst (EInt 6)) |}]
;;

(* EIf test *)
let%expect_test "interpret if-then-else: true condition" =
  let expr = EIf (EBool true, EInt 1, EInt 0) in
  test_inter expr;
  [%expect {| (VConst (EInt 1)) |}]
;;

let%expect_test "interpret if-then-else: false condition" =
  let expr = EIf (EBool false, EInt 1, EInt 0) in
  test_inter expr;
  [%expect {| (VConst (EInt 0)) |}]
;;

let%expect_test "interpret if-then-else: non-boolean condition error" =
  let expr = EIf (EInt 5, EInt 1, EInt 0) in
  test_inter expr;
  [%expect {| Interpreter Error: unexpected error |}]
;;

(* non rec let test *)
let%expect_test "ELet NonRecursive: multiple definitions without body" =
  test_inter_wenv
    Env.empty
    (ELet
       ( NonRecursive
       , [ PVar ("f", Some TInt), EInt 0
         ; ( PVar ("g", Some TInt)
           , EFun (PVar ("x", None), EBinOp (Add, EVar ("x", None), EInt 1)) )
         ]
       , None ));
  [%expect
    {|
    (VDeclaration
       [("f", (EInt 0));
         ("g",
          (EFun ((PVar ("x", None)), (EBinOp (Add, (EVar ("x", None)), (EInt 1)))
             )))
         ]) |}]
;;

let%expect_test "ELet NonRecursive: multiple definitions without body" =
  test_inter_wenv
    Env.empty
    (ELet
       ( NonRecursive
       , [ PVar ("f", Some TInt), EInt 0; PVar ("g", Some TInt), EInt 0 ]
       , Some (EVar ("f", None)) ));
  [%expect {| (VConst (EInt 0)) |}]
;;

(* rec let test *)
let%expect_test "recursive let: factorial" =
  let expr =
    ELet
      ( Recursive
      , [ ( PVar ("fact", None)
          , EFun
              ( PVar ("n", None)
              , EIf
                  ( EBinOp (Eq, EVar ("n", None), EInt 0)
                  , EInt 1
                  , EBinOp
                      ( Mul
                      , EVar ("n", None)
                      , EApp (EVar ("fact", None), EBinOp (Sub, EVar ("n", None), EInt 1))
                      ) ) ) )
        ]
      , Some (EApp (EVar ("fact", None), EInt 5)) )
  in
  test_inter expr;
  [%expect {| (VConst (EInt 120)) |}]
;;

let%expect_test "recursive let: fibonacci" =
  let expr =
    ELet
      ( Recursive
      , [ ( PVar ("fib", None)
          , EFun
              ( PVar ("n", None)
              , EIf
                  ( EBinOp (Lt, EVar ("n", None), EInt 2)
                  , EVar ("n", None)
                  , EBinOp
                      ( Add
                      , EApp (EVar ("fib", None), EBinOp (Sub, EVar ("n", None), EInt 1))
                      , EApp (EVar ("fib", None), EBinOp (Sub, EVar ("n", None), EInt 2))
                      ) ) ) )
        ]
      , Some (EApp (EVar ("fib", None), EInt 7)) )
  in
  test_inter expr;
  [%expect {| (VConst (EInt 13)) |}]
;;

let%expect_test "recursive let: multiple recursive functions" =
  let expr =
    ELet
      ( Recursive
      , [ ( PVar ("even", None)
          , EFun
              ( PVar ("n", None)
              , EIf
                  ( EBinOp (Eq, EVar ("n", None), EInt 0)
                  , EInt 1
                  , EApp (EVar ("odd", None), EBinOp (Sub, EVar ("n", None), EInt 1)) ) )
          )
        ; ( PVar ("odd", None)
          , EFun
              ( PVar ("n", None)
              , EIf
                  ( EBinOp (Eq, EVar ("n", None), EInt 0)
                  , EInt 0
                  , EApp (EVar ("even", None), EBinOp (Sub, EVar ("n", None), EInt 1)) )
              ) )
        ]
      , Some (EApp (EVar ("even", None), EInt 4)) )
  in
  test_inter expr;
  [%expect {| (VConst (EInt 1)) |}]
;;

let%expect_test "recursive let: multiple recursive functions" =
  let expr =
    ELet
      ( Recursive
      , [ ( PVar ("even", Some (TFun (TInt, TInt)))
          , EFun
              ( PVar ("n", Some TInt)
              , EIf
                  ( EBinOp (Eq, EVar ("n", None), EInt 0)
                  , EInt 1
                  , EApp (EVar ("odd", None), EBinOp (Sub, EVar ("n", None), EInt 1)) ) )
          )
        ; ( PVar ("odd", Some (TFun (TInt, TInt)))
          , EFun
              ( PVar ("n", Some TInt)
              , EIf
                  ( EBinOp (Eq, EVar ("n", None), EInt 0)
                  , EInt 0
                  , EApp (EVar ("even", None), EBinOp (Sub, EVar ("n", None), EInt 1)) )
              ) )
        ]
      , None )
  in
  test_inter expr;
  [%expect
    {|
    (VDeclaration
       [("even",
         (EFun ((PVar ("n", (Some TInt))),
            (EIf ((EBinOp (Eq, (EVar ("n", None)), (EInt 0))), (EInt 1),
               (EApp ((EVar ("odd", None)),
                  (EBinOp (Sub, (EVar ("n", None)), (EInt 1)))))
               ))
            )));
         ("odd",
          (EFun ((PVar ("n", (Some TInt))),
             (EIf ((EBinOp (Eq, (EVar ("n", None)), (EInt 0))), (EInt 0),
                (EApp ((EVar ("even", None)),
                   (EBinOp (Sub, (EVar ("n", None)), (EInt 1)))))
                ))
             )))
         ]) |}]
;;

let%expect_test "interpreter: tuple" =
  let expr = ETuple [ EInt 1; EInt 2; EInt 3 ] in
  test_inter expr;
  [%expect {| (VConst (ETuple [(EInt 3); (EInt 2); (EInt 1)])) |}]
;;

let%expect_test "interpreter: list" =
  let expr = EList [ EInt 1; EInt 2; EInt 3 ] in
  test_inter expr;
  [%expect {| (VConst (EList [(EInt 3); (EInt 2); (EInt 1)])) |}]
;;

let%expect_test "interpretation of Some expression" =
  let expr = ESome (EInt 42) in
  test_inter expr;
  [%expect {| (VConst (ESome (EInt 42))) |}]
;;

let%expect_test "interpretation of None expression" =
  let expr = ENone in
  test_inter expr;
  [%expect {| (VConst ENone) |}]
;;
