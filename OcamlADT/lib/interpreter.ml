(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Format
open Printf

type error =
  | DivisionByZero
  | TypeMismatch
  | NotImplemented
  | UnboundVariable of string
  | PatternMismatch
  | RecursionError
  | IDK
  | EmptyProgram

type value =
  | VInt of int
  | VString of string
  | VChar of char
  | VBool of bool
  | VTuple of value List2.t
  | VFun of Pattern.t List1.t * Expression.t * environment * Expression.rec_flag
  | VFunction of Expression.t Expression.case List1.t * environment
  | VConstruct of ident * value option
  | VBuiltin_binop of (value -> value -> (value, error) Result.t)
  | VBuiltin_print of (value -> (value, error) Result.t)

and environment = (string, value, String.comparator_witness) Base.Map.t

let compare_values v1 v2 =
  match v1, v2 with
  | VInt i1, VInt i2 -> i1 = i2
  | VString s1, VString s2 -> Base.String.equal s1 s2
  | VChar c1, VChar c2 -> Base.Char.equal c1 c2
  | _ -> false
;;

let list1_to_list2 lst =
  match lst with
  | el1, el2 :: ell -> Some (el1, el2, ell)
  | _ -> None
;;

let make_list1 lst =
  match lst with
  | [] -> None
  | x :: xs -> Some (x, xs)
;;

module type Error_monad = sig
  (* 'a - successfull value, 'e - error type *)
  type ('a, 'e) t

  (* Wraps in Result type *)
  val return : 'a -> ('a, 'e) t

  (* Monad interface description *)
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Env (M : Error_monad) = struct
  (*Environment functor that is used to store the initialized data in some scope.
    It is treated like an abstract mapping (a lil different from one in TypeInf)*)

  open M

  let builtin_functions =
    Base.Map.of_alist_exn
      (module String)
      [ ( "print_endline"
        , VBuiltin_print
            (fun v ->
              match v with
              | VString s ->
                print_endline s;
                Ok (VString "")
              | _ -> Error TypeMismatch) )
      ; ( "print_int"
        , VBuiltin_print
            (fun v ->
              match v with
              | VInt s ->
                print_int s;
                Ok (VInt 0)
              | _ -> Error TypeMismatch) )
      ; ( "print_char"
        , VBuiltin_print
            (fun v ->
              match v with
              | VChar c ->
                print_char c;
                Ok (VChar ' ')
              | _ -> Error TypeMismatch) )
      ; ( "print_bool"
        , VBuiltin_print
            (fun v ->
              match v with
              | VBool b ->
                print_bool b;
                Ok (VBool true)
              | _ -> Error TypeMismatch) )
      ]
  ;;

  let builtin_binops =
    Base.Map.of_alist_exn
      (module String)
      [ ( "+"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VInt (a + b))
              | _ -> Error TypeMismatch) )
      ; ( "-"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VInt (a - b))
              | _ -> Error TypeMismatch) )
      ; ( "*"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VInt (a * b))
              | _ -> Error TypeMismatch) )
      ; ( "/"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt _, VInt 0 -> Error DivisionByZero
              | VInt a, VInt b -> Ok (VInt (a / b))
              | _ -> Error TypeMismatch) )
      ; ( "="
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a == b))
              | VBool a, VBool b -> Ok (VBool (a == b))
              | _ -> Error TypeMismatch) )
      ; ( ">"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a > b))
              | _ -> Error TypeMismatch) )
      ; ( "<"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a < b))
              | _ -> Error TypeMismatch) )
      ; ( ">="
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a >= b))
              | _ -> Error TypeMismatch) )
      ; ( "<="
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a <= b))
              | _ -> Error TypeMismatch) )
      ; ( "<>"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a <> b))
              | _ -> Error TypeMismatch) )
      ; ( "&&"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VBool a, VBool b -> Ok (VBool (a && b))
              | _ -> Error TypeMismatch) )
      ; ( "||"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VBool a, VBool b -> Ok (VBool (a || b))
              | _ -> Error TypeMismatch) )
      ]
  ;;

  let init =
    let merged_binops_and_functions =
      Base.Map.merge builtin_binops builtin_functions ~f:(fun ~key:_ ->
          function
          | `Left v -> Some v
          | `Right v -> Some v
          | `Both (v, _) -> Some v)
    in
    Base.Map.merge
      merged_binops_and_functions
      (Base.Map.empty (module Base.String))
      ~f:(fun ~key:_ ->
        function
        | `Left v -> Some v
        | `Right v -> Some v
        | `Both (v, _) -> Some v)
  ;;

  let lookup env name =
    match Map.find env name with
    | Some s -> return s (* returing the value *)
    | None -> fail (UnboundVariable name)
  ;;

  let extend env name value = Base.Map.set env ~key:name ~data:value

  let combine env1 env2 =
    Map.fold env2 ~f:(fun ~key ~data env_acc -> extend env_acc key data) ~init:env1
  ;;
end

(*Interpretator functor, that uses M monad as a base for evaluation *)
module Interpreter (M : Error_monad) = struct
  open M
  module E = Env (M)

  let lift_result r =
    match r with
    | Ok v -> return v
    | Error e -> fail e
  ;;

  (* mapM applies a monadic function f to each element of a list and combines results in a list *)
  let mapM f env lst =
    let rec aux acc = function
      | [] -> return (List.rev acc)
      | x :: xs ->
        let* res = f env x in
        aux (res :: acc) xs
    in
    aux [] lst
  ;;

  let mapM2 f env lst lst2 =
    let rec aux acc = function
      | [], [] -> return (List.rev acc)
      | x :: xs, y :: ys ->
        let* res = f x y env in
        aux (res :: acc) (xs, ys)
      | _ -> fail PatternMismatch (* In case lists have different lengths *)
    in
    aux [] (lst, lst2)
  ;;

  let eval_const = function
    | Constant.Const_integer i -> return (VInt i)
    | Constant.Const_char c -> return (VChar c)
    | Constant.Const_string s -> return (VString s)
  ;;

  let rec eval_pattern pattern value env =
    match pattern, value with
    | Pattern.Pat_any, _ -> return (Some env)
    | Pattern.Pat_var var, v -> return (Some (E.extend env var v))
    | Pattern.Pat_constant c, v ->
      let* const_val = eval_const c in
      if compare_values const_val v then return (Some env) else return None
    | Pattern.Pat_tuple (p1, p2, ps), VTuple (v1, v2, vs) ->
      let* _ = eval_pattern p1 v1 env in
      let* _ = eval_pattern p2 v2 env in
      let* _ = mapM2 eval_pattern env ps vs in
      return (Some env)
      (*TODO*)
    | Pattern.Pat_constraint (pat, _), v -> eval_pattern pat v env
    | Pattern.Pat_construct (ctor, Some args), VFun (ctor_pat, body, env, _) -> fail IDK
    | Pattern.Pat_construct (ctor, None), VString s ->
      if String.equal ctor s then return (Some env) else fail PatternMismatch
    | _ -> fail PatternMismatch
  ;;

  let rec eval_expr (env : environment) = function
    | Expression.Exp_ident name -> E.lookup env name
    | Expression.Exp_constant ex -> eval_const ex
    | Expression.Exp_tuple (e1, e2, el) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      let* vl = mapM eval_expr env el in
      return (VTuple (v1, v2, vl))
    | Expression.Exp_function (c1, cl) -> return (VFunction ((c1, cl), env))
    | Expression.Exp_fun (patterns, body) ->
      return (VFun (patterns, body, env, Nonrecursive))
    | Expression.Exp_apply (func, args) ->
      let* func_val = eval_expr env func in
      (match func_val with
       | VBuiltin_binop binop ->
         (match args with
          | Expression.Exp_tuple (arg1, arg2, []) ->
            let* arg1_val = eval_expr env arg1 in
            let* arg2_val = eval_expr env arg2 in
            let* binop_res = lift_result (binop arg1_val arg2_val) in
            return binop_res
          | exp ->
            (*negative operator with 1 operand case *)
            let* arg_val = eval_expr env exp in
            let* binop_res = lift_result (binop (VInt 0) arg_val) in
            return binop_res)
       | VBuiltin_print print_fn ->
         (match args with
          | Expression.Exp_constant c ->
            let* arg_val = eval_const c in
            let* _ = lift_result (print_fn arg_val) in
            return (VString "")
          | Expression.Exp_ident id ->
            let* arg_val = E.lookup env id in
            let* _ = lift_result (print_fn arg_val) in
            return (VString "")
          | Expression.Exp_apply (func, args) ->
            let* arg_val = eval_expr env (Expression.Exp_apply (func, args)) in
            let* _ = lift_result (print_fn arg_val) in
            return (VString "")
          | _ -> fail PatternMismatch)
       | VFun (patterns, body, fun_env, rec_flag) ->
         let p1, pl = patterns in
         let* arg_val = eval_expr env args in
         let* extended_env_opt = eval_pattern p1 arg_val fun_env in
         let* new_env =
           match rec_flag, extended_env_opt with
           | Recursive, Some extended_env -> return (E.combine env extended_env)
           | Nonrecursive, Some extended_env -> return extended_env
           | _, None -> fail PatternMismatch
         in
         (match pl with
          | [] -> eval_expr new_env body
          | pf :: p_rest -> return (VFun ((pf, p_rest), body, new_env, Recursive)))
       | _ -> fail TypeMismatch)
    | Expression.Exp_match (expr, cases) ->
      let c1, cl = cases in
      let* v = eval_expr env expr in
      let rec eval_cases = function
        | [] -> fail PatternMismatch
        | { Expression.first = pattern; second = body } :: rest ->
          let* env' = eval_pattern pattern v env in
          (match env' with
           | Some env' -> eval_expr env' body
           | _ -> eval_cases rest)
      in
      eval_cases (c1 :: cl)
    | Expression.Exp_if (cond, then_expr, else_expr_opt) ->
      let* cond_val = eval_expr env cond in
      (match cond_val with
       | VBool false ->
         (match else_expr_opt with
          | Some else_expr -> eval_expr env else_expr
          | None -> fail PatternMismatch)
       | VBool true -> eval_expr env then_expr
       | _ -> fail TypeMismatch)
    | Expression.Exp_let (Nonrecursive, (b1, bl), body) ->
      (* Non-recursive bindings: evaluate and extend one by one *)
      let rec eval_value_binding_list env value_binding_list =
        List.fold_left
          ~init:(return env)
          ~f:(fun acc_env { Expression.pat; expr } ->
            let* env = acc_env in
            let* value = eval_expr env expr in
            match pat with
            | Pattern.Pat_var name | Pattern.Pat_constraint (Pattern.Pat_var name, _) ->
              let env = E.extend env name value in
              return env
            | _ ->
              let* env = eval_pattern pat value env in
              (match env with
               | Some extended_env -> return extended_env
               | None -> fail PatternMismatch))
          value_binding_list
      in
      let* env = eval_value_binding_list env (b1 :: bl) in
      eval_expr env body
    | Expression.Exp_let (Recursive, (b1, bl), body) ->
      (* Handle recursive bindings directly *)
      let rec eval_rec_value_binding_list env value_binding_list =
        List.fold_left
          ~init:(return env)
          ~f:(fun acc_env { Expression.pat; expr } ->
            let* env = acc_env in
            let* value = eval_expr env expr in
            match pat with
            | Pattern.Pat_var name | Pattern.Pat_constraint (Pattern.Pat_var name, _) ->
              let value =
                match value with
                | VFun (patterns, body, closure_env, Nonrecursive) ->
                  VFun (patterns, body, closure_env, Recursive)
                | other -> other
              in
              let env = E.extend env name value in
              return env
            | _ -> fail PatternMismatch)
          value_binding_list
      in
      let* env = eval_rec_value_binding_list env (b1 :: bl) in
      eval_expr env body
    | Expression.Exp_construct (ctor, Some arg) ->
      fail IDK
      (*let* v = eval_expr env arg in
        return (VFun ([ Pattern.Pat_constant (Constant.Const_string ctor) ], v)) *)
    | Expression.Exp_construct (ctor, None) -> return (VString ctor)
    | Expression.Exp_constraint (expr, _type_expr) -> eval_expr env expr
  ;;

  let eval_str_item (env : environment) = function
    | Structure.Str_eval str ->
      (* Evaluate the expression and return its value *)
      eval_expr env str
    | Structure.Str_value (rec_flag, bindings) ->
      (* Extend the environment based on bindings and return the value of the last binding *)
      let bindings_list = fst bindings :: snd bindings in
      let extend_env env bindings =
        List.fold bindings ~init:(return env) ~f:(fun acc { Expression.pat; expr } ->
          let* env = acc in
          let* value =
            match rec_flag with
            | Nonrecursive -> eval_expr env expr
            | Recursive -> fail IDK (* Add support for recursive bindings later *)
          in
          let* env' = eval_pattern pat value env in
          match env' with
          | Some new_env -> return new_env
          | None -> fail PatternMismatch)
      in
      let* new_env = extend_env env bindings_list in
      (* Evaluate and return the last expression's value *)
      (match List.rev bindings_list with
       | { Expression.expr = last_expr } :: _ -> eval_expr new_env last_expr
       | _ -> fail PatternMismatch)
  ;;

  (*to add adt*)

  let interpret_expr expr = eval_expr E.init expr

  let interpret_program (prog : program) =
    let rec eval_prog env = function
      | [] -> fail EmptyProgram (* Handle empty programs appropriately *)
      | [ item ] -> eval_str_item env item (* Return the value of the last item *)
      | item :: rest ->
        let* _ = eval_str_item env item in
        eval_prog env rest
    in
    eval_prog E.init prog
  ;;
end

module RESULT_MONAD_ERROR = struct
  (* Basic Result monad extension.
     Result is used for more advanced error handling *)
  include Result

  let ( let* ) m f = m >>= fun x -> f x
end

(* Interpreter functor extension *)
module InterpreterWResult = Interpreter (RESULT_MONAD_ERROR)

let run_interpreter = InterpreterWResult.interpret_program

module PPrinter = struct
  open Format

  let rec pp_value fmt = function
    | VInt i -> fprintf fmt "%d" i
    | VChar c -> fprintf fmt "%c" c
    | VString s -> fprintf fmt "%s" s
    | VBool b -> fprintf fmt "%b" b
    | VTuple (fst_val, snd_val, val_list) ->
      fprintf
        fmt
        "(%a)"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
        (fst_val :: snd_val :: val_list)
    | VFun _ -> fprintf fmt "<fun>"
    | VFunction _ -> fprintf fmt "<function>"
    | VBuiltin_print _ -> fprintf fmt "<builtin>"
    (*TODO*)
    | _ -> fprintf fmt "Intepreter error: Value error"
  ;;

  let pp_error fmt = function
    | IDK -> fprintf fmt "IDK"
    | PatternMismatch -> fprintf fmt "Intepreter error: Pattern mismatch"
    | DivisionByZero -> fprintf fmt "Intepreter error: Division by zero"
    | NotImplemented -> fprintf fmt "Intepreter error: Not implemented"
    | UnboundVariable s -> fprintf fmt "Intepreter error: Unbound value %s" s
    | TypeMismatch -> fprintf fmt "Intepreter error: Type mismatch"
    | RecursionError -> fprintf fmt "Interpreter error: Recursion error"
    | EmptyProgram -> fprintf fmt "Interpreter error: Empty program"
  ;;

  let print_value = printf "%a" pp_value
  let print_error = printf "%a" pp_error
end
