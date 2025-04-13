(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | DivisionByZero
  | TypeMismatch (* just a stub for interp unit tests + some below *)
  | UnboundVariable of string
  | PatternMismatch
  | RecursionError
  | EmptyProgram
  | ParserError
  | NotAnADT of string
  | NotAnADTVariant of string
  | UndefinedConstructor of string
  | UndefinedArgs

type value =
  | VInt of int
  | VString of string
  | VChar of char
  | VBool of bool
  | VTuple of value List2.t
  | VFun of Pattern.t List1.t * Expression.t * environment * Expression.rec_flag
  | VFunction of Expression.t Expression.case List1.t * environment
  | VConstruct of ident * value option
  | VAdt of (value * ident list * ident * (ident * TypeExpr.t option) List1.t)
  (* ident list is being left for type printing *)
  | VUnit
  | VType of TypeExpr.t * ident option (* ident - adt type name *)
  | VBuiltin_binop of (value -> value -> (value, error) Result.t)
  | VBuiltin_print of (value -> (value, error) Result.t)

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

let compare_values v1 v2 =
  match v1, v2 with
  | VInt i1, VInt i2 -> i1 = i2
  | VString s1, VString s2 -> Base.String.equal s1 s2
  | VChar c1, VChar c2 -> Base.Char.equal c1 c2
  | _ -> false
;;

let list1_to_list2 = function
  | el1, el2 :: ell -> Some (el1, el2, ell)
  | _ -> None
;;

let make_list1 = function
  | [] -> None
  | x :: xs -> Some (x, xs)
;;

let to_bool = function
  | VBool b -> b
  | VInt n -> n <> 0
  | _ -> raise (Invalid_argument "TypeMismatch")
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

  let builtin_bools =
    Base.Map.of_alist_exn
      (module Base.String)
      [ "true", VBool true; "false", VBool false ]
  ;;

  let builtin_functions =
    Base.Map.of_alist_exn
      (module Base.String)
      [ ( "print_endline"
        , VBuiltin_print
            (function
              | VString s ->
                print_endline s;
                Ok VUnit
              | _ -> Error TypeMismatch) )
      ; ( "print_int"
        , VBuiltin_print
            (function
              | VInt i ->
                print_endline (string_of_int i);
                Ok VUnit
              | _ -> Error TypeMismatch) )
      ; ( "print_char"
        , VBuiltin_print
            (function
              | VChar c ->
                print_endline (String.make 1 c);
                Ok VUnit
              | _ -> Error TypeMismatch) )
      ; ( "print_bool"
        , VBuiltin_print
            (function
              | VBool b ->
                print_endline (string_of_bool b);
                Ok VUnit
              | _ -> Error TypeMismatch) )
      ]
  ;;

  let create_binop f =
    VBuiltin_binop
      (fun v1 v2 ->
        match v1, v2 with
        | VInt a, VInt b -> Ok (VInt (f a b))
        | _ -> Error TypeMismatch)
  ;;

  let builtin_binops =
    Base.Map.of_alist_exn
      (module Base.String)
      [ "+", create_binop ( + )
      ; "-", create_binop ( - )
      ; "*", create_binop ( * )
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
              | VInt a, VInt b -> Ok (VBool (a = b))
              | VBool a, VBool b -> Ok (VBool (a = b))
              | VString a, VString b -> Ok (VBool (a = b))
              | VChar a, VChar b -> Ok (VBool (a = b))
              | _ -> Error TypeMismatch) )
      ; ( ">"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a > b))
              | VString a, VString b -> Ok (VBool (a > b))
              | VChar a, VChar b -> Ok (VBool (a > b))
              | _ -> Error TypeMismatch) )
      ; ( "<"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a < b))
              | VString a, VString b -> Ok (VBool (a < b))
              | VChar a, VChar b -> Ok (VBool (a < b))
              | _ -> Error TypeMismatch) )
      ; ( ">="
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a >= b))
              | VString a, VString b -> Ok (VBool (a >= b))
              | VChar a, VChar b -> Ok (VBool (a >= b))
              | _ -> Error TypeMismatch) )
      ; ( "<="
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a <= b))
              | VString a, VString b -> Ok (VBool (a <= b))
              | VChar a, VChar b -> Ok (VBool (a <= b))
              | _ -> Error TypeMismatch) )
      ; ( "<>"
        , VBuiltin_binop
            (fun v1 v2 ->
              match v1, v2 with
              | VInt a, VInt b -> Ok (VBool (a <> b))
              | VString a, VString b -> Ok (VBool (a <> b))
              | VChar a, VChar b -> Ok (VBool (a <> b))
              | _ -> Error TypeMismatch) )
      ; ( "&&"
        , VBuiltin_binop
            (fun v1 v2 ->
              try Ok (VBool (to_bool v1 && to_bool v2)) with
              | Invalid_argument _ -> Error TypeMismatch) )
      ; ( "||"
        , VBuiltin_binop
            (fun v1 v2 ->
              try Ok (VBool (to_bool v1 || to_bool v2)) with
              | Invalid_argument _ -> Error TypeMismatch) )
      ]
  ;;

  let init =
    let all_bindings =
      List.concat
        [ Base.Map.to_alist builtin_binops
        ; Base.Map.to_alist builtin_functions
        ; Base.Map.to_alist builtin_bools
        ]
    in
    Base.Map.of_alist_reduce (module Base.String) all_bindings ~f:(fun v _ -> v)
  ;;

  (* If duplicates exist, prefer the first occurrence *)
  let lookup env name =
    match Base.Map.find env name with
    | Some s -> return s
    | None -> fail (UnboundVariable name)
  ;;

  let extend env name value = Base.Map.set env ~key:name ~data:value

  let combine env1 env2 =
    Base.Map.fold env2 ~f:(fun ~key ~data env_acc -> extend env_acc key data) ~init:env1
  ;;
end

(*Interpretator functor, that uses M monad as a base for evaluation *)
module Interpreter (M : Error_monad) = struct
  open M
  module E = Env (M)

  let lift_result = function
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
    let rec aux acc env = function
      | [], [] -> return (Some env)
      | x :: xs, y :: ys ->
        let* env_opt = f x y env in
        (match env_opt with
         | Some new_env -> aux acc new_env (xs, ys)
         | None -> return None)
      | _ -> fail PatternMismatch (* In case lists have different lengths *)
    in
    aux [] env (lst, lst2)
  ;;

  let eval_const = function
    | Constant.Const_integer i -> return (VInt i)
    | Constant.Const_char c -> return (VChar c)
    | Constant.Const_string s -> return (VString s)
  ;;

  (* let rec eval_type_expr env = function
     | TypeExpr.Type_var ident ->
     let* as_value = E.lookup env ident in
     (match as_value with
     | VType (type_value, _) -> return type_value
     | _ -> fail TypeMismatch)
     | TypeExpr.Type_arrow (t1, t2) ->
     let* t1_resolved = eval_type_expr env t1 in
     let* t2_resolved = eval_type_expr env t2 in
     return (TypeExpr.Type_arrow (t1_resolved, t2_resolved))
     | TypeExpr.Type_tuple types ->
     let t1, t2, tl = types in
     let* rt1 = eval_type_expr env t1 in
     let* rt2 = eval_type_expr env t2 in
     let* rtl = mapM eval_type_expr env tl in
     return (TypeExpr.Type_tuple (rt1, rt2, rtl))
     | TypeExpr.Type_construct (type_name, args) ->
     let* as_val = E.lookup env type_name in
     (match as_val with
     | VType (TypeExpr.Type_construct (_, tparams), _) ->
     if List.length args <> List.length tparams
     then fail UndefinedArgs
     else
     let* resolved_args = mapM eval_type_expr env args in
     return (TypeExpr.Type_construct (type_name, resolved_args))
     | VType (TypeExpr.Type_var s, _) -> return (TypeExpr.Type_var s)
     | VAdt (_, tparams, _, _) ->
     if List.length args <> List.length tparams
     then fail UndefinedArgs
     else
     let* resolved_args = mapM eval_type_expr env args in
     return (TypeExpr.Type_construct (type_name, resolved_args))
     | _ -> fail (UndefinedConstructor type_name))
     ;; *)

  let rec eval_pattern pattern value env =
    match pattern, value with
    | Pattern.Pat_any, _ -> return (Some env)
    | Pattern.Pat_var var, v -> return (Some (E.extend env var v))
    | Pattern.Pat_constant c, v ->
      let* const_val = eval_const c in
      if compare_values const_val v then return (Some env) else return None
    | Pattern.Pat_tuple (p1, p2, ps), VTuple (v1, v2, vs) ->
      let* env1_opt = eval_pattern p1 v1 env in
      let* env1 =
        match env1_opt with
        | Some env -> return env
        | None -> fail PatternMismatch
      in
      let* env2_opt = eval_pattern p2 v2 env1 in
      let* env2 =
        match env2_opt with
        | Some env -> return env
        | None -> fail PatternMismatch
      in
      let* final_env_opt = mapM2 eval_pattern env2 ps vs in
      return final_env_opt
    | Pattern.Pat_construct ("Some", Some p), VConstruct ("Some", Some v) ->
      eval_pattern p v env
    | Pattern.Pat_construct ("None", None), VConstruct ("None", None) -> return (Some env)
    | Pattern.Pat_construct ("()", None), _ -> return (Some env)
    | ( Pattern.Pat_construct ("::", Some (Pattern.Pat_tuple (p_hd, p_tl, [])))
      , VConstruct ("::", Some (VTuple (v_hd, v_tl, []))) ) ->
      let* env1_opt = eval_pattern p_hd v_hd env in
      let* env1 =
        match env1_opt with
        | Some env -> return env
        | None -> fail PatternMismatch
      in
      eval_pattern p_tl v_tl env1
    | Pattern.Pat_construct (cname, Some pat), VAdt (args, _, tname, _) ->
      if String.equal cname tname
      then (
        match args with
        | VTuple (v1, v2, vs) ->
          (match pat with
           | Pattern.Pat_tuple (p1, p2, ps) ->
             let* env1_opt = eval_pattern p1 v1 env in
             let* env1 =
               match env1_opt with
               | Some env -> return env
               | None -> fail PatternMismatch
             in
             let* env2_opt = eval_pattern p2 v2 env1 in
             let* env2 =
               match env2_opt with
               | Some env -> return env
               | None -> fail PatternMismatch
             in
             let* final_env_opt = mapM2 eval_pattern env2 ps vs in
             return final_env_opt
           | _ -> fail PatternMismatch)
        | VConstruct ("[]", None) ->
          (match pat with
           | Pattern.Pat_construct ("[]", None) -> return (Some env)
           | _ -> fail PatternMismatch)
        | VConstruct ("::", Some (VTuple (head, tail, []))) ->
          (match pat with
           | Pattern.Pat_construct ("::", Some (Pattern.Pat_tuple (ph, pt, []))) ->
             let* env1_opt = eval_pattern ph head env in
             let* env1 =
               match env1_opt with
               | Some env -> return env
               | None -> fail PatternMismatch
             in
             eval_pattern pt tail env1
           | _ -> fail PatternMismatch)
        | _ -> eval_pattern pat args env)
      else return None
    | Pattern.Pat_construct (ctor, None), VString s ->
      if String.equal ctor s then return (Some env) else fail PatternMismatch
    | Pattern.Pat_construct (ctor, None), VAdt (_, _, tname, _) ->
      if String.equal ctor tname then return (Some env) else return None
    | Pattern.Pat_construct (cname, Some p), v ->
      (match v with
       | VAdt (args, _, tname, _) ->
         if String.equal cname tname
         then (
           match args with
           | VTuple (v1, v2, vs) ->
             (match p with
              | Pattern.Pat_tuple (p1, p2, ps) ->
                let* env1_opt = eval_pattern p1 v1 env in
                let* env1 =
                  match env1_opt with
                  | Some env -> return env
                  | None -> fail PatternMismatch
                in
                let* env2_opt = eval_pattern p2 v2 env1 in
                let* env2 =
                  match env2_opt with
                  | Some env -> return env
                  | None -> fail PatternMismatch
                in
                let* final_env_opt = mapM2 eval_pattern env2 ps vs in
                return final_env_opt
              | _ -> fail PatternMismatch)
           | _ -> eval_pattern p args env)
         else return None
       | VConstruct (_, None) | VInt _ -> eval_pattern p v env
       | VUnit -> return (Some env)
       | VConstruct (name, Some value) ->
         if String.equal cname name then eval_pattern p value env else return None
       | _ -> fail PatternMismatch)
    | Pattern.Pat_construct (cname, None), v ->
      (match v with
       | VConstruct (name, Some _) ->
         if String.equal cname name then return (Some env) else return None
       | VUnit -> return (Some env)
       | VConstruct (_, None) | VInt _ -> return (Some env)
       | _ -> fail PatternMismatch)
    | Pattern.Pat_constraint (pat, _), v -> eval_pattern pat v env
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
            (* Negative operator with 1 operand case *)
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
       | VFunction ((c1, cl), env) ->
         let* arg_val = eval_expr env args in
         eval_cases env arg_val (c1 :: cl)
       | _ -> fail TypeMismatch)
    | Expression.Exp_match (expr, cases) ->
      let c1, cl = cases in
      let* v = eval_expr env expr in
      eval_cases env v (c1 :: cl)
    | Expression.Exp_if (cond, then_expr, else_expr_opt) ->
      let* cond_val = eval_expr env cond in
      (match cond_val with
       | VBool false ->
         (match else_expr_opt with
          | Some else_expr -> eval_expr env else_expr
          | None -> fail PatternMismatch)
       | VBool true -> eval_expr env then_expr
       | VConstruct ("true", None) -> eval_expr env then_expr
       | VConstruct ("false", None) ->
         (match else_expr_opt with
          | Some else_expr -> eval_expr env else_expr
          | None -> fail PatternMismatch)
       | _ -> fail PatternMismatch)
    | Expression.Exp_let (Nonrecursive, (b1, bl), body) ->
      (* Non-recursive bindings: evaluate and extend one by one *)
      let* env = eval_value_binding_list env (b1 :: bl) in
      eval_expr env body
    | Expression.Exp_let (Recursive, (b1, bl), body) ->
      (* Handle recursive bindings directly *)
      let* env = eval_rec_value_binding_list env (b1 :: bl) in
      eval_expr env body
    | Expression.Exp_construct ("Some", Some e) ->
      let* v = eval_expr env e in
      return (VConstruct ("Some", Some v))
    | Expression.Exp_construct ("None", None) -> return (VConstruct ("None", None))
    | Expression.Exp_construct ("()", None) -> return VUnit
    | Expression.Exp_construct (ctor_name, args) ->
      (match args with
       | Some provided_args ->
         let* evaluated_args = eval_expr env provided_args in
         return (VConstruct (ctor_name, Some evaluated_args))
       | None -> return (VConstruct (ctor_name, None)))
    | Expression.Exp_constraint (expr, _type_expr) -> eval_expr env expr

  and eval_cases env value = function
    | [] -> fail PatternMismatch
    | { Expression.first = pattern; second = body } :: rest ->
      let* env' = eval_pattern pattern value env in
      (match env' with
       | Some extended_env -> eval_expr extended_env body
       | None -> eval_cases env value rest)

  and eval_rec_value_binding_list env value_binding_list =
    Base.List.fold_left
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

  and eval_value_binding_list env value_binding_list =
    Base.List.fold_left
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
  ;;

  let eval_str_item (env : environment) olist =
    let rec extract_names_from_pat env acc = function
      | Pattern.Pat_var id ->
        let* value = E.lookup env id in
        return (acc @ [ Some id, value ])
      | Pattern.Pat_tuple (fst_pat, snd_pat, pat_list) ->
        Base.List.fold_left
          (fst_pat :: snd_pat :: pat_list)
          ~init:(return acc)
          ~f:(fun acc_monadic pat ->
            let* acc = acc_monadic in
            extract_names_from_pat env acc pat)
      | Pattern.Pat_construct ("::", Some exp) ->
        (match exp with
         | Pattern.Pat_tuple (head, tail, []) ->
           let* acc = extract_names_from_pat env acc head in
           extract_names_from_pat env acc tail
         | _ -> return acc)
      | Pattern.Pat_construct ("Some", Some pat) -> extract_names_from_pat env acc pat
      | Pattern.Pat_constraint (pat, _) -> extract_names_from_pat env acc pat
      | _ -> return acc
    in
    (* Extract names from value bindings *)
    let get_names_from_vb env bindings =
      Base.List.fold_left
        ~init:(return [])
        ~f:(fun acc_monadic { Expression.pat; _ } ->
          let* acc = acc_monadic in
          extract_names_from_pat env acc pat)
          (* Extract names from patterns in bindings *)
        bindings
    in
    function
    | Structure.Str_eval str ->
      let* vl = eval_expr env str in
      return (env, olist @ [ None, vl ])
      (* No tag for the evaluated expression *)
    | Structure.Str_value (Nonrecursive, bindings) ->
      let bindings_list = fst bindings :: snd bindings in
      let* env = eval_value_binding_list env bindings_list in
      let* vl = get_names_from_vb env bindings_list in
      return (env, olist @ vl)
    | Structure.Str_value (Recursive, bindings) ->
      let bindings_list = fst bindings :: snd bindings in
      let* env = eval_rec_value_binding_list env bindings_list in
      let* vl = get_names_from_vb env bindings_list in
      return (env, olist @ vl)
    | Structure.Str_adt (targs, type_name, constructors) ->
      (* Add the ADT type itself to the environment *)
      let new_env =
        E.extend env type_name (VAdt (VUnit, targs, type_name, constructors))
      in
      return (new_env, olist)
  ;;

  let remove_duplicates out_list =
    let fun_equal el1 el2 =
      match el1, el2 with
      | (Some id1, _), (Some id2, _) -> String.equal id1 id2
      | _ -> false
    in
    Base.List.fold_right out_list ~init:[] ~f:(fun x acc ->
      if Base.List.exists acc ~f:(fun y -> fun_equal x y) then acc else x :: acc)
  ;;

  let interpret_program (prog : program) =
    let rec eval_prog env olist = function
      | [] -> return olist
      | [ item ] ->
        let* _, vl = eval_str_item env olist item in
        return (olist @ vl)
      | item :: rest ->
        let* new_env, new_olist = eval_str_item env olist item in
        eval_prog new_env new_olist rest
    in
    match prog with
    | [] -> fail EmptyProgram
    | _ ->
      let* final_olist = eval_prog E.init [] prog in
      let deduplicated_olist = remove_duplicates final_olist in
      return deduplicated_olist
  ;;
end

module RESULT_MONAD_ERROR = struct
  (* Basic Result monad extension.
     Result is used for more advanced error handling *)
  include Result

  type ('a, 'e) t = ('a, 'e) result

  let return x = Ok x
  let fail e = Error e

  let ( >>= ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;

  let ( let* ) = ( >>= )
end

(* Interpreter functor extension *)
module InterpreterWResult = Interpreter (RESULT_MONAD_ERROR)

let run_interpreter = InterpreterWResult.interpret_program

module PPrinter = struct
  open Stdlib.Format

  let rec pp_value fmt = function
    | VInt i -> fprintf fmt "%i" i
    | VChar c -> fprintf fmt "'%c'" c
    | VString s -> fprintf fmt "%S" s
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
    (* Recursively format list elements *)
    | VConstruct ("::", Some (VTuple (head, tail, []))) ->
      let rec extract_list acc = function
        | VConstruct ("::", Some (VTuple (hd, tl, []))) -> extract_list (hd :: acc) tl
        | VConstruct ("[]", None) -> List.rev acc
        | v -> List.rev (v :: acc)
      in
      let elements = extract_list [ head ] tail in
      fprintf
        fmt
        "[%a]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_value)
        elements
    | VAdt (_, _, name, _) -> fprintf fmt "<ADT>: %s" name
    | VConstruct ("[]", _) -> fprintf fmt "[]"
    | VConstruct (ct, Some v) ->
      fprintf fmt "%s " ct;
      pp_value fmt v
    | VConstruct (ct, None) -> fprintf fmt "%s" ct
    | VUnit -> fprintf fmt "unit"
    | _ -> fprintf fmt "Intepreter error: Value error"
  ;;

  let pp_error fmt = function
    | PatternMismatch -> fprintf fmt "Interpreter error: Pattern mismatch"
    | DivisionByZero -> fprintf fmt "Interpreter error: Division by zero"
    | UnboundVariable s -> fprintf fmt "Interpreter error: Unbound value %s" s
    | TypeMismatch -> fprintf fmt "Interpreter error: Type mismatch"
    | RecursionError -> fprintf fmt "Interpreter error: Recursion error"
    | EmptyProgram -> fprintf fmt "Empty program"
    | ParserError -> fprintf fmt "Parser Error"
    | NotAnADT s -> fprintf fmt "Interpreter error: %s is not an ADT" s
    | NotAnADTVariant s -> fprintf fmt "Interpreter error: %s is not an ADT's variant" s
    | UndefinedConstructor s ->
      fprintf fmt "Interpreter error: Undefined constructor %s" s
    | UndefinedArgs -> fprintf fmt "InterpreterError: Undefined arguments"
  ;;

  let print_error = printf "%a" pp_error
end
