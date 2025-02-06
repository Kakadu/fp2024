(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | DivisionByZero
  | TypeMismatch
  | UnboundVariable of string
  | PatternMismatch
  | RecursionError
  | NotImplemented
  | EmptyProgram
  | ParserError
  | NotAnADT of string
  | NotAnADTVariant of string
  | UndefinedConstructor of string
  | InvalidConstructorArguments of string

type value =
  | VInt of int
  | VString of string
  | VChar of char
  | VBool of bool
  | VTuple of value List2.t
  | VFun of Pattern.t List1.t * Expression.t * environment * Expression.rec_flag
  | VFunction of Expression.t Expression.case List1.t * environment
  | VConstruct of ident * value option
  | VAdt of (value * ident list * ident * (ident * TypeExpr.t list) List1.t)
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

  let builtin_types =
    Base.Map.of_alist_exn
      (module Base.String)
      [ "int", VType (TypeExpr.Type_var "int", None)
      ; "char", VType (TypeExpr.Type_var "char", None)
      ; "string", VType (TypeExpr.Type_var "string", None)
      ; "bool", VType (TypeExpr.Type_var "bool", None)
      ]
  ;;

  let builtin_functions =
    Base.Map.of_alist_exn
      (module Base.String)
      [ ( "print_endline"
        , VBuiltin_print
            (fun v ->
              match v with
              | VString s ->
                print_endline s;
                Ok VUnit
              | _ -> Error TypeMismatch) )
      ; ( "print_int"
        , VBuiltin_print
            (fun v ->
              match v with
              | VInt i ->
                print_endline (string_of_int i);
                Ok VUnit
              | _ -> Error TypeMismatch) )
      ; ( "print_char"
        , VBuiltin_print
            (fun v ->
              match v with
              | VChar c ->
                print_endline (String.make 1 c);
                Ok VUnit
              | _ -> Error TypeMismatch) )
      ; ( "print_bool"
        , VBuiltin_print
            (fun v ->
              match v with
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
    let merged_binops_and_functions =
      Base.Map.merge builtin_binops builtin_functions ~f:(fun ~key:_ ->
          function
          | `Left v -> Some v
          | `Right v -> Some v
          | `Both (v, _) -> Some v)
    in
    let merged_with_types =
      Base.Map.merge merged_binops_and_functions builtin_types ~f:(fun ~key:_ ->
          function
          | `Left v -> Some v
          | `Right v -> Some v
          | `Both (v, _) -> Some v)
    in
    Base.Map.merge
      merged_with_types
      (Base.Map.empty (module Base.String))
      ~f:(fun ~key:_ ->
        function
        | `Left v -> Some v
        | `Right v -> Some v
        | `Both (v, _) -> Some v)
  ;;

  let lookup env name =
    match Base.Map.find env name with
    | Some s -> return s
    | None -> fail (UnboundVariable name)
  ;;

  let extend env name value = Base.Map.set env ~key:name ~data:value

  let combine env1 env2 =
    Base.Map.fold env2 ~f:(fun ~key ~data env_acc -> extend env_acc key data) ~init:env1
  ;;

  (* Начинаем искать в окружении *)

  (* ADT-specified

     let type_definitions = Base.Map.empty (module Base.String)

     let add_type env type_name constructors =
     match Base.Map.find env type_definitions type_name with
     | Some _ -> fail (TypeAlreadyDefined type_name)
     | None -> return (Base.Map.set env ~key:type_name ~data:constructors)
     ;;

     let lookup_type env type_name =
     match Base.Map.find env type_definitions type_name with
     | Some constructors -> return constructors
     | None -> fail (UnboundType type_name)
     ;;
  *)
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

  let rec foldM f acc = function
    | [] -> return acc
    | x :: xs ->
      let* acc' = f acc x in
      foldM f acc' xs
  ;;

  let eval_const = function
    | Constant.Const_integer i -> return (VInt i)
    | Constant.Const_char c -> return (VChar c)
    | Constant.Const_string s -> return (VString s)
  ;;

  let rec eval_type_expr env = function
    | TypeExpr.Type_var ident ->
      let* as_value = E.lookup env ident in
      (match as_value with
       | VType (type_value, _) -> return type_value
       | _ -> failwith ("Unbound type variable: " ^ ident))
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
       | VType (TypeExpr.Type_construct (tn, tparams), _) ->
         if List.length args <> List.length tparams
         then
           failwith
             (Printf.sprintf
                "Type constructor %s expects %d arguments, but got %d"
                type_name
                (List.length tparams)
                (List.length args));
         let* resolved_args = mapM eval_type_expr env args in
         return (TypeExpr.Type_construct (type_name, resolved_args))
       | VType (TypeExpr.Type_var s, _) -> return (TypeExpr.Type_var s)
       | VAdt (_, tparams, _, _) ->
         if List.length args <> List.length tparams
         then
           failwith
             (Printf.sprintf
                "Type constructor %s expects %d arguments, but got %d"
                type_name
                (List.length tparams)
                (List.length args));
         let* resolved_args = mapM eval_type_expr env args in
         return (TypeExpr.Type_construct (type_name, resolved_args))
       | _ -> fail (UndefinedConstructor type_name))
  ;;

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
        | _ -> eval_pattern pat args env)
      else
        return None
        (*ya hz chto eto. 02.02.24 *)
        (*   | Pattern.Pat_tuple (p1, p2, ps), VAdt (_, args, type_name, constructors) ->
             let candidates =
             let first_ctor, rest_ctors = constructors in
             first_ctor :: rest_ctors
             in
             let matching_candidates =
             List.filter
             (fun (_, param_types) -> List.length param_types = List.length (p1 :: p2 :: ps))
             candidates
             in
             if matching_candidates = []
             then fail PatternMismatch
             else (
             let rec evaluate_candidates candidates =
             match candidates with
             | [] -> fail PatternMismatch
             | (ctor_name, param_types) :: rest_candidates ->
             let resolved_values =
             match
             mapM
             (fun param_type ->
             match param_type with
             | TypeExpr.Type_var tvar ->
             (match E.lookup env tvar with
             | VType (type_expr, Some adt_name) when adt_name = type_name ->
             Ok (VType (type_expr, Some adt_name))
             | Ok _ -> Error PatternMismatch
             | Error _ -> Error PatternMismatch)
             | _ -> Error NotImplemented)
             param_types
             with
             | Ok values -> values
             | Error _ -> fail PatternMismatch
             in
             (* Если удалось разрешить все типы, сравниваем с паттернами *)
             let patterns = p1 :: p2 :: ps in
             (match mapM2 eval_pattern env patterns resolved_values with
             | Ok env_opt -> return env_opt
             | Error _ -> evaluate_candidates rest_candidates)
             in
             evaluate_candidates matching_candidates) *)
    | Pattern.Pat_construct ("()", None), _ -> return (Some env)
    | Pattern.Pat_construct (ctor, None), VString s ->
      if String.equal ctor s then return (Some env) else fail PatternMismatch
    | Pattern.Pat_construct (cname, Some p), v ->
      (match v with
       | VAdt (_, _, tname, _) ->
         if String.equal cname tname then eval_pattern p v env else return None
       | VString s ->
         if String.equal cname s then eval_pattern p v env else fail PatternMismatch
       | VConstruct _ -> fail NotImplemented
       | VInt _ -> eval_pattern p v env
       | _ -> fail PatternMismatch)
    | Pattern.Pat_constraint (pat, _), v ->
      eval_pattern pat v env (* idk, haven't thought yet *)
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
            (* negative operator with 1 operand case *)
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
      let* env = eval_value_binding_list env (b1 :: bl) in
      eval_expr env body
    | Expression.Exp_let (Recursive, (b1, bl), body) ->
      (* Handle recursive bindings directly *)
      let* env = eval_rec_value_binding_list env (b1 :: bl) in
      eval_expr env body
    | Expression.Exp_construct (ctor_name, args) ->
      let* type_def = E.lookup env ctor_name in
      (match type_def with
       | VType (_, Some adt_name) ->
         let* adt_def = E.lookup env adt_name in
         (match adt_def with
          | VAdt (_, targs, _, constr) ->
            let c1, cl = constr in
            (* Searching for a constructor with ctor_name in the ADT definition *)
            (match Base.List.Assoc.find (c1 :: cl) ~equal:String.equal ctor_name with
             | Some ctor_arg_types ->
               (match args with
                | Some provided_args ->
                  if List.length ctor_arg_types != 0
                  then
                    let* evaluated_args = eval_expr env provided_args in
                    return (VAdt (evaluated_args, targs, ctor_name, constr))
                  else fail (InvalidConstructorArguments ctor_name)
                | None ->
                  (* If no arguments are provided, ensure the constructor expects none *)
                  if List.length ctor_arg_types = 0
                  then return (VAdt (VUnit, targs, ctor_name, constr))
                  else fail (InvalidConstructorArguments ctor_name))
             | None -> fail (UndefinedConstructor ctor_name))
          | _ -> fail (NotAnADT adt_name))
       | _ -> fail (NotAnADTVariant ctor_name))
    | Expression.Exp_constraint (expr, _type_expr) -> eval_expr env expr

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
      (* Add each constructor to the environment *)
      let c1, cl = constructors in
      let* neww_env =
        foldM
          (fun acc_env (ctor_name, param_types) ->
            let extended_env =
              List.fold_left
                (fun env tvar ->
                  E.extend env tvar (VType (TypeExpr.Type_var tvar, Some type_name)))
                acc_env
                targs
            in
            let* resolved_param_types = mapM eval_type_expr extended_env param_types in
            let adt_type =
              TypeExpr.Type_construct
                (type_name, List.map (fun t -> TypeExpr.Type_var t) targs)
            in
            let ctor_type =
              List.fold_right
                (fun param_type acc -> TypeExpr.Type_arrow (param_type, acc))
                resolved_param_types
                adt_type
            in
            (*  | VAdt of (value * ident list * ident * (ident * TypeExpr.t list) List1.t) *)
            (* let acc_env = E.extend acc_env ctor_name (VAdt ((Vtype ctor_type), targs, ctor_name, param_types)) *)
            return (E.extend acc_env ctor_name (VType (ctor_type, Some type_name))))
          new_env
          (c1 :: cl)
      in
      return (neww_env, olist)
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
    | VAdt (_, _, name, _) -> fprintf fmt "<ADT>: %s" name
    | _ -> fprintf fmt "Intepreter error: Value error"
  ;;

  let pp_error fmt = function
    | PatternMismatch -> fprintf fmt "Intepreter error: Pattern mismatch"
    | DivisionByZero -> fprintf fmt "Intepreter error: Division by zero"
    | NotImplemented -> fprintf fmt "Intepreter error: Not implemented"
    | UnboundVariable s -> fprintf fmt "Intepreter error: Unbound value %s" s
    | TypeMismatch -> fprintf fmt "Intepreter error: Type mismatch"
    | RecursionError -> fprintf fmt "Interpreter error: Recursion error"
    | EmptyProgram -> fprintf fmt "Empty program"
    | ParserError -> fprintf fmt "Parser Error"
    | NotAnADT s -> fprintf fmt "Interpreter error: %s is not an ADT" s
    | NotAnADTVariant s -> fprintf fmt "Interpreter error: %s is not an ADT's variant" s
    | InvalidConstructorArguments s ->
      fprintf fmt "Interpreter error: Invalid arguments at %s" s
    | UndefinedConstructor s ->
      fprintf fmt "Interpreter error: Undefined constructor %s" s
  ;;

  let print_error = printf "%a" pp_error
end
