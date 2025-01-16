(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module EvalError = struct
  type error =
    [ `Division_by_zero
    | `Match_failure
    | `Type_mismatch
    | `Unbound_variable of string
    | `Args_after_not_variable_let
    | `Bound_several_times
    ]

  let pp_error fmt =
    let open Format in
    function
    | `Division_by_zero -> fprintf fmt "Division by zero\n"
    | `Match_failure -> fprintf fmt "Match failure\n"
    | `Type_mismatch ->
      fprintf fmt "Not possible scenario: type mismatch after type check\n"
    | `Unbound_variable name -> fprintf fmt "Unbound variable : %s\n" name
    | `Args_after_not_variable_let -> fprintf fmt "Args are allowed only after variable\n"
    | `Bound_several_times -> fprintf fmt "Variable is bound several times\n"
  ;;

  let bound_error : error = `Bound_several_times
end

module R : sig
  type 'a t
  type error = EvalError.error

  val pp_error : Format.formatter -> error -> unit
  val bound_error : error
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val ( <|> ) : 'a t -> 'a t -> 'a t
  val run : 'a t -> ('a, error) Result.t
end = struct
  open Base
  include Result
  include EvalError

  type 'a t = ('a, error) Result.t

  let ( <|> ) x y =
    match x with
    | Ok x -> Ok x
    | Error _ -> y
  ;;

  module Syntax = struct
    let ( let* ) = ( >>= )
    let ( let+ ) = ( >>| )
  end

  let run c = c
end

module ValueEnv : sig
  type t

  type value =
    | VUnit
    | VInt of int
    | VString of string
    | VBool of bool
    | VTuple of value * value * value list
    | VList of value list
    | VFun of string option * pattern * pattern list * expr * t
    | VFunction of case * case list
    | VOption of value option

  val find_err : t -> string -> value R.t
  val extend : t -> string -> value -> t
  val update_exn : t -> string -> f:(value -> value) -> t
  val remove : t -> string -> t
  val find_exn : t -> string -> value
  val pp_value : Format.formatter -> value -> unit
  val default : t
end = struct
  open Base

  type value =
    | VUnit
    | VInt of int
    | VString of string
    | VBool of bool
    | VTuple of value * value * value list
    | VList of value list
    | VFun of string option * pattern * pattern list * expr * t
    | VFunction of case * case list
    | VOption of value option

  and t = (string, value, Base.String.comparator_witness) Base.Map.t

  let rec pp_value fmt =
    let open Format in
    function
    | VUnit -> fprintf fmt "() "
    | VInt i -> fprintf fmt "%d " i
    | VString s -> fprintf fmt "%S " s
    | VBool b -> fprintf fmt "%a " pp_print_bool b
    | VTuple (fst, snd, rest) ->
      fprintf
        fmt
        "(%a) "
        (pp_print_list pp_value ~pp_sep:(fun fmt () -> fprintf fmt ", "))
        (fst :: snd :: rest)
    | VList l ->
      fprintf
        fmt
        "[%a] "
        (pp_print_list pp_value ~pp_sep:(fun fmt () -> fprintf fmt "; "))
        l
    | VFun (_, _, _, _, _) -> fprintf fmt "<fun> "
    | VFunction (_, _) -> fprintf fmt "<fun> "
    | VOption (Some v) -> fprintf fmt "Some %a " pp_value v
    | VOption None -> fprintf fmt "None "
  ;;

  let extend mp key data = Map.update mp key ~f:(function _ -> data)

  let update_exn env k ~f =
    let v = Map.find_exn env k in
    extend env k (f v)
  ;;

  let remove = Map.remove

  let find_err env name =
    let open R in
    match Map.find env name with
    | Some v -> return v
    | None ->
      let x = fail (`Unbound_variable name) in
      x
  ;;

  let default = Map.empty (module String)
  let find_exn = Map.find_exn
end

open R
open R.Syntax
open ValueEnv
module ExtractIdents = ExtractIdents.Make (R)
open ExtractIdents

let rec match_pattern env =
  let match_pattern_list env pl vl =
    Base.List.fold2_exn
      ~init:(return env)
      ~f:(fun acc pat value ->
        let* acc = acc in
        match_pattern acc (pat, value))
      pl
      vl
  in
  function
  | Wild, _ -> return env
  | PList pl, VList vl when List.length pl = List.length vl ->
    match_pattern_list env pl vl
  | PCons (hd, tl), VList (vhd :: vtl) ->
    match_pattern_list env [ hd; tl ] [ vhd; VList vtl ]
  | PTuple (pfst, psnd, prest), VTuple (vfst, vsnd, vrest) ->
    match_pattern_list env (pfst :: psnd :: prest) (vfst :: vsnd :: vrest)
  | PConst (Int_lt p), VInt v when p = v -> return env
  | PConst (Bool_lt p), VBool v when p = v -> return env
  | PConst (String_lt p), VString v when p = v -> return env
  | PConst Unit_lt, VUnit -> return env
  | PVar (Ident name), v -> return (ValueEnv.extend env name v)
  | POption (Some p), VOption (Some v) -> match_pattern env (p, v)
  | POption None, VOption None -> return env
  | PConstraint (p, _), v -> match_pattern env (p, v)
  | _ -> fail `Match_failure
;;

let rec eval_binequal =
  let eval_binequal_list l1 l2 =
    Base.List.fold2_exn
      ~init:(return (VBool true))
      ~f:(fun acc left right ->
        let* acc = acc in
        let* eq_res = eval_binequal (left, right) in
        match acc, eq_res with
        | VBool b1, VBool b2 -> return (VBool (b1 && b2))
        | _ -> fail `Type_mismatch)
      l1
      l2
  in
  function
  | VInt i1, VInt i2 -> return (VBool (i1 = i2))
  | VString s1, VString s2 -> return (VBool (s1 = s2))
  | VBool b1, VBool b2 -> return (VBool (b1 = b2))
  | VUnit, VUnit -> return (VBool true)
  | VList l1, VList l2 when List.length l1 = List.length l2 -> eval_binequal_list l1 l2
  | VTuple (fst1, snd1, rest1), VTuple (fst2, snd2, rest2)
    when List.length rest1 = List.length rest2 ->
    eval_binequal_list (fst1 :: snd1 :: rest1) (fst2 :: snd2 :: rest2)
  | _ -> fail `Type_mismatch
;;

let eval_binexpr op v1 v2 =
  match op, v1, v2 with
  | Binary_equal, v1, v2 -> eval_binequal (v1, v2)
  | Binary_unequal, v1, v2 ->
    let* b = eval_binequal (v1, v2) in
    (match b with
     | VBool b -> return (VBool (not b))
     | _ -> fail `Type_mismatch)
  | Binary_less, VInt v1, VInt v2 -> return (VBool (v1 < v2))
  | Binary_less_or_equal, VInt v1, VInt v2 -> return (VBool (v1 <= v2))
  | Binary_greater, VInt v1, VInt v2 -> return (VBool (v1 > v2))
  | Binary_greater_or_equal, VInt v1, VInt v2 -> return (VBool (v1 >= v2))
  | Binary_add, VInt v1, VInt v2 -> return (VInt (v1 + v2))
  | Binary_subtract, VInt v1, VInt v2 -> return (VInt (v1 - v2))
  | Binary_multiply, VInt v1, VInt v2 -> return (VInt (v1 * v2))
  | Binary_divide, VInt v1, VInt v2 -> return (VInt (v1 / v2))
  | Logical_or, VBool v1, VBool v2 -> return (VBool (v1 || v2))
  | Logical_and, VBool v1, VBool v2 -> return (VBool (v1 && v2))
  | Binary_or_bitwise, VInt v1, VInt v2 -> return (VInt (v1 lor v2))
  | Binary_xor_bitwise, VInt v1, VInt v2 -> return (VInt (v1 lxor v2))
  | Binary_and_bitwise, VInt v1, VInt v2 -> return (VInt (v1 land v2))
  | Binary_cons, v, VList vl -> return (VList (v :: vl))
  | _ -> fail `Type_mismatch
;;

let rec eval_expr env = function
  | Const c ->
    (match c with
     | Unit_lt -> return VUnit
     | Int_lt i -> return (VInt i)
     | String_lt s -> return (VString s)
     | Bool_lt b -> return (VBool b))
  | Tuple (fst, snd, rest) ->
    let* fst_value = eval_expr env fst in
    let* snd_value = eval_expr env snd in
    let* rest_values = eval_expr_fold env rest in
    return (VTuple (fst_value, snd_value, rest_values))
  | List l ->
    let* l_values = eval_expr_fold env l in
    return (VList l_values)
  | Variable (Ident n) -> find_err env n
  | Unary_expr (op, e) ->
    let* v = eval_expr env e in
    (match op, v with
     | Unary_minus, VInt i -> return (VInt (-i))
     | Unary_not, VBool b -> return (VBool (not b))
     | _ -> fail `Type_mismatch)
  | Bin_expr (op, e1, e2) ->
    let* v1 = eval_expr env e1 in
    let* v2 = eval_expr env e2 in
    eval_binexpr op v1 v2
  | If_then_else (c, t, e) ->
    let* c_value = eval_expr env c in
    (match c_value with
     | VBool true -> eval_expr env t
     | VBool false ->
       (match e with
        | None -> return VUnit
        | Some e -> eval_expr env e)
     | _ -> fail `Type_mismatch)
  | Lambda (arg, args, body) -> return (VFun (None, arg, args, body, env))
  | Apply (f, applying_arg) ->
    let* f_value = eval_expr env f in
    let* applying_arg_value = eval_expr env applying_arg in
    (match f_value with
     | VFun (name, arg, args, body, env) ->
       let* env = match_pattern env (arg, applying_arg_value) in
       let env =
         match name with
         | Some name -> ValueEnv.extend env name f_value
         | None -> env
       in
       (match args with
        | [] -> eval_expr env body
        | arg1 :: args -> return (VFun (None, arg1, args, body, env)))
     | VFunction (c, cl) -> eval_match env applying_arg_value (c :: cl)
     | _ -> fail `Type_mismatch)
  | Match (e, c, cl) ->
    let* v = eval_expr env e in
    eval_match env v (c :: cl)
  | Function (c, cl) -> return (VFunction (c, cl))
  | LetIn (rec_flag, let_bind, let_binds, e) ->
    let* env = extend_env_with_let_binds env rec_flag (let_bind :: let_binds) in
    let* value = eval_expr env e in
    return value
  | Option None -> return (VOption None)
  | Option (Some e) ->
    let* value = eval_expr env e in
    return (VOption (Some value))
  | EConstraint (e, _) -> eval_expr env e

and eval_expr_fold env l =
  Base.List.fold
    ~init:(return [])
    ~f:(fun acc e ->
      let* acc = acc in
      let* e_value = eval_expr env e in
      return (e_value :: acc))
    l

and eval_match env v = function
  | [] -> fail `Match_failure
  | (pat, expr) :: tl ->
    (let* ext_env = match_pattern env (pat, v) in
     eval_expr ext_env expr)
    <|> eval_match env v tl

and extend_env_with_let_bind env rec_flag = function
  | Let_bind (name, args, body) ->
    (match args with
     | arg1 :: args ->
       (match name with
        | PVar (Ident n) ->
          let value =
            match rec_flag with
            | Rec -> VFun (Some n, arg1, args, body, env)
            | Nonrec -> VFun (None, arg1, args, body, env)
          in
          let* env = match_pattern env (name, value) in
          return (Some n, env)
        | _ -> fail `Args_after_not_variable_let)
     | [] ->
       let n =
         match rec_flag, name with
         | Rec, PVar (Ident n) -> Some n
         | _ -> None
       in
       let* value = eval_expr env body in
       let* env = match_pattern env (name, value) in
       return (n, env))

and extend_env_with_let_binds env rec_flag let_binds =
  let* names, env =
    Base.List.fold
      ~init:(return ([], env))
      ~f:(fun acc let_bind ->
        let* names, env = acc in
        let* name, env = extend_env_with_let_bind env rec_flag let_bind in
        match name with
        | Some name -> return (name :: names, env)
        | None -> return (names, env))
      let_binds
  in
  let env =
    match rec_flag with
    | Rec ->
      Base.List.fold
        ~init:env
        ~f:(fun env name ->
          ValueEnv.update_exn env name ~f:(function
            | VFun (n, arg, args, body, _) -> VFun (n, arg, args, body, env)
            | other -> other))
        names
    | Nonrec -> env
  in
  return env
;;

let eval_statement env =
  let open Base in
  function
  | Let (rec_flag, let_bind, let_binds) ->
    let let_binds = let_bind :: let_binds in
    let* env = extend_env_with_let_binds env rec_flag let_binds in
    let* bind_names = extract_bind_names_from_let_binds let_binds >>| elements in
    let bind_names_with_values =
      List.fold
        bind_names
        ~init:(Map.empty (module String))
        ~f:(fun map name ->
          let value = ValueEnv.find_exn env name in
          Map.set map ~key:name ~data:value)
    in
    return (env, bind_names_with_values)
;;

let eval_construction env = function
  | Expr e ->
    let* value = eval_expr env e in
    return (env, Base.Map.singleton (module Base.String) "-" value)
  | Statement s -> eval_statement env s
;;

let eval env c = run (eval_construction env c)

type global_error =
  [ error
  | Inferencer.error
  ]

let pp_global_error fmt (e : global_error) =
  match e with
  | #error as e -> pp_error fmt e
  | #Inferencer.error as e -> Inferencer.pp_error fmt e
;;

let run_interpreter type_env value_env state c =
  let open Base in
  let new_state, infer_result = Inferencer.infer c type_env state in
  match infer_result with
  | Error (#Inferencer.error as type_err) -> new_state, Result.fail type_err
  | Ok (new_type_env, names_and_types) ->
    (match eval value_env c with
     | Error (#error as eval_err) -> new_state, Result.fail eval_err
     | Ok (new_value_env, names_and_values) ->
       let names_with_types_and_values =
         Map.fold
           names_and_types
           ~init:(Map.empty (module String))
           ~f:(fun ~key ~data map ->
             let value = Map.find_exn names_and_values key in
             Map.set map ~key ~data:(data, value))
       in
       new_state, Result.return (new_type_env, new_value_env, names_with_types_and_values))
;;
