(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

(* ========== errors ========== *)

type error =
  | UnboundVariable of string
  | TypeMissmatch
  | DivisionByZero

let pp_error = function
  | UnboundVariable s -> "Unbound variable: " ^ s
  | TypeMissmatch -> "Type error"
  | DivisionByZero -> "Division by zero"
;;

(* ========== values ========== *)

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VTuple of value * value * value list
  | VFun of rec_flag * pattern * expression * environment
  | VFunMutual of rec_flag * pattern * expression * environment
  | VList of value list
  | VOption of value option
  | VUnit
  | VPrintInt

and environment = (string, value, Base.String.comparator_witness) Base.Map.t

(* ========== result monad ========== *)

module type Monad = sig
  include Base.Monad.S2

  val fail : error -> ('a, error) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

(* ========== environment ========== *)

module Environment (M : Monad) = struct
  open M

  let empty = Base.Map.empty (module Base.String) (* create empty env *)

  let find env name =
    (* get from env by name *)
    match Base.Map.find env name with
    | Some x -> return x
    | None -> fail (UnboundVariable name)
  ;;

  (* put in env binding *)
  let extend env k v = Base.Map.update env k ~f:(fun _ -> v)
end

(* ========== evaluation ========== *)

module Evaluate (M : Monad) = struct
  open M
  open Environment (M)

  let rec match_pattern env = function
    | PAny, _ -> Some env
    | PConst (CInt i1), VInt i2 when i1 = i2 -> Some env
    | PConst (CBool b1), VBool b2 when Bool.equal b1 b2 -> Some env
    | PConst CUnit, VUnit -> Some env
    | PVar x, v -> Some (extend env x v)
    | PTuple (p1, p2, prest), VTuple (v1, v2, vrest) ->
      match_pattern_list env (p1 :: p2 :: prest) (v1 :: v2 :: vrest)
    | PList pl, VList vl -> match_pattern_list env pl vl
    | _ -> None

  and match_pattern_list env patterns values =
    if List.length patterns <> List.length values
    then None
    else (
      let f1 acc (p, v) =
        match acc with
        | None -> None
        | Some env' -> match_pattern env' (p, v)
      in
      let rec zip l1 l2 =
        match l1, l2 with
        | [], [] -> []
        | x :: xs, y :: ys -> (x, y) :: zip xs ys
        | _ -> failwith ""
      in
      List.fold_left ~f:f1 ~init:(Some env) (zip patterns values))
  ;;

  let rec eval_expression env = function
    | EConst c ->
      (match c with
       | CInt i -> return (VInt i)
       | CBool b -> return (VBool b)
       | CUnit -> return VUnit)
    | EVar x ->
      let* v = find env x in
      let v =
        match v with
        | VFun (Recursive, p, e, env) -> VFun (Recursive, p, e, extend env x v)
        | _ -> v
      in
      return v
    | EBinary (op, e1, e2) ->
      let* v1 = eval_expression env e1 in
      let* v2 = eval_expression env e2 in
      (match op, v1, v2 with
       | Add, VInt x, VInt y -> return (VInt (x + y))
       | Sub, VInt x, VInt y -> return (VInt (x - y))
       | Mul, VInt x, VInt y -> return (VInt (x * y))
       | Div, VInt x, VInt y ->
         (match y with
          | 0 -> fail DivisionByZero
          | _ -> return (VInt (x / y)))
       | Lt, VInt x, VInt y -> return (VBool (x < y))
       | Gt, VInt x, VInt y -> return (VBool (x > y))
       | Eq, VList x, VList y ->
         let rec eq_list lst1 lst2 =
           match lst1, lst2 with
           | [], [] -> true
           | VInt a :: t1, VInt b :: t2 -> a = b && eq_list t1 t2
           | VBool a :: t1, VBool b :: t2 ->
             ((a && b) || ((not a) && not b)) && eq_list t1 t2
           | VList a :: t1, VList b :: t2 -> eq_list a b && eq_list t1 t2
           | _ -> false
         in
         return (VBool (eq_list x y))
       | Eq, VInt x, VInt y -> return (VBool (x = y))
       | NEq, VInt x, VInt y -> return (VBool (x <> y))
       | Lte, VInt x, VInt y -> return (VBool (x <= y))
       | Gte, VInt x, VInt y -> return (VBool (x >= y))
       | And, VBool x, VBool y -> return (VBool (x && y))
       | Or, VBool x, VBool y -> return (VBool (x || y))
       | _ -> fail TypeMissmatch)
    | EUnary (op, e) ->
      let* v = eval_expression env e in
      (match op, v with
       | Pos, VInt x -> return (VInt (+x))
       | Neg, VInt x -> return (VInt (-x))
       | _ -> fail TypeMissmatch)
    | EIf (i, t, e) ->
      let* cv = eval_expression env i in
      (match cv with
       | VBool true -> eval_expression env t
       | VBool false ->
         (match e with
          | Some e -> eval_expression env e
          | None -> return VUnit)
       | _ -> fail TypeMissmatch)
    | ELet (Nonrecursive, b, bl, e) ->
      let bindings = b :: bl in
      let* env2 = eval_nonrec_bs env bindings in
      eval_expression env2 e
    | ELet (Recursive, (PVar x, e1), [], e) ->
      let* v = eval_expression env e1 in
      let env1 = extend env x v in
      let v =
        match v with
        | VFun (_, p, e, _) -> VFun (Recursive, p, e, env1)
        | _ -> v
      in
      let env2 = extend env x v in
      eval_expression env2 e
    | ELet (Recursive, b, bl, e) ->
      let bindings = b :: bl in
      let* env2 = eval_rec_bs env bindings in
      eval_expression env2 e
    | EFun (p, e) -> return (VFun (Nonrecursive, p, e, env))
    | EApply (e1, e2) ->
      let* v1 = eval_expression env e1 in
      let* v2 = eval_expression env e2 in
      (match v1 with
       | VFun (_, p, e, env) ->
         let* env' =
           match match_pattern env (p, v2) with
           | Some env -> return env
           | None -> fail TypeMissmatch
         in
         eval_expression env' e
       | VFunMutual (_, p, e, _) ->
         let* env' =
           match match_pattern env (p, v2) with
           | Some env -> return env
           | None -> fail TypeMissmatch
         in
         eval_expression env' e
       | VPrintInt ->
         (match v2 with
          | VInt i ->
            print_int i;
            return VUnit
          | _ -> fail TypeMissmatch)
       | _ -> fail TypeMissmatch)
    | ETuple (e1, e2, el) ->
      let* v1 = eval_expression env e1 in
      let* v2 = eval_expression env e2 in
      let* vl =
        Base.List.fold_left
          ~f:(fun acc e ->
            let* acc = acc in
            let* v = eval_expression env e in
            return (v :: acc))
          ~init:(return [])
          el
      in
      return (VTuple (v1, v2, List.rev vl))
    | EList el ->
      let rec eval_list_elements env = function
        | [] -> return []
        | e :: es ->
          let* v = eval_expression env e in
          let* vs = eval_list_elements env es in
          return (v :: vs)
      in
      let* vl = eval_list_elements env el in
      return (VList vl)
    | EOption opt_expr ->
      (match opt_expr with
       | None -> return (VOption None)
       | Some e ->
         let* v = eval_expression env e in
         return (VOption (Some v)))
    | EType (e, _) -> eval_expression env e

  and eval_rec_bs env bl =
    let* env2 =
      Base.List.fold_left
        ~f:(fun env b ->
          let* env = env in
          let p, e = b in
          match p with
          | PVar name ->
            (match e with
             | EFun (p1, e1) ->
               return (extend env name (VFunMutual (Recursive, p1, e1, env)))
             | _ -> return env)
          | _ -> return env)
        ~init:(return env)
        bl
    in
    return env2

  and eval_nonrec_bs env bl =
    let* env2 =
      List.fold_left
        ~f:(fun env b ->
          let* env = env in
          let p, e = b in
          match p with
          | PVar name ->
            let* v = eval_expression env e in
            return (extend env name v)
          | PType (PVar name, _) ->
            let* v = eval_expression env e in
            return (extend env name v)
          | PAny ->
            let* _ = eval_expression env e in
            return env
          | PConst CUnit ->
            let* _ = eval_expression env e in
            return env
          | PTuple (p1, p2, pl) -> return env (* TODO *)
          | _ -> return env)
        ~init:(return env)
        bl
    in
    return env2
  ;;

  let eval_structure_item env s =
    let env = extend env "print_int" VPrintInt in
    match s with
    | SValue (Nonrecursive, b, bl) -> eval_nonrec_bs env (b :: bl)
    | SValue (Recursive, b, bl) -> eval_rec_bs env (b :: bl)
  ;;

  let eval_program (p : program) =
    List.fold_left
      ~f:(fun env structure_item ->
        let* env = env in
        let* env = eval_structure_item env structure_item in
        return env)
      ~init:(return empty)
      p
  ;;
end

module Interpret = Evaluate (struct
    include Result

    let ( let* ) m f = bind m ~f
  end)

let interpret = Interpret.eval_program
