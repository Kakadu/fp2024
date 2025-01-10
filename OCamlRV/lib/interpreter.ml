(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type builtin =
  | BInt of (int -> unit)
  | BString of (string -> unit)

type environment = (string, value, Base.String.comparator_witness) Base.Map.t

and value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VTuple of value list
  | VNil
   | VNone
  | VSome of value
  | VFun of pattern * rec_flag * expression * environment
  | VBuiltin of pattern * rec_flag * builtin * environment

let vint i = VInt i
let vbool b = VBool b
let vstring s = VString s
let vunit = VUnit
let vnil = VNil
let vfun p rf e env = VFun (p, rf, e, env)

type error =
  | Pattern_matching_failed
  | Wrong_type of value
  | Unbound_variable
  | Evaluationg_Need_ToBeReplaced
  | BuiltinEvaluatingError

module type MONAD_FAIL = sig
  include Monad.S2

  val fail : error -> ('a, error) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | VInt x -> fprintf ppf "%d" x
  | VBool b -> fprintf ppf "%b" b
  | VString s -> fprintf ppf "%s" s
  | VUnit -> fprintf ppf "()"
  | VList vl ->
    fprintf
      ppf
      "[%a]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_value)
      vl
  | VTuple vl ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
      vl
  | VNil -> fprintf ppf "[]"
  | VFun _ -> fprintf ppf "<fun>"
  | VBuiltin _ -> fprintf ppf "<builtin>"
  | VNone | VSome _ -> fprintf ppf "<Option>"
;;

module Env (M : MONAD_FAIL) = struct
  open M

  let empty = Base.Map.empty (module String)
  let extend env k v = Base.Map.update env k ~f:(fun _ -> v)

  let find env name =
    match Base.Map.find env name with
    | Some x -> return x
    | None -> fail Unbound_variable
  ;;
end

module Eval (M : MONAD_FAIL) = struct
  open M
  open Env (M)

  let rec check_match env = function
    | PAny, _ -> Some env
    | PConstant (CInt i1), VInt i2 when i1 = i2 -> Some env
    | PConstant (CBool b1), VBool b2 when Bool.equal b1 b2 -> Some env
    | PConstant (CString s1), VString s2 when String.equal s1 s2 -> Some env
    | PConstant CNil, VList [] -> Some env
    | PVar x, v -> Some (extend env x v)
    | PTuple (p1, p2, pl), VTuple vl ->
      let env =
        List.fold2
          pl
          vl
          ~f:(fun env p v ->
            match env with
            | Some e -> check_match e (p, v)
            | None -> None)
          ~init:(Some env)
      in
      (match env with
       | Ok env -> env
       | _ -> None)
    | PCons (p1, p2), VList (v :: vl) ->
      let env = check_match env (p2, VList vl) in
      (match env with
       | Some env -> check_match env (p1, v)
       | None -> None)
    | _ -> None
  ;;

  let eval_binop (op, v1, v2) =
    match op, v1, v2 with
    | Add, VInt x, VInt y -> return (vint (x + y))
    | Sub, VInt x, VInt y -> return (vint (x - y))
    | Mul, VInt x, VInt y -> return (vint (x * y))
    | Div, VInt x, VInt y -> return (vint (x / y))
    | Lt, VInt x, VInt y -> return (vbool (x < y))
    | Gt, VInt x, VInt y -> return (vbool (x > y))
    | Eq, VInt x, VInt y -> return (vbool (x = y))
    | Neq, VInt x, VInt y -> return (vbool (x <> y))
    | Lte, VInt x, VInt y -> return (vbool (x <= y))
    | Gte, VInt x, VInt y -> return (vbool (x >= y))
    | And, VBool x, VBool y -> return (vbool (x && y))
    | Or, VBool x, VBool y -> return (vbool (x || y))
    | _ -> fail Evaluationg_Need_ToBeReplaced
  ;;

  let eval_expr =
    let rec helper (env : environment) = function
      | ExprConstant c ->
        (match c with
         | CInt i -> return (vint i)
         | CBool b -> return (vbool b)
         | CString s -> return (vstring s)
         | CUnit -> return vunit
         | CNil -> return vnil)
      | ExprVariable x ->
        let* v = find env x in
        let v =
          match v with
          | VFun (p, Rec, e, env) -> VFun (p, Rec, e, extend env x v)
          | VBuiltin (p, NonRec, f, env) -> VBuiltin (p, NonRec, f, extend env x v)
          | _ -> v
        in
        return v
      | ExprBinOperation (op, e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        eval_binop (op, v1, v2)
      | ExprIf (cond, t, Some f) ->
        let* cv = helper env cond in
        (match cv with
         | VBool true -> helper env t
         | VBool false -> helper env f
         | _ -> fail (Wrong_type cv))
      | ExprLet (NonRec, (PVar x, e1), [], e) ->
        let* v = helper env e1 in
        let env = extend env x v in
        helper env e
      | ExprLet (Rec, (PVar x, e1), [], e) ->
        let* v = helper env e1 in
        let env1 = extend env x v in
        let v =
          match v with
          | VFun (p, _, e, _) -> VFun (p, Rec, e, env1)
          | _ -> v
        in
        let env2 = extend env x v in
        helper env2 e
      | ExprFun (p, e) -> return (vfun p NonRec e env)
      | ExprApply (e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        (match v1 with
         | VFun (p, _, e, env) ->
           let* env' =
             match check_match env (p, v2) with
             | Some env -> return env
             | None -> fail Pattern_matching_failed
           in
           helper env' e
         | VBuiltin (_, _, b, _) ->
           let status =
             match b, v2 with
             | BInt b, VInt i ->
               b i;
               Ok VUnit
             | BString b, VString s ->
               b s;
               Ok VUnit
             | _, _ -> Error VUnit
           in
           (match status with
            | Ok _ -> return VUnit
            | Error _ -> fail (Wrong_type v1))
         | _ -> fail BuiltinEvaluatingError)
      | ExprTuple (e1, e2, el) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        let* vl =
          List.fold_left
            ~f:(fun acc e ->
              let* acc = acc in
              let* v = helper env e in
              return (v :: acc))
            ~init:(return [])
            el
        in
        return (VTuple (v1 :: v2 :: List.rev vl))
      | ExprList (hd, tl) ->
        let* vhd = helper env hd in
        let* vtl =
          List.fold_left
            ~f:(fun acc e ->
              let* acc = acc in
              let* v = helper env e in
              return (v :: acc))
            ~init:(return [])
            tl
        in
        return (VList (vhd :: List.rev vtl))
      | ExprCons (h, tl) ->
        let* hv = helper env h in
        let* tlv = helper env tl in
        (match tlv with
        | VList vl -> return (VList (hv :: vl))
        | _ -> fail (Wrong_type tlv))
      | ExprOption opt_expr ->
         (match opt_expr with
          | None -> return VNone
      | Some e ->
            let* v = helper env e in
        return (VSome v))

      | _ -> fail Evaluationg_Need_ToBeReplaced
    in
    helper
  ;;

  let eval_structure_item (env : environment) structure_item =
    let env =
      extend
        env
        "print_int"
        (VBuiltin (PConstant (CString "print_int"), NonRec, BInt print_int, env))
    in
    let env =
      extend
        env
        "print_endline"
        (VBuiltin (PConstant (CString "print_endline"), NonRec, BString print_endline, env))
    in
    match structure_item with
    | SEval e ->
      let* v = eval_expr env e in
      let env2 = extend env "-" v in
      return env2
    | SValue (NonRec, (PVar x, e), []) ->
      let* v = eval_expr env e in
      let env = extend env x v in
      return env
    | SValue (Rec, (PVar x, e), []) ->
      let* v = eval_expr env e in
      let env1 = extend env x v in
      let v =
        match v with
        | VFun (p, _, e, _) -> VFun (p, Rec, e, env1)
        | _ -> v
      in
      let env = extend env x v in
      return env
    | _ -> fail Evaluationg_Need_ToBeReplaced
  ;;

  let eval_structure (s : structure) =
    List.fold_left
      ~f:(fun env item ->
        let* env = env in
        let* env = eval_structure_item env item in
        return env)
      ~init:(return empty)
      s
  ;;
end

module Interpret = Eval (struct
    include Base.Result

    let ( let* ) m f = bind m ~f
  end)

let test_interpret s =
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    let _ = Interpret.eval_structure parsed in
    printf ""
  | Error e -> printf "Parsing error: %s\n" e
;;
