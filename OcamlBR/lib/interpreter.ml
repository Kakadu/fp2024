(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Values
open Ast

module type MONAD = sig
  (* a basic monad that has type of state * result *)
  include Base.Monad.S2

  (* error-handling *)
  val fail : error -> ('a, error) t

  (* for readability *)
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

(* utility layer for handling environments, i.e. symbol tables *)
module Env (M : MONAD) = struct
  (* for error propagation *)
  open M

  (* creates an empty environment *)
  let empty = Base.Map.empty (module Base.String)

  (* looks up a variable in the environment *)
  let find env name =
    match Base.Map.find env name with
    | Some x -> return x
    | None -> fail (`Unbound_variable name)
  ;;

  (* adds or updates a binding in the environment *)
  let extend env key value = Base.Map.update env key ~f:(fun _ -> value)

  (* composes two envs: if they have overlapping values, the latter is chosen *)
  let compose env1 env2 =
    Base.Map.fold env2 ~init:env1 ~f:(fun ~key ~data acc_env -> extend acc_env key data)
  ;;
end

module Eval (M : MONAD) : sig end = struct
  open M
  open Env (M)

  let rec match_pattern env = function
    | PAny, _ -> Some env
    | PConst (Int i1), VInt i2 when i1 = i2 -> Some env
    | PConst (Bool b1), VBool b2 when Bool.equal b1 b2 -> Some env
    | PConst (String s1), VString s2 when String.equal s1 s2 -> Some env
    | PConst Unit, VUnit -> Some env
    | PVar (Id name), v -> Some (extend env name v)
    | PList patterns, VList values -> match_list_pattern env patterns values
    | PTuple (p1, p2, p_rest), VTuple (v1, v2, v_rest) ->
      match_list_pattern env (p1 :: p2 :: p_rest) (v1 :: v2 :: v_rest)
    | PCons (p1, p2), VList (v1 :: v2) ->
      (match match_pattern env (p1, v1) with
       | Some env' -> match_pattern env' (p2, VList v2)
       | None -> None)
    | POption p, VOption v ->
      (match p, v with
       | Some p, Some v -> match_pattern env (p, v)
       | None, None -> Some env
       | _ -> None)
    | _ -> None

  and match_list_pattern env patterns values =
    (* to avoid Invalid argument exception *)
    if List.length patterns <> List.length values
    then None
    else (
      let f1 acc p v =
        match acc with
        | None -> None
        | Some env' -> match_pattern env' (p, v)
      in
      List.fold_left2 f1 (Some env) patterns values)
  ;;

  let print_env env =
    let open Stdlib.Format in
    printf "{\n";
    Base.Map.iteri env ~f:(fun ~key ~data -> printf "%s = %a\n" key pp_value data);
    printf "}\n"
  ;;

  let eval_un_op = function
    | Negative, VInt i -> return (VInt (-i))
    | Positive, VInt i -> return (VInt i)
    | Not, VBool b -> return (VBool (not b))
    | _ ->
      Format.printf "ty err8\n";
      fail `Type_error
  ;;

  let rec eval_bin_op = function
    | Add, VInt i1, VInt i2 -> return (VInt (i1 + i2))
    | Mult, VInt i1, VInt i2 -> return (VInt (i1 * i2))
    | Sub, VInt i1, VInt i2 -> return (VInt (i1 - i2))
    | Div, VInt _, VInt i2 when i2 = 0 -> fail `Division_by_zero
    | Div, VInt i1, VInt i2 -> return (VInt (i1 / i2))
    | Cons, v, VList vl -> return (VList (v :: vl))
    | Gt, VInt i1, VInt i2 -> return (VBool (i1 > i2))
    | Lt, VInt i1, VInt i2 -> return (VBool (i1 < i2))
    | Gte, VInt i1, VInt i2 -> return (VBool (i1 >= i2))
    | Lte, VInt i1, VInt i2 -> return (VBool (i1 <= i2))
    | And, VBool b1, VBool b2 -> return (VBool (b1 && b2))
    | Or, VBool b1, VBool b2 -> return (VBool (b1 || b2))
    | Eq, VInt i1, VInt i2 -> return (VBool (i1 = i2))
    | Neq, VInt i1, VInt i2 -> return (VBool (i1 <> i2))
    | Eq, VString s1, VString s2 -> return (VBool (s1 = s2))
    | Neq, VString s1, VString s2 -> return (VBool (s1 <> s2))
    | Eq, VBool b1, VBool b2 -> return (VBool (b1 = b2))
    | Neq, VBool b1, VBool b2 -> return (VBool (b1 <> b2))
    | Eq, VUnit, VUnit -> return (VBool true)
    | Neq, VUnit, VUnit -> return (VBool false)
    | Eq, VList l1, VList l2 -> eval_eq_list Eq l1 l2
    | Neq, VList l1, VList l2 -> eval_eq_list Neq l1 l2
    | Eq, VTuple (v1, v2, v_rest), VTuple (v1', v2', v_rest') ->
      eval_eq_list Eq (v1 :: v2 :: v_rest) (v1' :: v2' :: v_rest')
    | Neq, VTuple (v1, v2, v_rest), VTuple (v1', v2', v_rest') ->
      eval_eq_list Neq (v1 :: v2 :: v_rest) (v1' :: v2' :: v_rest')
    | Eq, VOption o1, VOption o2 ->
      (match o1, o2 with
       | Some o1, Some o2 -> eval_bin_op (Eq, o1, o2)
       | None, None -> return (VBool true)
       | _ -> return (VBool false))
    | Neq, VOption o1, VOption o2 ->
      (match o1, o2 with
       | Some o1, Some o2 -> eval_bin_op (Neq, o1, o2)
       | None, None -> return (VBool true)
       | _ -> return (VBool false))
    | _ ->
      Format.printf "ty err-1\n";
      fail `Type_error

  and eval_eq_list op l1 l2 =
    if List.length l1 <> List.length l2
    then return (VBool false)
    else (
      let f1 acc el1 el2 =
        let* acc = acc in
        match acc with
        | VBool false -> return (VBool false)
        | VBool true ->
          let* res = eval_bin_op (op, el1, el2) in
          (match res with
           | VBool true -> return (VBool true)
           | _ -> return (VBool false))
        | _ ->
          Format.printf "ty err1\n";
          fail `Type_error
      in
      List.fold_left2 f1 (return (VBool true)) l1 l2)
  ;;
end
