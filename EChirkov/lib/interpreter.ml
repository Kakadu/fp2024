(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

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
  | VFun of pattern * expression * environment
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

  let eval_structure_item (env : environment) = function
    | SValue (Nonrecursive, b, bl) ->
      eval_non_rec_bs env (b :: bl)
    | SValue (Recursive, b, bl) ->
      eval_rec_bs env (b :: bl)
  ;;

  let eval_structure_item (p : program) =
    List.fold_left
      ~f:(fun env str_item ->
        let* env = env in
        let* env = eval_str_item env str_item in
        return env)
      ~init:(return empty)
      p
  ;;
end

let initial_environment = Environment.extend Environment.empty "print_int" (VBuiltin (BPrintInt))
let interpret_structure_item = Evaluate.eval_structure_item initial_environment

let interpret s = 
