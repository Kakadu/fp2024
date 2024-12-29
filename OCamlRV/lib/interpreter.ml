(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

module type MONAD_FAIL = sig
  include Monad.S2

  val fail : string -> ('a, string) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VNil

let vint i = VInt i
let vbool b = VBool b
let vstring s = VString s
let vunit = VUnit
let vnil = VNil

let pp_value ppf =
  let open Stdlib.Format in
  function
  | VInt x -> fprintf ppf "%d" x
  | VBool b -> fprintf ppf "%b" b
  | VString s -> fprintf ppf "%s" s
  | VUnit -> fprintf ppf "()"
  | VNil -> fprintf ppf "[]"
;;

module Env (M : MONAD_FAIL) = struct
  type environment = (string, value, Base.String.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module String)

  let extend env k v = Base.Map.update env k ~f:(fun _ -> v)
end

module Eval (M : MONAD_FAIL) = struct
  open M
  open Env (M)

  let eval_binop (op, v1, v2) =
    match op, v1, v2 with
    | Mul, VInt x, VInt y -> return (vint (x * y))
    | Div, VInt x, VInt y -> return (vint (x / y))
    | Add, VInt x, VInt y -> return (vint (x + y))
    | Sub, VInt x, VInt y -> return (vint (x - y))
    | _ -> fail "error while evaluating binary operations (?need to be replaced?)"
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
      | ExprBinOperation (op, e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        eval_binop (op, v1, v2)
      | _ -> fail "error while evaluating expressions (?need to be replaced?)"
    in
    helper
  ;;

  let eval_structure_item (env : environment) = function
    | SEval e ->
      let* v = eval_expr env e in
      let env2 = extend env "-" v in
      return env2
    | _ -> fail "error while structure item (?need to be replaced?)"
  ;;

  let eval_structure (s : structure) =
    List.fold_left
      ~f:(fun env item ->
        let* env = env in
        let* env = eval_structure_item env item in
        let _ =
          Base.Map.iter
            ~f:(fun value ->
              Format.fprintf Format.std_formatter "- : some_type = %a\n" pp_value value)
            env
        in
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
    Stdlib.Format.printf "%s\n\n" (Ast.show_structure parsed);
    let _ = Interpret.eval_structure parsed in
    printf ""
  | Error e -> printf "Parsing error: %s\n" e
;;
