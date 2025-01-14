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
end
