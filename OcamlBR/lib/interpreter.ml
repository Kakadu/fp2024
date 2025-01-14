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
