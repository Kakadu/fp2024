(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Base
open Forest.ValuesTree

module Res : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> ('a, error) Result.t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end = struct
  type 'a t = ('a, error) Result.t

  let return = Result.return
  let fail = Result.fail
  let run m = m

  let ( >>= ) monad func =
    match monad with
    | Ok res -> func res
    | Error err -> fail err
  ;;

  let ( let* ) = ( >>= )
end

module EvalEnv : sig
  type t = env

  val empty : t
  val extend : t -> string -> value -> t
  val compose : t -> t -> t
  val find_val : t -> string -> value Res.t
end = struct
  type t = env

  let empty = Map.empty (module String)
  let extend env id value = Map.update env id ~f:(fun _ -> value)

  let compose env1 env2 =
    Map.fold env2 ~f:(fun ~key ~data acc -> extend acc key data) ~init:env1
  ;;

  let find_val env id =
    match Map.find env id with
    | Some value -> Res.return value
    | None -> Res.fail (NoVariable id)
  ;;
end
