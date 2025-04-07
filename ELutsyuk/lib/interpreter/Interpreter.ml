(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open IntAuxilary.EvalEnv
open IntAuxilary.Res
open Forest.ValuesTree
open Forest.Ast

let rec match_pattern env = function
  | PatAny, _ -> Some env
  | PatConst _, _ -> Some env
  | PatList pats, ValList vals -> match_list_pat env pats vals
  | PatTup (p1, p2, ps), ValTup (v1, v2, vs) ->
    match_list_pat env (p1 :: p2 :: ps) (v1 :: v2 :: vs)
  | PatListCons (p1, p2), ValList (v1 :: v2) ->
    (match match_pattern env (p1, v1) with
     | Some env1 -> match_pattern env1 (p2, ValList v2)
     | None -> None)
  | _ -> None

and match_list_pat env pats vals =
  let helper acc p v =
    match acc with
    | None -> None
    | Some env -> match_pattern env (p, v)
  in
  match Base.List.fold2 pats vals ~f:helper ~init:(Some env) with
  | Ok res -> res
  | _ -> None
;;
