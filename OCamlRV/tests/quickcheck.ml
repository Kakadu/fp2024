(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Ast
open OCamlRV_lib.Parser
open OCamlRV_lib.Pprintast
(* open QCheck *)
(*
   let rec shrink_pattern = function
   | PVar _ -> Iter.empty
   | PAny -> Iter.empty
   | PLiteral (IntLiteral _) -> Iter.return (PLiteral (IntLiteral 1))
   | PLiteral (BoolLiteral b) -> Iter.return (PLiteral (BoolLiteral b))
   | PLiteral _ -> Iter.empty
   | PCons ls ->
   Iter.(
   let shrink_list_length =
   map (fun ls' -> PList ls') (QCheck.Shrink.list ~shrink:shrink_pattern ls)
   in
   let shrink_elements =
   List.fold_right
   (fun e acc ->
   map
   (fun e' -> PList (List.map2 (fun x y -> if x = e then e' else y) ls ls))
   (shrink_pattern e)
   <+> acc)
   ls
   empty
   in
   shrink_list_length <+> shrink_elements)
   | PTuple (p1, p2, p3) ->
   Iter.(
   map (fun p1' -> PTuple (p1', p2, p3)) (shrink_pattern p1)
   <+> map (fun p2' -> PTuple (p1, p2', p3)) (shrink_pattern p2)
   <+> map
   (fun p3' -> PTuple (p1, p2, p3'))
   (QCheck.Shrink.list ~shrink:shrink_pattern p3))
   ;; *)

(*
   (* Shrink lists by removing elements *)
   let shrink_list shrink_elem lst =
   let remove_elements =
   QCheck.Iter.(
   0 -- (List.length lst - 1) >|= fun i ->
   List.filteri (fun j _ -> i <> j) lst)
   in
   let shrink_individual =
   QCheck.Iter.(
   of_list lst >>= fun x ->
   shrink_elem x >>= fun x' ->
   [ x' ])
   in
   QCheck.Iter.(remove_elements <+> shrink_individual)

   (* Shrink an option by removing it *)
   let shrink_option shrink_elem = function
   | None -> QCheck.Iter.empty
   | Some x ->
   QCheck.Iter.(
   return None <+> (shrink_elem x >|= fun x' -> Some x'))

   let shrink_literal = function
   | IntLiteral n -> Iter.map (fun n' -> IntLiteral n') (Shrink.int n)
   | BoolLiteral _ -> Iter.empty
   | StringLiteral s -> Iter.map (fun s' -> StringLiteral s') (Shrink.string s)
   | UnitLiteral -> Iter.empty
   | NilLiteral -> Iter.empty

   let rec shrink_pattern = function
   | PAny -> Iter.empty
   | PLiteral lit -> Iter.map (fun lit' -> PLiteral lit') (shrink_literal lit)
   | PVar _ -> Iter.empty
   | PCons (p1, p2) ->
   Iter.(shrink_pattern p1 >|= fun p1' -> PCons (p1', p2))
   <+> Iter.(shrink_pattern p2 >|= fun p2' -> PCons (p1, p2'))
   | PTuple (p1, p2, ps) ->
   Iter.(shrink_pattern p1 >|= fun p1' -> PTuple (p1', p2, ps))
   <+> Iter.(shrink_pattern p2 >|= fun p2' -> PTuple (p1, p2', ps))
   <+> shrink_list shrink_pattern ps
   | POption opt -> shrink_option shrink_pattern opt
   | PType (p, _) -> Iter.map (fun p' -> PType (p', AInt)) (shrink_pattern p)

   let rec shrink_expression = function
   | ExprVariable _ -> Iter.empty
   | ExprLiteral lit -> Iter.map (fun lit' -> ExprLiteral lit') (shrink_literal lit)
   | ExprBinOperation (op, e1, e2) ->
   Iter.(shrink_expression e1 >|= fun e1' -> ExprBinOperation (op, e1', e2))
   <+> Iter.(shrink_expression e2 >|= fun e2' -> ExprBinOperation (op, e1, e2'))
   | ExprUnOperation (op, e) ->
   Iter.map (fun e' -> ExprUnOperation (op, e')) (shrink_expression e)
   | ExprIf (e1, e2, opt) ->
   Iter.(shrink_expression e1 >|= fun e1' -> ExprIf (e1', e2, opt))
   <+> Iter.(shrink_expression e2 >|= fun e2' -> ExprIf (e1, e2', opt))
   <+> shrink_option shrink_expression opt
   | ExprMatch (e, cases) ->
   Iter.(shrink_expression e >|= fun e' -> ExprMatch (e', cases))
   <+> shrink_list shrink_case cases
   | ExprLet (_, bindings, e) ->
   Iter.(shrink_expression e >|= fun e' -> ExprLet (NonRec, bindings, e'))
   <+> shrink_list shrink_binding bindings
   | ExprApply (e1, e2) ->
   Iter.(shrink_expression e1 >|= fun e1' -> ExprApply (e1', e2))
   <+> Iter.(shrink_expression e2 >|= fun e2' -> ExprApply (e1, e2'))
   | ExprTuple (e1, e2, es) ->
   Iter.(shrink_expression e1 >|= fun e1' -> ExprTuple (e1', e2, es))
   <+> Iter.(shrink_expression e2 >|= fun e2' -> ExprTuple (e1, e2', es))
   <+> shrink_list shrink_expression es
   | ExprCons (e1, e2) ->
   Iter.(shrink_expression e1 >|= fun e1' -> ExprCons (e1', e2))
   <+> Iter.(shrink_expression e2 >|= fun e2' -> ExprCons (e1, e2'))
   | ExprFun (p, e) ->
   Iter.(shrink_pattern p >|= fun p' -> ExprFun (p', e))
   <+> Iter.(shrink_expression e >|= fun e' -> ExprFun (p, e'))
   | ExprOption opt -> shrink_option shrink_expression opt

   and shrink_case (p, e) =
   Iter.(shrink_pattern p >|= fun p' -> (p', e))
   <+> Iter.(shrink_expression e >|= fun e' -> (p, e'))

   and shrink_binding (p, e) =
   Iter.(shrink_pattern p >|= fun p' -> (p', e))
   <+> Iter.(shrink_expression e >|= fun e' -> (p, e'))
*)

let arbitrary_structure =
  QCheck.make gen_structure ~print:(Format.asprintf "%a" pp_structure_item_list)
;;

let prop_round_trip =
  QCheck.Test.make ~count:1 arbitrary_structure (fun s ->
    Stdlib.Format.printf "%s\n" (show_structure s);
    Result.ok s = parse (Format.asprintf "%a" pp_structure_item_list s))
;;

QCheck_base_runner.set_seed 52987003;;
QCheck_base_runner.run_tests [ prop_round_trip ]
