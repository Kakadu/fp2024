(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Ast
open Pr_printer
open Parser

let rec shrink_pattern = function
  | PVar _ -> Iter.empty
  | PAny -> Iter.empty
  | PConst (Int _) -> Iter.return (PConst (Int 1))
  | PConst (Bool b) -> Iter.return (PConst (Bool b))
  | PConst _ -> Iter.empty
  | PList ls ->
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
;;

let rec shrink_expr = function
  | Econst (Int _) -> Iter.return (Econst (Int 1))
  | Econst (Bool b) -> Iter.return (Econst (Bool b))
  | Econst _ -> Iter.empty
  | Ebin_op (op, e1, e2) ->
    Iter.(
      return e1
      <+> return e2
      <+> map (fun e1' -> Ebin_op (op, e1', e2)) (shrink_expr e1)
      <+> map (fun e2' -> Ebin_op (op, e1, e2')) (shrink_expr e2)
      <+> return (Ebin_op (Add, e1, e2)))
  | Eun_op (_, e) ->
    Iter.(return e <+> map (fun e' -> Eun_op (Negative, e')) (shrink_expr e))
  | Eif_then_else (cond_e, then_e, Some else_e) ->
    Iter.(
      return then_e
      <+> return else_e
      <+> map
            (fun cond_e' -> Eif_then_else (cond_e', then_e, Some else_e))
            (shrink_expr cond_e)
      <+> map
            (fun then_e' -> Eif_then_else (cond_e, then_e', Some else_e))
            (shrink_expr then_e)
      <+> map
            (fun else_e' -> Eif_then_else (cond_e, then_e, Some else_e'))
            (shrink_expr else_e))
  | Eif_then_else (cond_e, then_e, None) ->
    Iter.(
      return then_e
      <+> map (fun cond_e' -> Eif_then_else (cond_e', then_e, None)) (shrink_expr cond_e)
      <+> map (fun then_e' -> Eif_then_else (cond_e, then_e', None)) (shrink_expr then_e))
  | Eoption (Some e) -> shrink_expr e
  | Eoption None -> Iter.empty
  | Elist es ->
    Iter.(
      (*removing elements from the list *)
      let shrink_list_length =
        map (fun es' -> Elist es') (QCheck.Shrink.list ~shrink:shrink_expr es)
      in
      (* shrink each element within the list *)
      let shrink_elements =
        List.fold_right
          (fun e acc ->
            map
              (fun e' -> Elist (List.map2 (fun x y -> if x = e then e' else y) es es))
              (shrink_expr e)
            <+> acc)
          es
          empty
      in
      shrink_list_length <+> shrink_elements)
  | Etuple (e1, e2, e3) ->
    Iter.(
      map (fun e1' -> Etuple (e1', e2, e3)) (shrink_expr e1)
      <+> map (fun e2' -> Etuple (e1, e2', e3)) (shrink_expr e2)
      <+> map
            (fun e3' -> Etuple (e1, e2, e3'))
            (QCheck.Shrink.list ~shrink:shrink_expr e3))
  | Elet (flag, vb, vb_l, e) ->
    Iter.(
      let shrink_value_binding_length =
        map (fun vb_l' -> Elet (flag, vb, vb_l', e)) (QCheck.Shrink.list vb_l)
      in
      let shrink_elements =
        List.fold_right
          (fun a acc ->
            map
              (fun e' ->
                Elet
                  (flag, vb, List.map2 (fun x y -> if x = a then e' else y) vb_l vb_l, e))
              (shrink_value_binding a)
            <+> acc)
          vb_l
          empty
      in
      return e
      <+> map (fun e' -> Elet (flag, vb, vb_l, e')) (shrink_expr e)
      <+> shrink_value_binding_length
      <+> map (fun vb' -> Elet (flag, vb', vb_l, e)) (shrink_value_binding vb)
      <+> shrink_elements)
  | Efun (pattern, patterns, body) ->
    Iter.(
      map (fun body' -> Efun (pattern, patterns, body')) (shrink_expr body)
      <+> map (fun pattern' -> Efun (pattern', patterns, body)) (shrink_pattern pattern)
      <+> map
            (fun patterns' -> Efun (pattern, patterns', body))
            (QCheck.Shrink.list patterns))
  | Efun_application (e1, e2) ->
    Iter.(
      return e1
      <+> return e2
      <+> map (fun e1' -> Efun_application (e1', e2)) (shrink_expr e1)
      <+> map (fun e2' -> Efun_application (e1, e2')) (shrink_expr e2))
  | Ematch (e, case, case_l) ->
    Iter.(
      let shrink_cases_length =
        map (fun cases' -> Ematch (e, case, cases')) (QCheck.Shrink.list case_l)
      in
      map (fun e' -> Ematch (e', case, case_l)) (shrink_expr e) <+> shrink_cases_length)
  | _ -> Iter.empty

and shrink_value_binding = function
  | Evalue_binding (id, e) ->
    Iter.(map (fun e' -> Evalue_binding (id, e')) (shrink_expr e))
;;

let shrink_structure_item = function
  | SEval e -> Iter.(map (fun e' -> SEval e') (shrink_expr e))
  | SValue (r, vb, vb_l) ->
    Iter.(
      let shrink_value_binding_length =
        map (fun vb_l' -> SValue (r, vb, vb_l')) (QCheck.Shrink.list vb_l)
      in
      map (fun vb' -> SValue (r, vb', vb_l)) (shrink_value_binding vb)
      <+> shrink_value_binding_length)
;;

let shrink_structure structure : structure Iter.t =
  match structure with
  | [] -> Iter.empty
  | _ ->
    Iter.(
      let shrink_elements =
        List.fold_right
          (fun e acc ->
            map
              (fun e' ->
                List.map2 (fun x y -> if x = e then e' else y) structure structure)
              (shrink_structure_item e)
            <+> acc)
          structure
          empty
      in
      QCheck.Shrink.list structure <+> shrink_elements)
;;

let arbitrary_structure =
  make gen_structure ~print:(Format.asprintf "%a" prpr_structure) ~shrink:shrink_structure
;;

let run_auto n =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_structure ~count:n (fun structure ->
          Result.ok structure = parse_expr (Format.asprintf "%a" prpr_structure structure)))
    ]
;;
