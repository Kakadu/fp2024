(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open QCheck.Shrink
open QCheck.Iter
open Ast

let shid = function
  | "a" -> empty
  | _ -> return "a"
;;

let shlist1 ~shrink (head, tail) =
  match tail with
  | [] -> shrink head >|= fun head' -> head', tail
  | _ ->
    shrink head
    >|= (fun head' -> head', tail)
    <+> (list ~shrink tail >|= fun tail' -> head, tail')
    |> filter (fun (_, t) ->
      match t with
      | [] -> false
      | _ -> true)
;;

let shlist2 ~shrink (first, second, rest) =
  match rest with
  | [] ->
    shrink first
    >|= (fun first' -> first', second, rest)
    <+> (shrink second >|= fun second' -> first, second', rest)
  | _ ->
    shrink first
    >|= (fun first' -> first', second, rest)
    <+> (shrink second >|= fun second' -> first, second', rest)
    <+> (list ~shrink rest >|= fun tail' -> first, second, tail')
    |> filter (fun (_, _, t) ->
      match t with
      | [] -> false
      | _ -> true)
;;

let rec shmeasure = function
  | Measure_ident id -> shid id >|= fun id' -> Measure_ident id'
  | Measure_prod (m1, m2) ->
    shmeasure m1
    >|= (fun m1' -> Measure_prod (m1', m2))
    <+> (shmeasure m2 >|= fun m2' -> Measure_prod (m1, m2'))
  | Measure_div (m1, m2) ->
    shmeasure m1
    >|= (fun m1' -> Measure_div (m1', m2))
    <+> (shmeasure m2 >|= fun m2' -> Measure_div (m1, m2'))
  | Measure_pow (m, exp) ->
    (match exp with
     | 2 -> empty
     | _ -> return 2)
    >>= fun exp' -> shmeasure m >|= fun m' -> Measure_pow (m', exp')
  | Measure_dimless -> empty
;;

let shconst = function
  | Const_int i -> int i >|= fun i' -> Const_int i'
  | Const_float f ->
    (match f with
     | 0. -> empty
     | _ -> return (Const_float 0.))
  | Const_string s -> string ~shrink:char s >|= fun s' -> Const_string s'
  | Const_char c -> char c >|= fun c' -> Const_char c'
  | Const_unit_of_measure (Unit_of_measure (n, m)) ->
    (match n with
     | Mnum_int i -> int i >|= fun i' -> Mnum_int i'
     | Mnum_float f ->
       (match f with
        | 0. -> empty
        | _ -> return (Mnum_float 0.)))
    >>= fun n' ->
    shmeasure m >|= fun m' -> Const_unit_of_measure (Unit_of_measure (n', m'))
  | _ -> empty
;;

let rec shtype = function
  | Type_ident _ -> empty
  | Type_func (t1, t2) ->
    shtype t1
    >|= (fun t1' -> Type_func (t1', t2))
    <+> (shtype t2 >|= fun t2' -> Type_func (t1, t2'))
  | Type_tuple (t1, t2, trest) ->
    shlist2 ~shrink:shtype (t1, t2, trest)
    >|= fun (t1', t2', trest') -> Type_tuple (t1', t2', trest')
;;

let rec shpat = function
  | Pattern_wild -> return Pattern_wild
  | Pattern_ident_or_op id -> shid id >|= fun id' -> Pattern_ident_or_op id'
  | Pattern_const c -> shconst c >|= fun c' -> Pattern_const c'
  | Pattern_typed (pat, t) ->
    shpat pat
    >|= (fun pat' -> Pattern_typed (pat', t))
    <+> (shtype t >|= fun t' -> Pattern_typed (pat, t'))
  | Pattern_tuple (p1, p2, prest) ->
    shlist2 ~shrink:shpat (p1, p2, prest)
    >|= fun (p1', p2', prest') -> Pattern_tuple (p1', p2', prest')
  | Pattern_list pl -> list ~shrink:shpat pl >|= fun pl' -> Pattern_list pl'
  | Pattern_or (p1, p2) ->
    shpat p1
    >|= (fun p1' -> Pattern_or (p1', p2))
    <+> (shpat p2 >|= fun p2' -> Pattern_or (p1, p2'))
  | Pattern_option None -> return (Pattern_option None)
  | Pattern_option (Some pat) -> shpat pat >|= fun pat' -> Pattern_option (Some pat')
;;

let rec shexp = function
  | Expr_const c -> shconst c >|= fun c' -> Expr_const c'
  | Expr_ident_or_op id -> shid id >|= fun id' -> Expr_ident_or_op id'
  | Expr_tuple (e1, e2, erest) ->
    shlist2 ~shrink:shexp (e1, e2, erest)
    >|= fun (e1', e2', erest') -> Expr_tuple (e1', e2', erest')
  | Expr_list el -> list ~shrink:shexp el >|= fun el' -> Expr_list el'
  | Expr_lam (p, e) ->
    shpat p >|= (fun p' -> Expr_lam (p', e)) <+> shexp e >|= fun e' -> Expr_lam (p, e')
  | Expr_ifthenelse (i, t, e) ->
    (match e with
     | None ->
       shexp i
       >|= (fun i' -> Expr_ifthenelse (i', t, e))
       <+> (shexp t >|= fun t' -> Expr_ifthenelse (i, t', e))
     | Some el ->
       shexp i
       >|= (fun i' -> Expr_ifthenelse (i', t, e))
       <+> (shexp t >|= fun t' -> Expr_ifthenelse (i, t', e))
       <+> (shexp el >|= fun el' -> Expr_ifthenelse (i, t, Some el')))
  | Expr_apply (e1, e2) ->
    shexp e1
    >|= (fun e1' -> Expr_apply (e1', e2))
    <+> (shexp e2 >|= fun e2' -> Expr_apply (e1, e2'))
  | Expr_let (flag, bindh, bindt, e) ->
    shlist1 ~shrink:shbind (bindh, bindt)
    >|= (fun (bindh', bindt') -> Expr_let (flag, bindh', bindt', e))
    <+> shexp e
    >|= fun e' -> Expr_let (flag, bindh, bindt, e')
  | Expr_match (cond, ruleh, rulet) ->
    shexp cond
    >|= (fun cond' -> Expr_match (cond', ruleh, rulet))
    <+> (shlist1 ~shrink:shrule (ruleh, rulet)
         >|= fun (ruleh', rulet') -> Expr_match (cond, ruleh', rulet'))
  | Expr_function (ruleh, rulet) ->
    shlist1 ~shrink:shrule (ruleh, rulet)
    >|= fun (ruleh', rulet') -> Expr_function (ruleh', rulet')
  | Expr_typed (e, t) ->
    shexp e
    >|= (fun e' -> Expr_typed (e', t))
    <+> (shtype t >|= fun t' -> Expr_typed (e, t'))
    | Expr_option None -> return (Expr_option None)
  | Expr_option (Some exp) -> shexp exp >|= fun exp' -> Expr_option (Some exp')

and shbind = function
  | Bind (pat, exp) ->
    shpat pat >>= fun pat' -> shexp exp >>= fun exp' -> return (Bind (pat', exp'))

and shrule = function
  | Rule (pat, exp) ->
    shpat pat >>= fun pat' -> shexp exp >>= fun exp' -> return (Rule (pat', exp'))
;;

let shsitem = function
  | Str_item_eval e -> shexp e >|= fun e' -> Str_item_eval e'
  | Str_item_def (flag, bindh, bindt) ->
    shlist1 ~shrink:shbind (bindh, bindt)
    >|= fun (bindh', bindt') -> Str_item_def (flag, bindh', bindt')
  | Str_item_type_def _ -> empty (* Isn't being generated yet *)
;;

let shprog = list ~shrink:shsitem
