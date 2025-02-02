(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Base.Result

type name = string
type key = int
type fresh = int

let ( --| ) x f =
  match x with
  | Ok ok -> Ok ok
  | Error e -> Error (f e)
;;

let ( --= ) x f =
  match x with
  | Ok ok -> Ok ok
  | Error e -> f e
;;

let ( let+ ) = ( >>| )
let ( let- ) = ( --| )
let ( let* ) = ( >>= )

module NMap = Map.Make (String)
module KMap = Map.Make (Int)

type dfs_key = key
type pe_exprs_key = key
type keys = dfs_key option * pe_exprs_key
type ident_ext = ident * pe_exprs_key

type 'a bintree =
  | Node of 'a * 'a * 'a
  | Nul

type pattern_ext =
  | P of pe_exprs_key option * pat_ext
  | Lnk of pe_exprs_key

and pat_ext =
  | PEConst of pconst
  | PECons of pattern_ext * pattern_ext
  | PEEnum of pattern_ext list
  | PETuple of pattern_ext * pattern_ext * pattern_ext list
  | PEMaybe of pattern_ext maybe
  | PETree of pattern_ext bintree

type df_ext =
  | VarsD of pattern_ext * bindingbody * binding list
  | FunDs of ident_ext * (pattern * pattern list * bindingbody * binding list) list

type value =
  | VConst of const
  | VMaybe of value maybe
  | VList of value list
  | VTuple of value * value * value list
  | VClosures of keys NMap.t * (pattern * pattern list * bindingbody * binding list) list
  | VTree of value bintree

type intern_p = pat_ext

type crit_err =
  [ `Typing_err
  | `Not_exh
  | `Div_by_zero
  | `Negative_exponent
  ]

type df =
  | Df of keys NMap.t * df_ext
  | Err of crit_err

type lazy_list =
  | IntLL of int * Z.t * int (** (start, step, fin) *)
  | BoolLL of bool (** infinite list of true / infinite list of false*)
  | UnitLL (** infinite list of () *)

type pe_expr =
  | V of value
  | ThTree of intern_p
  | ThLeaf of keys NMap.t * expression
  | Link of pe_exprs_key
  | Er of crit_err
  | LazyLst of lazy_list (** invariant: non empty*)

module Triple = struct
  let fst (x, _, _) = x
  let snd (_, x, _) = x
  let thrd (_, _, x) = x
end

type level =
  | TopLevel
  | Inner

type env = df KMap.t * pe_expr KMap.t * keys NMap.t

open Triple
open KMap

let ext_pe_exprs_opt pe_expr pe_exprs = function
  | None -> pe_exprs
  | Some k -> add k pe_expr pe_exprs
;;

let init_env, init_fresh =
  let arg fresh =
    let name = Int.to_string fresh in
    name, ([], PIdentificator (Ident name), []), (Identificator (Ident name), [])
  in
  let dfs, pe_exprs, kk, fresh = empty, empty, NMap.empty, 0 in
  let prnt, fresh =
    let name, p, e = arg fresh in
    let kk' = NMap.add name (None, fresh) kk in
    let bd =
      OrdBody (FunctionApply ((Identificator (Ident "print_int"), []), e, []), [])
    in
    V (VClosures (kk', [ p, [], bd, [] ])), fresh + 1
  in
  let sq, fresh =
    let (name1, p1, e1), (name2, p2, e2) = arg fresh, arg (fresh + 1) in
    let kk' = NMap.add name2 (None, fresh + 1) (NMap.add name1 (None, fresh) kk) in
    let bd =
      OrdBody (FunctionApply ((Identificator (Ident "print_int"), []), e1, [ e2 ]), [])
    in
    V (VClosures (kk', [ p1, [ p2 ], bd, [] ])), fresh + 2
  in
  let kk = NMap.add "print_int" (None, fresh + 1) (NMap.add "seq" (None, fresh) kk) in
  let pe_exprs = add fresh sq (add (fresh + 1) prnt pe_exprs) in
  (dfs, pe_exprs, kk), fresh + 2
;;

let fm_add_many = List.fold_left (fun m (k, el) -> add k el m)

let new_th kk fresh pe_exprs (e, _) =
  (add fresh (ThLeaf (kk, e)) pe_exprs, fresh + 1), Lnk fresh
;;

let enode_to_pat pe_exprs fresh kk e1 e2 e3 =
  let (pe_exprs, fresh), p1' = new_th kk fresh pe_exprs e1 in
  let (pe_exprs, fresh), p2' = new_th kk fresh pe_exprs e2 in
  let (pe_exprs, fresh), p3' = new_th kk fresh pe_exprs e3 in
  PETree (Node (p1', p2', p3')), pe_exprs, fresh
;;

let etuple_to_pat pe_exprs fresh kk e1 e2 ee =
  let (pe_exprs, fresh), p1' = new_th kk fresh pe_exprs e1 in
  let pf, p2' = new_th kk fresh pe_exprs e2 in
  let (pe_exprs, fresh), pp' = List.fold_left_map (fun (pe, f) -> new_th kk f pe) pf ee in
  PETuple (p1', p2', pp'), pe_exprs, fresh
;;

let econs_to_pat pe_exprs fresh kk e1 e2 =
  let (pe_exprs, fresh), p1' = new_th kk fresh pe_exprs e1 in
  let (pe_exprs, fresh), p2' = new_th kk fresh pe_exprs e2 in
  PECons (p1', p2'), pe_exprs, fresh
;;

let conv_res = function
  | fst, `V v -> fst, V v
  | fst, `Th (kk, (e, _)) -> fst, ThLeaf (kk, e)
  | fst, `LazyLst ll -> fst, LazyLst ll
;;

let from_crit_err = function
  | (`Not_exh : crit_err) -> `Not_exh
  | `Div_by_zero -> `Div_by_zero
  | `Negative_exponent -> `Negative_exponent
  | `Typing_err -> `Typing_err
;;

let pm_key ((dfs, pe_exprs, kk) as env) fresh helper ok_lnk ok k ff =
  let pattern_match_v, pattern_match, patpat_match, ptrnll_match = ff in
  let pm_res_hndl = function
    | Ok (to_pe_ex, (dfs, pe_exprs, fresh)) ->
      let pe_exprs = fm_add_many pe_exprs to_pe_ex in
      ok (dfs, pe_exprs, thrd env) fresh
    | Error (`Not_match, (dfs, pe_exprs, fresh)) -> helper (dfs, pe_exprs, thrd env) fresh
    | Error
        ( ((`Not_exh | `Typing_err | `Div_by_zero | `Negative_exponent) as er)
        , (dfs, pe_exprs, fresh) ) -> Error (er, (dfs, pe_exprs, fresh))
  in
  function
  | Lnk k' -> ok_lnk env fresh k k'
  | P (k0, pat) as p ->
    let rec helper_key k =
      match KMap.find k pe_exprs with
      | Er er -> Error (er, (dfs, pe_exprs, fresh))
      | Link k -> helper_key k
      | V v ->
        (match pattern_match_v p v [] with
         | Ok to_pe_ex ->
           let pe_exprs = fm_add_many pe_exprs to_pe_ex in
           ok (dfs, pe_exprs, kk) fresh
         | Error `Not_match -> helper env fresh
         | Error ((`Not_exh | `Typing_err | `Div_by_zero | `Negative_exponent) as er) ->
           Error (er, (dfs, pe_exprs, fresh)))
      | ThLeaf (kk, e) ->
        pattern_match [] (dfs, pe_exprs, kk) fresh (Some k) p e |> pm_res_hndl
      | ThTree pat' ->
        let to_pe_ex =
          match k0 with
          | Some k0 -> [ k0, Link k ]
          | None -> []
        in
        (match patpat_match to_pe_ex env fresh pat pat' with
         | Error (`Not_match, ((dfs, pe_exprs, fresh), pat'')) ->
           helper (dfs, add k (ThTree pat'') pe_exprs, kk) fresh
         | Error
             ( ((`Not_exh | `Typing_err | `Div_by_zero | `Negative_exponent) as er)
             , ((dfs, pe_exprs, fresh), pat'') ) ->
           Error (er, (dfs, add k (ThTree pat'') pe_exprs, fresh))
         | Ok ((to_pe_ex, (dfs, pe_exprs, fresh)), pat'') ->
           let pe_exprs = fm_add_many pe_exprs ((k, ThTree pat'') :: to_pe_ex) in
           ok (dfs, pe_exprs, kk) fresh)
      | LazyLst ll -> ptrnll_match env fresh [] (Some k) ll p |> pm_res_hndl
    in
    helper_key k
;;

let rec eval_bnds level env fresh bnds =
  let to_dfs, (kk, main, fresh) =
    let prep_eval_bnd ((to_dfs, (kk0, main, fresh)) as init) = function
      | Decl _ -> init
      | Def (FunDef ((Ident i as id), p, pp, bd, bb)) ->
        (match
           List.fold_left_map
             (fun fl -> function
               | dk, FunDs ((i, pek), defs) when id = i ->
                 true, (dk, FunDs ((i, pek), (p, pp, bd, bb) :: defs))
               | el -> fl, el)
             false
             to_dfs
         with
         | true, to_dfs' -> to_dfs', Stdlib.snd init
         | _ ->
           ( (fresh, FunDs ((id, fresh + 1), [ p, pp, bd, bb ])) :: to_dfs
           , (NMap.add i (Some fresh, fresh + 1) kk0, main, fresh + 2) ))
      | Def (VarsDef (p, bd, bb)) ->
        let dfs_key, fresh = fresh, fresh + 1 in
        let ((kk, _, _) as stf), pe =
          prep_p ~dfs_key:(Some dfs_key) (kk0, main, fresh) p
        in
        (if kk == kk0 then to_dfs else (dfs_key, VarsD (pe, bd, bb)) :: to_dfs), stf
    in
    List.fold_left prep_eval_bnd ([], (thrd env, None, fresh)) bnds
  in
  let dfs =
    List.fold_left (fun dfs (k, d) -> KMap.add k (Df (kk, d)) dfs) (fst env) to_dfs
  in
  match level, main with
  | TopLevel, Some keys ->
    (match eval_var_full keys (dfs, snd env, fresh) with
     | Ok ((dfs, pe_exprs, fresh), _) -> Ok ((dfs, pe_exprs, kk), fresh)
     | Error (e, (dfs, pe_exprs, fresh)) -> Error (e, ((dfs, pe_exprs, kk), fresh)))
  | _ -> Ok ((dfs, snd env, kk), fresh)

and prep_p ?(dfs_key = None) (kk, main, fresh) (alias, p, _) =
  let prep_p = prep_p ~dfs_key in
  let helper_list = List.fold_left_map prep_p in
  let com_key =
    match alias with
    | [] -> None
    | _ -> Some fresh
  in
  let kk, main =
    List.fold_left
      (fun (kk, main) (Ident i) ->
        ( NMap.add i (dfs_key, fresh) kk
        , if String.equal i "main" then Some (dfs_key, fresh) else main ))
      (kk, main)
      alias
  in
  let stf = kk, main, fresh + 1 in
  match p with
  | PWildcard -> stf, Lnk fresh
  | PIdentificator (Ident i) ->
    ( ( NMap.add i (dfs_key, fresh) kk
      , (if String.equal i "main" then Some (dfs_key, fresh) else main)
      , fresh + 1 )
    , Lnk fresh )
  | PConst c -> stf, P (com_key, PEConst c)
  | PMaybe Nothing -> stf, P (com_key, PEMaybe Nothing)
  | PTree PNul -> stf, P (com_key, PETree Nul)
  | PMaybe (Just p) ->
    let stf, pe = prep_p stf p in
    stf, P (com_key, PEMaybe (Just pe))
  | PTuple (p1, p2, pp) ->
    let stf, pe1 = prep_p stf p1 in
    let stf, pe2 = prep_p stf p2 in
    let stf, pes = helper_list stf pp in
    stf, P (com_key, PETuple (pe1, pe2, pes))
  | PList (PEnum pp) ->
    let stf, pes = helper_list stf pp in
    stf, P (com_key, PEEnum pes)
  | PList (PCons (p1, p2)) ->
    let stf, pe1 = prep_p stf p1 in
    let stf, pe2 = prep_p stf p2 in
    stf, P (com_key, PECons (pe1, pe2))
  | PTree (PNode (p1, p2, p3)) ->
    let stf, pe1 = prep_p stf p1 in
    let stf, pe2 = prep_p stf p2 in
    let stf, pe3 = prep_p stf p3 in
    stf, P (com_key, PETree (Node (pe1, pe2, pe3)))

and find_expr ((dfs, pe_exprs, fresh) as dpf) = function
  | None, pe_exprs_key -> Ok (dpf, find pe_exprs_key pe_exprs)
  | Some dfs_key, pe_exprs_key ->
    (try Ok (dpf, find pe_exprs_key pe_exprs) with
     | Not_found ->
       (match find dfs_key dfs with
        | Err e -> Error (e, dpf)
        | Df (kk, FunDs ((_, pe_exprs_key), defs)) ->
          let v = VClosures (kk, defs) in
          Ok ((dfs, add pe_exprs_key (V v) pe_exprs, fresh), V v)
        | Df (kk, VarsD (p, bd, bb)) ->
          let pm (e, _) env fresh =
            match pattern_match [] env fresh None p e with
            | Ok (to_pe_ex, (dfs, pe_exprs, fresh)) ->
              let pe_exprs = fm_add_many pe_exprs to_pe_ex in
              Ok (dfs, pe_exprs, fresh)
            | Error (`Not_match, (dfs, pe_exprs, fresh)) ->
              let dfs = add dfs_key (Err `Not_exh) dfs in
              Error (`Not_exh, (dfs, pe_exprs, fresh))
            | Error
                ( ((`Not_exh | `Typing_err | `Div_by_zero | `Negative_exponent) as er)
                , (dfs, pe_exprs, fresh) ) ->
              let dfs = add dfs_key (Err er) dfs in
              Error (er, (dfs, pe_exprs, fresh))
          in
          let- e, (dfs, pe_exprs_key, fresh) =
            match eval_bnds Inner (dfs, pe_exprs, kk) fresh bb with
            | Error (e, ((dfs, pe_exprs, _), fresh)) -> Error (e, (dfs, pe_exprs, fresh))
            | Ok (((_, _, kk) as env), fresh) ->
              let+ dpf =
                match bd with
                | OrdBody e -> pm e env fresh
                | Guards (cb, cbs) ->
                  let* (dfs, pe_exprs, fresh), e =
                    eval_step_guards env fresh (cb :: cbs)
                  in
                  pm e (dfs, pe_exprs, kk) fresh
              in
              dpf, find pe_exprs_key (snd dpf)
          in
          e, (add dfs_key (Err e) dfs, pe_exprs_key, fresh)))

and pattern_match to_pe_ex ((dfs, pe_exprs, kk) as env) fresh src ptrn e =
  match ptrn with
  | Lnk k -> Ok ((k, ThLeaf (kk, e)) :: to_pe_ex, (dfs, pe_exprs, fresh))
  | P (k, pat) ->
    let pmv_call_tr k to_pe_ex v dpf =
      match pattern_match_v (P (k, pat)) v to_pe_ex with
      | Ok to_pe_ex -> Ok (to_pe_ex, dpf)
      | Error e -> Error (e, dpf)
    in
    let pmv_call_lf v dpf =
      match pattern_match_v (P (None, pat)) v [] with
      | Ok _ -> Ok dpf
      | Error e -> Error (e, dpf)
    in
    let ppm_call to_pe_ex k pk env fresh pat2 =
      let to_pe_ex =
        match k with
        | None -> to_pe_ex
        | Some k -> (k, Link pk) :: to_pe_ex
      in
      match patpat_match to_pe_ex env fresh pat pat2 with
      | Ok ((to_pe_ex, (dfs, pe_exprs, fresh)), pat2') ->
        Ok (to_pe_ex, (dfs, add pk (ThTree pat2') pe_exprs, fresh))
      | Error (e, ((dfs, pe_exprs, fresh), pat2')) ->
        Error (e, (dfs, add pk (ThTree pat2') pe_exprs, fresh))
    in
    let from_crit_full src ((e : crit_err), ((dfs, pe_exprs, fresh) as dpf)) =
      let dpf =
        match src with
        | None -> dpf
        | Some k -> dfs, add k (Er e) pe_exprs, fresh
      in
      from_crit_err e, dpf
    in
    let other_exprs typing_err src cont ((_, _, kk) as env) fresh v_call ac =
      let v_call ((dfs, pe_exprs, fresh), v) =
        let pe_exprs = ext_pe_exprs_opt (V v) pe_exprs src in
        v_call v (dfs, pe_exprs, fresh)
      in
      function
      | IfThenEsle (c, th, el) ->
        let* (dfs, pe_exprs, fresh), (e, _) =
          eval_step_ite c th el env fresh --| from_crit_full src
        in
        cont ~after_complex:true src (dfs, pe_exprs, kk) fresh e
      | Case (e, br, brs) ->
        let* env, fresh, (e, _) =
          eval_step_case e env fresh (br :: brs) --| from_crit_full src
        in
        cont ~after_complex:true src env fresh e
      | FunctionApply (f, a, aa) ->
        eval_step_funapp env fresh (f, a :: aa) --| from_crit_full src
        >>= (function
         | (dfs, pe_exprs, fresh), `Th (kk, (e, _)) ->
           cont ~after_complex:true src (dfs, pe_exprs, kk) fresh e
         | dpf, `V v -> v_call (dpf, v))
      | InnerBindings (b, bb, (e, _)) ->
        let* env, fresh = eval_step_inner_bb (b :: bb) env fresh --| from_crit_full src in
        cont ~after_complex:true src env fresh e
      | Binop (e1, op, e2) when op <> Cons ->
        eval_arlog env fresh e1 e2 op --| from_crit_full src >>= v_call
      | Neg e -> eval_expr_full env fresh e --| from_crit_full src >>= v_call
      | e -> typing_err env fresh src ac e
    in
    let rec helper_keys_tr helper to_pe_ex src k ((_, pk) as keys) env fresh =
      let dfs, pe_exprs, kk = env in
      let helper_keys_tr = helper_keys_tr helper to_pe_ex src k in
      let* ((dfs, pe_exprs, fresh) as dpf), e =
        find_expr (dfs, pe_exprs, fresh) keys --| from_crit_full src
      in
      match e with
      | Link k -> helper_keys_tr (None, k) (dfs, pe_exprs, kk) fresh
      | _ ->
        let ((dfs, pe_exprs, fresh) as dpf) =
          match src with
          | Some k -> dfs, add k (Link pk) pe_exprs, fresh
          | None -> dpf
        in
        (match e with
         | Er er -> Error (from_crit_err er, dpf)
         | V v -> pmv_call_tr k to_pe_ex v dpf
         | ThLeaf (kk, e) ->
           let to_pe_ex =
             match k with
             | None -> to_pe_ex
             | Some k -> (k, Link pk) :: to_pe_ex
           in
           helper to_pe_ex None (Some pk) (dfs, pe_exprs, kk) fresh e
         | ThTree pat2 -> ppm_call to_pe_ex k pk (dfs, pe_exprs, kk) fresh pat2
         | LazyLst ll -> ptrnll_match env fresh to_pe_ex (Some pk) ll (P (k, pat))
         | Link k -> helper_keys_tr (None, k) (dfs, pe_exprs, kk) fresh (*недостижимо *))
    in
    let rec helper_keys_lf helper src k ((_, pk) as keys) (dfs, pe_exprs, kk) fresh =
      let helper_keys_lf = helper_keys_lf helper src k in
      let* ((dfs, pe_exprs, fresh) as dpf), e =
        find_expr (dfs, pe_exprs, fresh) keys --| from_crit_full src
      in
      match e with
      | Link k -> helper_keys_lf (None, k) (dfs, pe_exprs, kk) fresh
      | _ ->
        let ((dfs, pe_exprs, fresh) as dpf) =
          match src with
          | None -> dpf
          | Some k -> dfs, add k (Link (Stdlib.snd keys)) pe_exprs, fresh
        in
        (match e with
         | Er e -> Error (from_crit_err e, dpf)
         | V v -> pmv_call_lf v dpf
         | ThLeaf (kk, e) -> helper (Some pk) (dfs, pe_exprs, kk) fresh e
         | ThTree pat2 ->
           (match patpat_match ~k:(Some pk) [] (dfs, pe_exprs, kk) fresh pat pat2 with
            | Ok ((_, dpf), _) -> Ok dpf
            | Error (e, (dpf, _)) -> Error (e, dpf))
         | LazyLst _ -> Error (`Typing_err, dpf)
         | Link k -> helper_keys_lf (None, k) (dfs, pe_exprs, kk) fresh (*недостижимо *))
    in
    let k_hndl_lf v dpf =
      match k with
      | None -> to_pe_ex, dpf
      | Some k -> (k, V v) :: to_pe_ex, dpf
    in
    let k_hndl_tr to_pe_ex = function
      | Some k -> (k, ThTree pat) :: to_pe_ex
      | None -> to_pe_ex
    in
    let ok_lf v (dfs, pe_exprs, fresh) src =
      let pe_exprs = ext_pe_exprs_opt (V v) pe_exprs src in
      Ok (dfs, pe_exprs, fresh)
    in
    let not_match_th e (dfs, pe_exprs, kk) fresh src after_complex =
      let pe_exprs =
        match src, after_complex with
        | None, _ | _, false -> pe_exprs
        | Some k, true -> add k (ThLeaf (kk, e)) pe_exprs
      in
      Error (`Not_match, (dfs, pe_exprs, fresh))
    in
    let not_match_v v (dfs, pe_exprs, fresh) src after_complex =
      let pe_exprs =
        match src, after_complex with
        | None, _ | _, false -> pe_exprs
        | Some k, true -> add k (V v) pe_exprs
      in
      Error (`Not_match, (dfs, pe_exprs, fresh))
    in
    let typing_err (dfs, pe_exprs, kk) fresh src after_complex e =
      let pe_exprs =
        match src, after_complex with
        | None, _ | _, false -> pe_exprs
        | Some k, true -> add k (ThLeaf (kk, e)) pe_exprs
      in
      Error (`Typing_err, (dfs, pe_exprs, fresh))
    in
    let elazylist_hndl e1 e2 e3 ((_, _, kk) as env) k src =
      elazylist_hndl e1 e2 e3 env fresh --| from_crit_full src
      >>= function
      | (dfs, pe_exprs, fresh), `LazyLst ll ->
        ptrnll_match (dfs, pe_exprs, kk) fresh to_pe_ex src ll (P (k, pat))
      | (dfs, pe_exprs, fresh), `V v ->
        let pe_exprs = ext_pe_exprs_opt (V v) pe_exprs src in
        pmv_call_tr k to_pe_ex v (dfs, pe_exprs, fresh)
    in
    (match pat with
     | PETree Nul ->
       let rec helper ~after_complex src ((dfs, pe_exprs, kk) as env) fresh = function
         | BinTreeBld Nul -> ok_lf (VTree Nul) (dfs, pe_exprs, fresh) src
         | BinTreeBld _ as e -> not_match_th e (dfs, pe_exprs, kk) fresh src after_complex
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_lf (helper ~after_complex:false) src k keys env fresh
         | e -> other_exprs typing_err src helper env fresh pmv_call_lf after_complex e
       in
       helper ~after_complex:false src env fresh e >>| k_hndl_lf (VTree Nul)
     | PEMaybe Nothing ->
       let rec helper ~after_complex src ((dfs, pe_exprs, kk) as env) fresh = function
         | ENothing -> ok_lf (VMaybe Nothing) (dfs, pe_exprs, fresh) src
         | FunctionApply ((EJust, _), _, []) as e ->
           not_match_th e (dfs, pe_exprs, kk) fresh src after_complex
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_lf (helper ~after_complex:false) src k keys env fresh
         | e -> other_exprs typing_err src helper env fresh pmv_call_lf after_complex e
       in
       helper ~after_complex:false src env fresh e >>| k_hndl_lf (VMaybe Nothing)
     | PEConst (OrdinaryPConst Unit) ->
       let rec helper ~after_complex src ((dfs, pe_exprs, kk) as env) fresh =
         let _ = after_complex in
         function
         | Const Unit -> ok_lf (VConst Unit) (dfs, pe_exprs, fresh) src
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_lf (helper ~after_complex:false) src k keys env fresh
         | e -> other_exprs typing_err src helper env fresh pmv_call_lf after_complex e
       in
       helper ~after_complex:false src env fresh e >>| k_hndl_lf (VConst Unit)
     | PEConst (OrdinaryPConst (Bool _ as c)) ->
       let rec helper ~after_complex src ((dfs, pe_exprs, kk) as env) fresh = function
         | Const c' when c = c' -> ok_lf (VConst c) (dfs, pe_exprs, fresh) src
         | Const (Bool b') ->
           not_match_v (VConst (Bool b')) (dfs, pe_exprs, fresh) src after_complex
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_lf (helper ~after_complex:false) src k keys env fresh
         | e -> other_exprs typing_err src helper env fresh pmv_call_lf after_complex e
       in
       helper ~after_complex:false src env fresh e >>| k_hndl_lf (VConst c)
     | PEConst (OrdinaryPConst (Int _ as c)) ->
       let rec helper ~after_complex src ((dfs, pe_exprs, kk) as env) fresh = function
         | Const c' when c = c' -> ok_lf (VConst c) (dfs, pe_exprs, fresh) src
         | Const (Int x) ->
           not_match_v (VConst (Int x)) (dfs, pe_exprs, fresh) src after_complex
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_lf (helper ~after_complex:false) src k keys env fresh
         | e -> other_exprs typing_err src helper env fresh pmv_call_lf after_complex e
       in
       helper ~after_complex:false src env fresh e >>| k_hndl_lf (VConst c)
     | PEConst (NegativePInt i) ->
       let rec helper ~after_complex src ((dfs, pe_exprs, kk) as env) fresh = function
         | Const (Int x) when x = -i -> ok_lf (VConst (Int x)) (dfs, pe_exprs, fresh) src
         | Const (Int x) ->
           not_match_v (VConst (Int x)) (dfs, pe_exprs, fresh) src after_complex
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_lf (helper ~after_complex:false) src k keys env fresh
         | e -> other_exprs typing_err src helper env fresh pmv_call_lf after_complex e
       in
       helper ~after_complex:false src env fresh e >>| k_hndl_lf (VConst (Int (-i)))
     | PETree (Node (p1, p2, p3)) ->
       let rec helper to_pe_ex k ~after_complex src ((dfs, pe_exprs, kk) as env) fresh
         = function
         | BinTreeBld Nul ->
           not_match_v (VTree Nul) (dfs, pe_exprs, fresh) src after_complex
         | BinTreeBld (Node (e1, e2, e3)) ->
           (match src with
            | None ->
              pattern_match_list
                ~strict_len:true
                (k_hndl_tr to_pe_ex k)
                env
                fresh
                [ p1; p2; p3 ]
                [ e1; e2; e3 ]
            | Some pk ->
              let pat', pe_exprs, fresh = enode_to_pat pe_exprs fresh kk e1 e2 e3 in
              ppm_call to_pe_ex k pk (dfs, pe_exprs, kk) fresh pat')
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_tr (helper ~after_complex:false) to_pe_ex src k keys env fresh
         | e ->
           let cont = helper to_pe_ex k in
           let pmv_call = pmv_call_tr k to_pe_ex in
           other_exprs typing_err src cont env fresh pmv_call after_complex e
       in
       helper ~after_complex:false to_pe_ex k src env fresh e
     | PEEnum pp ->
       let rec helper to_pe_ex k ~after_complex src ((dfs, pe_exprs, kk) as env) fresh
         = function
         | ListBld (LazyList (e1, e2, e3)) -> elazylist_hndl e1 e2 e3 env k src
         | ListBld (OrdList (IncomprehensionlList ee)) ->
           (match src with
            | None ->
              let to_pe_ex = k_hndl_tr to_pe_ex k in
              pattern_match_list ~strict_len:false to_pe_ex env fresh pp ee
            | Some pk ->
              let (pe_exprs, fresh), pp' =
                List.fold_left_map (fun (pe, f) -> new_th kk f pe) (pe_exprs, fresh) ee
              in
              ppm_call to_pe_ex k pk (dfs, pe_exprs, kk) fresh (PEEnum pp'))
         | Binop (((e1, _) as ex1), Cons, ((e2, _) as ex2)) as e ->
           (match pp with
            | [] -> not_match_th e (dfs, pe_exprs, kk) fresh src after_complex
            | p :: pp ->
              (match src with
               | None ->
                 let* to_pe_ex, (dfs, pe_exprs, fresh) =
                   pattern_match to_pe_ex (dfs, pe_exprs, kk) fresh None p e1
                 in
                 let p2 = P (None, PEEnum pp) in
                 let to_pe_ex =
                   match k with
                   | None -> to_pe_ex
                   | Some k -> (k, ThTree (PECons (p, p2))) :: to_pe_ex
                 in
                 pattern_match to_pe_ex (dfs, pe_exprs, kk) fresh None p2 e2
               | Some pk ->
                 let pat', pe_exprs, fresh = econs_to_pat pe_exprs fresh kk ex1 ex2 in
                 ppm_call to_pe_ex k pk (dfs, pe_exprs, kk) fresh pat'))
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_tr (helper ~after_complex:false) to_pe_ex src k keys env fresh
         | e ->
           let cont = helper to_pe_ex k in
           let pmv_call = pmv_call_tr k to_pe_ex in
           other_exprs typing_err src cont env fresh pmv_call after_complex e
       in
       helper ~after_complex:false to_pe_ex k src env fresh e
     | PECons (p1, p2) ->
       let rec helper to_pe_ex k ~after_complex src ((dfs, pe_exprs, kk) as env) fresh
         = function
         | ListBld (LazyList (e1, e2, e3)) -> elazylist_hndl e1 e2 e3 env k src
         | Binop (ex1, Cons, ex2) ->
           (match src with
            | None ->
              pattern_match_list
                ~strict_len:true
                (k_hndl_tr to_pe_ex k)
                env
                fresh
                [ p1; p2 ]
                [ ex1; ex2 ]
            | Some pk ->
              let pat', pe_exprs, fresh = econs_to_pat pe_exprs fresh kk ex1 ex2 in
              ppm_call to_pe_ex k pk (dfs, pe_exprs, kk) fresh pat')
         | ListBld (OrdList (IncomprehensionlList [])) ->
           not_match_v (VList []) (dfs, pe_exprs, fresh) src after_complex
         | ListBld (OrdList (IncomprehensionlList (e :: ee))) ->
           (match src with
            | None ->
              pattern_match_list
                ~strict_len:true
                (k_hndl_tr to_pe_ex k)
                env
                fresh
                [ p1; p2 ]
                [ e; ListBld (OrdList (IncomprehensionlList ee)), [] ]
            | Some pk ->
              let (pe_exprs, fresh), pp' =
                List.fold_left_map
                  (fun (pe, f) -> new_th kk f pe)
                  (pe_exprs, fresh)
                  (e :: ee)
              in
              ppm_call to_pe_ex k pk (dfs, pe_exprs, kk) fresh (PEEnum pp'))
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_tr (helper ~after_complex:false) to_pe_ex src k keys env fresh
         | e ->
           let cont = helper to_pe_ex k in
           let pmv_call = pmv_call_tr k to_pe_ex in
           other_exprs typing_err src cont env fresh pmv_call after_complex e
       in
       helper ~after_complex:false to_pe_ex k src env fresh e
     | PEMaybe (Just p) ->
       let rec helper to_pe_ex k ~after_complex src ((dfs, pe_exprs, kk) as env) fresh
         = function
         | ENothing ->
           not_match_v (VMaybe Nothing) (dfs, pe_exprs, fresh) src after_complex
         | FunctionApply ((EJust, _), ((e, _) as ex), []) ->
           (match src with
            | None ->
              let to_pe_ex = k_hndl_tr to_pe_ex k in
              pattern_match to_pe_ex (dfs, pe_exprs, kk) fresh None p e
            | Some pk ->
              let (pe_exprs, fresh), p1' = new_th kk fresh pe_exprs ex in
              ppm_call to_pe_ex k pk (dfs, pe_exprs, kk) fresh (PEMaybe (Just p1')))
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_tr (helper ~after_complex:false) to_pe_ex src k keys env fresh
         | e ->
           let cont = helper to_pe_ex k in
           let pmv_call = pmv_call_tr k to_pe_ex in
           other_exprs typing_err src cont env fresh pmv_call after_complex e
       in
       helper ~after_complex:false to_pe_ex k src env fresh e
     | PETuple (p1, p2, pp) ->
       let rec helper to_pe_ex k ~after_complex src ((dfs, pe_exprs, kk) as env) fresh =
         let _ = after_complex in
         function
         | TupleBld (e1, e2, ee) ->
           (match src with
            | None ->
              pattern_match_list
                ~strict_len:true
                (k_hndl_tr to_pe_ex k)
                env
                fresh
                (p1 :: p2 :: pp)
                (e1 :: e2 :: ee)
            | Some pk ->
              let pat', pe_exprs, fresh = etuple_to_pat pe_exprs fresh kk e1 e2 ee in
              ppm_call to_pe_ex k pk (dfs, pe_exprs, kk) fresh pat')
         | Identificator (Ident n) ->
           let keys = NMap.find n kk in
           helper_keys_tr (helper ~after_complex:false) to_pe_ex src k keys env fresh
         | e ->
           let cont = helper to_pe_ex k in
           let pmv_call = pmv_call_tr k to_pe_ex in
           other_exprs typing_err src cont env fresh pmv_call after_complex e
       in
       helper ~after_complex:false to_pe_ex k src env fresh e)

and pattern_match_list ~strict_len to_pe_ex (dfs, pe_exprs, kk) fresh pp ee =
  let rec helper pp ee (to_pe_ex, ((dfs, pe_exprs, fresh) as dpf)) =
    match pp, ee with
    | [], [] -> Ok (to_pe_ex, dpf)
    | p :: pp, (e, _) :: ee ->
      pattern_match to_pe_ex (dfs, pe_exprs, kk) fresh None p e >>= helper pp ee
    | _ -> Error ((if strict_len then `Typing_err else `Not_match), dpf)
  in
  helper pp ee (to_pe_ex, (dfs, pe_exprs, fresh))

and pattern_match_v ptrn v to_pe_ex =
  let pattern_match_v_list =
    List.fold_left2 (fun acc p v -> acc >>= pattern_match_v p v) (Ok to_pe_ex)
  in
  match ptrn with
  | Lnk k -> Ok ((k, V v) :: to_pe_ex)
  | P (k, pat) ->
    let to_pe_ex =
      match k with
      | None -> to_pe_ex
      | Some k -> (k, V v) :: to_pe_ex
    in
    (match pat, v with
     | PETree Nul, VTree Nul | PEMaybe Nothing, VMaybe Nothing -> Ok to_pe_ex
     | PEMaybe (Just p), VMaybe (Just v) -> pattern_match_v p v to_pe_ex
     | PEConst (OrdinaryPConst c), VConst c' when c = c' -> Ok to_pe_ex
     | PEConst (NegativePInt i), VConst (Int i') when i = -i' -> Ok to_pe_ex
     | PETuple (p1, p2, pp), VTuple (v1, v2, vv) ->
       (try pattern_match_v_list (p1 :: p2 :: pp) (v1 :: v2 :: vv) with
        | Invalid_argument _ -> Error `Typing_err)
     | PETree (Node (p1, p2, p3)), VTree (Node (v1, v2, v3)) ->
       pattern_match_v_list [ p1; p2; p3 ] [ v1; v2; v3 ]
     | PETree _, VTree _ | PEMaybe _, VMaybe _ -> Error `Not_match
     | PEConst (OrdinaryPConst (Int _) | NegativePInt _), VConst (Int _)
     | PEConst (OrdinaryPConst (Bool _)), VConst (Bool _) -> Error `Not_match
     | PEEnum pp, VList vv ->
       (try pattern_match_v_list pp vv with
        | Invalid_argument _ -> Error `Typing_err)
     | PECons _, VList [] -> Error `Not_match
     | PECons (p1, p2), VList (v :: vv) -> pattern_match_v_list [ p1; p2 ] [ v; VList vv ]
     | _ -> Error `Typing_err)

and patpat_match ?(k = None) to_pe_ex ((dfs, pe_exprs, kk) as env) fresh pat1 pat2 =
  let suc_default = Ok ((to_pe_ex, (dfs, pe_exprs, fresh)), pat2) in
  let suc_with_k k v = Ok ((to_pe_ex, (dfs, add k (V v) pe_exprs, fresh)), pat2) in
  let not_match_default = Error (`Not_match, ((dfs, pe_exprs, fresh), pat2)) in
  let not_match_with_k k v =
    Error (`Not_match, ((dfs, add k (V v) pe_exprs, fresh), pat2))
  in
  let typing_error =
    Error (`Typing_err, ((dfs, ext_pe_exprs_opt (ThTree pat2) pe_exprs k, fresh), pat2))
  in
  let ptrnptrn_match' to_pe_ex env fresh p1 p2 err_p ok =
    match ptrnptrn_match to_pe_ex env fresh p1 p2 with
    | Error (e, (dpf, p2')) -> Error (e, (dpf, err_p p2'))
    | Ok (td, p2') -> ok td p2'
  in
  let helper_conscons env fresh p11 p12 p21 p22 =
    let ok1 (to_pe_ex, (d, p, f)) p21' =
      let err_p p22' = PECons (p21', p22') in
      ptrnptrn_match' to_pe_ex (d, p, kk) f p12 p22 err_p
      @@ fun td p22' -> Ok (td, PECons (p21', p22'))
    in
    ptrnptrn_match' to_pe_ex env fresh p11 p21 (fun p21' -> PECons (p21', p22)) ok1
  in
  match pat1, pat2, k with
  | PETree (Node (p11, p12, p13)), PETree (Node (p21, p22, p23)), _ ->
    let node p1 p2 p3 = PETree (Node (p1, p2, p3)) in
    let ok2 p21' (to_pe_ex, (d, p, f)) p22' =
      let err_p = node p21' p22' in
      ptrnptrn_match' to_pe_ex (d, p, kk) f p13 p23 err_p
      @@ fun td p23' -> Ok (td, node p21' p22' p23')
    in
    let ok1 (to_pe_ex, (d, p, f)) p21' =
      let err_p p22' = node p21' p22' p23 in
      ptrnptrn_match' to_pe_ex (d, p, kk) f p12 p22 err_p (ok2 p21')
    in
    ptrnptrn_match' to_pe_ex env fresh p11 p21 (fun p21' -> node p21' p22 p23) ok1
  | PETree Nul, PETree Nul, None | PEMaybe Nothing, PEMaybe Nothing, None -> suc_default
  | PETree Nul, PETree Nul, Some k -> suc_with_k k (VTree Nul)
  | PEMaybe Nothing, PEMaybe Nothing, Some k -> suc_with_k k (VMaybe Nothing)
  | PEConst c, PEConst c', None when c = c' -> suc_default
  | PEConst (OrdinaryPConst (Int i)), PEConst (NegativePInt i'), None
  | PEConst (NegativePInt i'), PEConst (OrdinaryPConst (Int i)), None
    when i = -i' -> suc_default
  | PEConst (OrdinaryPConst (Int i)), PEConst (NegativePInt i'), Some k
  | PEConst (NegativePInt i'), PEConst (OrdinaryPConst (Int i)), Some k
    when i = -i' -> suc_with_k k (VConst (Int i))
  | PEConst (OrdinaryPConst c), PEConst (OrdinaryPConst c'), Some k when c = c' ->
    suc_with_k k (VConst c)
  | PEConst (NegativePInt i), PEConst (NegativePInt i'), Some k when i = i' ->
    suc_with_k k (VConst (Int (-i)))
  | PEMaybe (Just p1), PEMaybe (Just p2), _ ->
    let ok td p2' = Ok (td, PEMaybe (Just p2')) in
    let env = dfs, pe_exprs, kk in
    ptrnptrn_match' to_pe_ex env fresh p1 p2 (fun p2' -> PEMaybe (Just p2')) ok
  | PEMaybe _, PEMaybe _, _
  | PETree _, PETree _, _
  | PEConst (OrdinaryPConst (Bool _)), PEConst (OrdinaryPConst (Bool _)), None
  | ( PEConst (OrdinaryPConst (Int _) | NegativePInt _)
    , PEConst (OrdinaryPConst (Int _) | NegativePInt _)
    , None )
  | PECons _, PEEnum [], _
  | PEEnum [], PECons _, _ -> not_match_default
  | PEConst (OrdinaryPConst (Bool _)), PEConst (OrdinaryPConst (Bool _ as c)), Some k
  | PEConst (OrdinaryPConst (Int _)), PEConst (OrdinaryPConst (Int _ as c)), Some k ->
    not_match_with_k k (VConst c)
  | PEConst (OrdinaryPConst (Int _)), PEConst (NegativePInt i), Some k ->
    not_match_with_k k (VConst (Int (-i)))
  | PEConst (NegativePInt _), PEConst (OrdinaryPConst (Int i)), Some k ->
    not_match_with_k k (VConst (Int i))
  | PETuple (p11, p12, pp1), PETuple (p21, p22, pp2), _ ->
    let ok2 p21' td p22' =
      match ptrnptrn_match_list ~strict_len:true kk (td, []) pp1 pp2 with
      | Ok (td, pp2') -> Ok (td, PETuple (p21', p22', pp2'))
      | Error (e, (dpf, pp2')) -> Error (e, (dpf, PETuple (p21', p22', pp2')))
    in
    let ok1 (to_pe_ex, (d, p, f)) p21' =
      let err_p p22' = PETuple (p21', p22', pp2) in
      ptrnptrn_match' to_pe_ex (d, p, kk) f p12 p22 err_p (ok2 p21')
    in
    ptrnptrn_match' to_pe_ex env fresh p11 p21 (fun p21' -> PETuple (p21', p22, pp2)) ok1
  | PEEnum pp1, PEEnum pp2, _ ->
    let init = (to_pe_ex, (dfs, pe_exprs, fresh)), [] in
    (match ptrnptrn_match_list ~strict_len:false kk init pp1 pp2 with
     | Ok (td, pp2') -> Ok (td, PEEnum pp2')
     | Error (e, (dpf, pp2')) -> Error (e, (dpf, PEEnum pp2')))
  | PECons (p11, p12), PECons (p21, p22), _ -> helper_conscons env fresh p11 p12 p21 p22
  | PECons (p11, p12), PEEnum (p21 :: pp), _ ->
    let pe_exprs, fresh = add fresh (ThTree (PEEnum pp)) pe_exprs, fresh + 1 in
    helper_conscons (dfs, pe_exprs, kk) fresh p11 p12 p21 (Lnk (fresh - 1))
  | PEEnum (p11 :: pp), PECons (p21, p22), _ ->
    (helper_conscons (dfs, pe_exprs, kk) fresh p11 @@ P (None, PEEnum pp)) p21 p22
  | _ -> typing_error

and ptrnptrn_match_list ~strict_len kk init pp1 pp2 =
  let rec helper = function
    | Ok (td, pp'), [], [] -> Ok (td, List.rev pp')
    | Ok ((_, dpf), pp'), _ :: _, [] ->
      Error ((if strict_len then `Typing_err else `Not_match), (dpf, List.rev pp'))
    | Error (_, (dpf, pp')), _ :: _, [] when strict_len ->
      Error (`Typing_err, (dpf, List.rev pp'))
    | Error (e, (dpf, pp')), _, [] -> Error (e, (dpf, List.rev pp'))
    | Ok ((to_pe_ex, (dfs, pe_exprs, fresh)), pp'), p1 :: pp1, p2 :: pp2 ->
      (match ptrnptrn_match to_pe_ex (dfs, pe_exprs, kk) fresh p1 p2 with
       | Ok (td, p2') -> helper (Ok (td, p2' :: pp'), pp1, pp2)
       | Error (e, (dpf, p2')) -> helper (Error (e, (dpf, p2' :: pp')), pp1, pp2))
    | Ok ((_, dpf), pp'), [], _ :: _ when strict_len ->
      helper (Error (`Typing_err, (dpf, pp')), pp1, pp2)
    | Ok ((_, dpf), pp'), [], _ :: _ -> helper (Error (`Not_match, (dpf, pp')), pp1, pp2)
    | Error (_, (dpf, pp')), [], p2 :: pp2 when strict_len ->
      helper (Error (`Typing_err, (dpf, p2 :: pp')), pp1, pp2)
    | Error (e, (dpf, pp')), [], p2 :: pp2 ->
      helper (Error (e, (dpf, p2 :: pp')), pp1, pp2)
    | Error (e, (dpf, pp')), _ :: pp1, p2 :: pp2 ->
      helper (Error (e, (dpf, p2 :: pp')), pp1, pp2)
  in
  helper (Ok init, pp1, pp2)

and ptrnptrn_match to_pe_ex ((dfs, pe_exprs, kk) as env) fresh p1 p2 =
  match p1, p2 with
  | Lnk k1, (Lnk k2 | P (Some k2, _)) ->
    Ok (((k1, Link k2) :: to_pe_ex, (dfs, pe_exprs, fresh)), p2)
  | Lnk k1, P (None, pat2) ->
    let dpf = dfs, add fresh (ThTree pat2) pe_exprs, fresh + 1 in
    Ok (((k1, Link fresh) :: to_pe_ex, dpf), Lnk fresh)
  | P (None, pat1), P (None, pat2) ->
    (match patpat_match to_pe_ex env fresh pat1 pat2 with
     | Ok (td, pat2') -> Ok (td, P (None, pat2'))
     | Error (e, (dpf, pat2')) -> Error (e, (dpf, P (None, pat2'))))
  | (P (k1, pat1) as p), (Lnk k2 | P (Some k2, _)) ->
    let rec helper_key k =
      let e = find k pe_exprs in
      let dpf = dfs, pe_exprs, fresh in
      let pm_res_hndl = function
        | Ok (to_pe_ex, dpf) -> Ok ((to_pe_ex, dpf), Lnk k)
        | Error (e, dpf) -> Error (e, (dpf, Lnk k))
      in
      match e with
      | Er e -> Error (from_crit_err e, (dpf, Lnk k))
      | V v ->
        (match pattern_match_v p1 v to_pe_ex with
         | Ok to_pe_ex -> Ok ((to_pe_ex, dpf), Lnk k)
         | Error e -> Error (e, (dpf, Lnk k)))
      | ThLeaf (kk, e) ->
        pattern_match to_pe_ex (dfs, pe_exprs, kk) fresh (Some k) p1 e |> pm_res_hndl
      | Link k' -> helper_key k'
      | ThTree pat2 ->
        let pe_exprs =
          match k1 with
          | None -> pe_exprs
          | Some k1 -> add k1 (Link k) pe_exprs
        in
        (match patpat_match to_pe_ex (dfs, pe_exprs, kk) fresh pat1 pat2 with
         | Ok ((to_pe_ex, (dfs, pe_exprs, fresh)), pat2') ->
           Ok ((to_pe_ex, (dfs, add k (ThTree pat2') pe_exprs, fresh)), Lnk k)
         | Error (e, ((dfs, pe_exprs, fresh), pat2')) ->
           Error (e, ((dfs, add k (ThTree pat2') pe_exprs, fresh), Lnk k)))
      | LazyLst ll -> ptrnll_match env fresh [] (Some k) ll p |> pm_res_hndl
    in
    helper_key k2
  | P (Some _, _), P (None, pat2) ->
    let p2' = P (Some fresh, pat2) in
    let dfs, pe_exprs, fresh = dfs, add fresh (ThTree pat2) pe_exprs, fresh + 1 in
    ptrnptrn_match to_pe_ex (dfs, pe_exprs, kk) fresh p1 p2'

and ptrnll_match (dfs, pe_exprs, kk) fresh =
  let rec helper dpf to_pe_ex src ll =
    let peconsll_match ((dfs, pe_exprs, fresh) as dpf) to_pe_ex k p11 p12 = function
      | None ->
        let to_pe_ex =
          match k with
          | None -> to_pe_ex
          | Some k -> (k, ThTree (PECons (p11, p12))) :: to_pe_ex
        in
        let v21, pe22 = lazylst_to_cons ll in
        let* to_pe_ex = pattern_match_v p11 v21 to_pe_ex --| fun e -> e, dpf in
        (match pe22 with
         | `V v ->
           (match pattern_match_v p12 v to_pe_ex with
            | Ok to_pe_ex -> Ok (to_pe_ex, dpf)
            | Error e -> Error (e, dpf))
         | `LazyLst ll -> helper dpf to_pe_ex None ll p12)
      | Some pk ->
        let pat2, pe_exprs, fresh = lazylst_to_pat pe_exprs fresh ll in
        let to_pe_ex =
          match k with
          | None -> to_pe_ex
          | Some k -> (k, Link pk) :: to_pe_ex
        in
        (match
           patpat_match to_pe_ex (dfs, pe_exprs, kk) fresh (PECons (p11, p12)) pat2
         with
         | Ok ((to_pe_ex, (dfs, pe_exprs, fresh)), pat2') ->
           Ok (to_pe_ex, (dfs, add pk (ThTree pat2') pe_exprs, fresh))
         | Error (e, ((dfs, pe_exprs, fresh), pat2')) ->
           Error (e, (dfs, add pk (ThTree pat2') pe_exprs, fresh)))
    in
    function
    | Lnk k -> Ok ((k, LazyLst ll) :: to_pe_ex, dpf)
    | P (k, PECons (p1, p2)) -> peconsll_match dpf to_pe_ex k p1 p2 src
    | P (_, PEEnum []) -> Error (`Not_match, dpf)
    | P (k, PEEnum (p :: pp)) -> peconsll_match dpf to_pe_ex k p (P (None, PEEnum pp)) src
    | _ -> Error (`Typing_err, dpf)
  in
  helper (dfs, pe_exprs, fresh)

and eval_var_full ((_, pe_exprs_key) as keys) dpf =
  let* ((dfs, pe_exprs, fresh) as dpf), pe_expr = find_expr dpf keys in
  let result_hndl = function
    | Ok ((dfs, pe_exprs, fresh), v) ->
      Ok ((dfs, add pe_exprs_key (V v) pe_exprs, fresh), v)
    | Error (e, (dfs, pe_exprs, fresh)) ->
      Error (e, (dfs, add pe_exprs_key (Er e) pe_exprs, fresh))
  in
  match pe_expr with
  | Er e -> Error (e, dpf)
  | V v -> Ok (dpf, v)
  | Link pk -> eval_var_full (None, pk) dpf |> result_hndl
  | ThLeaf (kk, e) -> eval_expr_full (dfs, pe_exprs, kk) fresh (e, []) |> result_hndl
  | LazyLst ll -> eval_lazylst dpf ll |> return |> result_hndl
  | ThTree ip ->
    let rec helper_ip ip dpf =
      let helper_pattern = function
        | P (Some k, _) | Lnk k -> eval_var_full (None, k)
        | P (_, ip) -> helper_ip ip
      in
      let helper_pattern_list dpf pp =
        let+ dpf, vv =
          Base.List.fold_result
            ~f:(fun (dpf, vv) p ->
              let+ dpf, v = helper_pattern p dpf in
              dpf, v :: vv)
            ~init:(dpf, [])
            pp
        in
        dpf, List.rev vv
      in
      match ip with
      | PEConst (OrdinaryPConst c) -> Ok (dpf, VConst c)
      | PEConst (NegativePInt i) -> Ok (dpf, VConst (Int (-i)))
      | PETree Nul -> Ok (dpf, VTree Nul)
      | PEMaybe Nothing -> Ok (dpf, VMaybe Nothing)
      | PECons (p1, p2) ->
        let* dpf, v1 = helper_pattern p1 dpf in
        let* dpf, v2 = helper_pattern p2 dpf in
        (match v2 with
         | VList vl -> Ok (dpf, VList (v1 :: vl))
         | _ -> Error (`Typing_err, dpf))
      | PEMaybe (Just p) ->
        let+ dpf, v = helper_pattern p dpf in
        dpf, VMaybe (Just v)
      | PEEnum pp ->
        let+ dpf, vv = helper_pattern_list dpf pp in
        dpf, VList vv
      | PETuple (p1, p2, pp) ->
        let* dpf, v1 = helper_pattern p1 dpf in
        let* dpf, v2 = helper_pattern p2 dpf in
        let+ dpf, vv = helper_pattern_list dpf pp in
        dpf, VTuple (v1, v2, vv)
      | PETree (Node (p1, p2, p3)) ->
        let* dpf, v1 = helper_pattern p1 dpf in
        let* dpf, v2 = helper_pattern p2 dpf in
        let+ dpf, v3 = helper_pattern p3 dpf in
        dpf, VTree (Node (v1, v2, v3))
    in
    helper_ip ip dpf |> result_hndl

and eval_lazylst dpf ll =
  let rec helper v_list ll =
    let h, tl = lazylst_to_cons ll in
    match tl with
    | `LazyLst ll -> helper (h :: v_list) ll
    | _ -> VList (List.rev v_list)
  in
  dpf, helper [] ll

and eval_expr_full_list (dfs, pe_exprs, kk) fresh ee =
  let+ dpf, vv =
    Base.List.fold_result
      ~f:(fun ((dfs, pe_exprs, fresh), vv) e ->
        let+ dpf, v = eval_expr_full (dfs, pe_exprs, kk) fresh e in
        dpf, v :: vv)
      ~init:((dfs, pe_exprs, fresh), [])
      ee
  in
  dpf, List.rev vv

and eval_expr_full ((dfs, pe_exprs, kk) as env) fresh =
  let dpf0 = dfs, pe_exprs, fresh in
  function
  | Const c, _ -> Ok (dpf0, VConst c)
  | ENothing, _ -> Ok (dpf0, VMaybe Nothing)
  | BinTreeBld Nul, _ -> Ok (dpf0, VTree Nul)
  | Identificator (Ident n), _ ->
    let keys = NMap.find n kk in
    eval_var_full keys dpf0
  | TupleBld (e1, e2, ee), _ ->
    let* (dfs, pe_exprs, fresh), v1 = eval_expr_full env fresh e1 in
    let* (dfs, pe_exprs, fresh), v2 = eval_expr_full (dfs, pe_exprs, kk) fresh e2 in
    let+ dpf, vv = eval_expr_full_list (dfs, pe_exprs, kk) fresh ee in
    dpf, VTuple (v1, v2, vv)
  | IfThenEsle (c, th, el), _ ->
    let* (dfs, pe_exprs, fresh), e = eval_step_ite c th el env fresh in
    eval_expr_full (dfs, pe_exprs, kk) fresh e
  | InnerBindings (b, bb, e), _ ->
    let* env, fresh = eval_step_inner_bb (b :: bb) env fresh in
    eval_expr_full env fresh e
  | BinTreeBld (Node (e1, e2, e3)), _ ->
    let* (dfs, pe_exprs, fresh), v1 = eval_expr_full env fresh e1 in
    let* (dfs, pe_exprs, fresh), v2 = eval_expr_full (dfs, pe_exprs, kk) fresh e2 in
    let+ dpf, v3 = eval_expr_full (dfs, pe_exprs, kk) fresh e3 in
    dpf, VTree (Node (v1, v2, v3))
  | ListBld (LazyList (e1, e2, e3)), _ ->
    let+ dpf, pe = elazylist_hndl e1 e2 e3 env fresh in
    (match pe with
     | `V v -> dpf, v
     | `LazyLst ll -> eval_lazylst dpf ll)
  | ListBld (OrdList (IncomprehensionlList ee)), _ ->
    eval_expr_full_list env fresh ee >>| fun (dpf, vv) -> dpf, VList vv
  | Binop (e1, Cons, e2), _ ->
    let* (dfs, pe_exprs, fresh), v1 = eval_expr_full env fresh e1 in
    let* dpf, v2 = eval_expr_full (dfs, pe_exprs, kk) fresh e2 in
    (match v2 with
     | VList vl -> Ok (dpf, VList (v1 :: vl))
     | _ -> Error (`Typing_err, dpf))
  | Case (e, br, brs), _ ->
    let* env, fresh, e = eval_step_case e env fresh (br :: brs) in
    eval_expr_full env fresh e
  | Neg e, _ ->
    let* dpf, v = eval_expr_full env fresh e in
    (match v with
     | VConst (Int x) -> Ok (dpf, VConst (Int (-x)))
     | _ -> Error (`Typing_err, dpf))
  | Binop (e1, op, e2), _ -> eval_arlog env fresh e1 e2 op
  | Lambda (p, pp, e), _ ->
    Ok ((dfs, pe_exprs, fresh), VClosures (kk, [ p, pp, OrdBody e, [] ]))
  | EJust, _ ->
    let (Ident n as id) = Ident (Int.to_string fresh) in
    let bd = OrdBody (FunctionApply ((EJust, []), (Identificator id, []), []), []) in
    Ok
      ( (dfs, pe_exprs, fresh + 1)
      , VClosures
          ( NMap.add n (None, fresh) (thrd env)
          , [ ([], PIdentificator id, []), [], bd, [] ] ) )
  | FunctionApply ((EJust, _), a, []), _ ->
    let+ dpf, v = eval_expr_full (dfs, pe_exprs, kk) fresh a in
    dpf, VMaybe (Just v)
  | FunctionApply ((EJust, _), _, _ :: _), _ -> Error (`Typing_err, dpf0)
  | FunctionApply (f, a, aa), _ ->
    eval_step_funapp env fresh (f, a :: aa)
    >>= (function
     | (dfs, pe_exprs, fresh), `Th (kk, e) -> eval_expr_full (dfs, pe_exprs, kk) fresh e
     | dpf, `V v -> Ok (dpf, v))

and eval_step_ite cond th el env fresh =
  let* dpf, v = eval_expr_full env fresh cond in
  match v with
  | VConst (Bool true) -> Ok (dpf, th)
  | VConst (Bool false) -> Ok (dpf, el)
  | _ -> Error (`Typing_err, (fst env, snd env, fresh))

and eval_step_inner_bb bb env fresh =
  eval_bnds Inner env fresh bb
  --| fun (e, ((dfs, pe_exprs, _), fresh)) -> e, (dfs, pe_exprs, fresh)

and eval_step_guards (dfs, pe_exprs, kk) fresh =
  Base.List.fold_until
    ~f:(fun (dfs, pe_exprs, fresh) (c, br) ->
      match eval_expr_full (dfs, pe_exprs, kk) fresh c with
      | Error _ as er -> Stop er
      | Ok (dpf, v) ->
        (match v with
         | VConst (Bool true) -> Stop (Ok (dpf, br))
         | VConst (Bool false) -> Continue dpf
         | _ -> Stop (Error (`Typing_err, dpf))))
    ~finish:(fun dpf -> Error (`Not_exh, dpf))
    ~init:(dfs, pe_exprs, fresh)

and eval_step_case (e, _) (dfs, pe_exprs, kk) fresh brs =
  let k, fresh, pe_exprs = fresh, fresh + 1, add fresh (ThLeaf (kk, e)) pe_exprs in
  let rec helper brs k ((dfs, pe_exprs, kk) as env) fresh =
    match brs with
    | [] -> Error (`Not_exh, (fst env, snd env, fresh))
    | (p, b) :: brs ->
      let b_hndl b env fresh =
        match b with
        | OrdBody e -> Ok (env, fresh, e)
        | Guards (cb, cbs) ->
          let+ (dfs, pe_exprs, fresh), e = eval_step_guards env fresh (cb :: cbs) in
          (dfs, pe_exprs, kk), fresh, e
      in
      let (kk, _, fresh), p = prep_p (kk, None, fresh) p in
      let ok_lnk (dfs, pe_exprs, kk) fresh k k' =
        b_hndl b (dfs, add k' (Link k) pe_exprs, kk) fresh
      in
      let ok env fresh = b_hndl b env fresh in
      let helper = helper brs k in
      let ff = pattern_match_v, pattern_match, patpat_match ~k:None, ptrnll_match in
      pm_key (dfs, pe_exprs, kk) fresh helper ok_lnk ok k ff p
  in
  helper brs k (dfs, pe_exprs, kk) fresh

and eval_step_funapp ((dfs, pe_exprs, kk0) as env0) fresh = function
  | (Identificator (Ident "seq"), _), [ a1; a2 ] ->
    let+ dpf, _ = eval_expr_full env0 fresh a1 in
    dpf, `Th (kk0, a2)
  | (Identificator (Ident "seq"), _), a1 :: a2 :: a3 :: aa ->
    let+ dpf, _ = eval_expr_full env0 fresh a1 in
    dpf, `Th (kk0, (FunctionApply (a2, a3, aa), []))
  | (Identificator (Ident "print_int"), _), a :: [] ->
    let* dpf, v = eval_expr_full env0 fresh a in
    (match v with
     | VConst (Int x) ->
       Printf.printf "%d\n" x;
       Ok (dpf, `V (VConst Unit))
     | _ -> Error (`Typing_err, dpf))
  | f, aa ->
    let (fresh, pe_exprs), aa' =
      List.fold_left_map
        (fun (fresh, pe_exprs) (a, _) ->
          (fresh + 1, add fresh (ThLeaf (kk0, a)) pe_exprs), fresh)
        (fresh, pe_exprs)
        aa
    in
    let rec es_ord_funcapp f aa' env fresh =
      let* ((dfs, pe_exprs, fresh) as dpf), v = eval_expr_full env fresh f in
      match v with
      | VClosures (kk, dd) ->
        let rec helper dd ((dfs, pe_exprs, _) as env) fresh =
          match dd with
          | [] -> Error (`Not_exh, (dfs, pe_exprs, fresh))
          | (p, pp, bd, bb) :: dd ->
            let rec helper_def ((dfs, pe_exprs, kk) as env) fresh = function
              | [], aa' ->
                (match eval_bnds Inner env fresh bb with
                 | Error (e, ((dfs, pe_exprs, _), fresh)) ->
                   Error (e, (dfs, pe_exprs, fresh))
                 | Ok (env, fresh) ->
                   let* ((dfs, pe_exprs, fresh) as dpf), e =
                     match bd with
                     | OrdBody e -> Ok ((fst env, snd env, fresh), e)
                     | Guards (cb, cbs) -> eval_step_guards env fresh (cb :: cbs)
                   in
                   (match aa' with
                    | [] -> Ok (dpf, `Th (thrd env, e))
                    | aa' -> es_ord_funcapp e aa' (dfs, pe_exprs, thrd env) fresh))
              | p :: pp, [] ->
                Ok ((dfs, pe_exprs, fresh), `V (VClosures (kk, [ p, pp, bd, bb ])))
              | p :: pp, a' :: aa' ->
                let (kk, _, fresh), p' = prep_p (kk, None, fresh) p in
                let ok_lnk (dfs, pe_exprs, kk) fresh k k' =
                  helper_def (dfs, add k' (Link k) pe_exprs, kk) fresh (pp, aa')
                in
                let ok env fresh = helper_def env fresh (pp, aa') in
                let helper = helper dd in
                let ff =
                  pattern_match_v, pattern_match, patpat_match ~k:None, ptrnll_match
                in
                pm_key (dfs, pe_exprs, kk) fresh helper ok_lnk ok a' ff p'
            in
            helper_def env fresh (p :: pp, aa')
        in
        helper dd (dfs, pe_exprs, kk) fresh
      | _ -> Error (`Typing_err, dpf)
    in
    es_ord_funcapp f aa' (dfs, pe_exprs, kk0) fresh

and eval_arlog ((dfs, pe_exprs, kk) as env) fresh e1 e2 =
  let arithm res snd_arg_check =
    let* (dfs, pe_exprs, fresh), v1 = eval_expr_full env fresh e1 in
    let* dpf, v2 = eval_expr_full (dfs, pe_exprs, kk) fresh e2 in
    match v1, v2 with
    | VConst (Int x), VConst (Int y) ->
      snd_arg_check (y, dpf) >>| fun _ -> dpf, VConst (Int (res x y))
    | _ -> Error (`Typing_err, dpf)
  in
  let log res =
    let* (dfs, pe_exprs, fresh), v1 = eval_expr_full env fresh e1 in
    let* dpf, v2 = eval_expr_full (dfs, pe_exprs, kk) fresh e2 in
    match v1, v2 with
    | VConst (Bool x), VConst (Bool y) -> Ok (dpf, VConst (Bool (res x y)))
    | _ -> Error (`Typing_err, dpf)
  in
  let rec ord ((dfs, pe_exprs, fresh) as dpf) src1 src2 ~ac1 ~ac2 pe1 pe2 =
    let cmpr_to_constr = function
      | 0 -> `Eq
      | cmpr -> if cmpr < 0 then `L else `G
    in
    let neg = function
      | `L, dpf -> `G, dpf
      | `G, dpf -> `L, dpf
      | oth -> oth
    in
    let rev () = ord dpf src2 src1 ~ac1:ac2 ~ac2:ac1 pe2 pe1 >>| neg in
    let src_hnd pe_e src pe_exprs = ext_pe_exprs_opt pe_e pe_exprs src in
    let ord_const dpf = function
      | Int x, Int y -> Ok (Int.compare x y |> cmpr_to_constr, dpf)
      | Bool x, Bool y -> Ok (Bool.compare x y |> cmpr_to_constr, dpf)
      | Unit, Unit -> Ok (`Eq, dpf)
      | _ -> Error ((`Typing_err : crit_err), dpf)
    in
    let pattern_hnd_l ((_, pe_exprs, _) as dpf) = function
      | Lnk k | P (Some k, _) -> ord dpf (Some k) None ~ac1:false ~ac2 (find k pe_exprs)
      | P (None, pat) -> ord dpf None None ~ac1 ~ac2 (ThTree pat)
    in
    let econst_before c = src_hnd (V (VConst c)) in
    let enothing_before = src_hnd (V (VMaybe Nothing)) in
    let enul_before = src_hnd (V (VTree Nul)) in
    let eempty_before = src_hnd (V (VList [])) in
    let ac_hnd src ~ac pe_ex =
      match src, ac with
      | Some k, true ->
        fun (fst, (dfs, pe_exprs, fresh)) -> fst, (dfs, add k pe_ex pe_exprs, fresh)
      | _ -> Fun.id
    in
    let ord_list ?(strict_len = true) inner_call =
      let rec helper dpf = function
        | hd1 :: tl1, hd2 :: tl2 ->
          inner_call dpf hd1 hd2
          >>= (function
           | `Eq, dpf -> helper dpf (tl1, tl2)
           | res -> return res)
        | [], [] -> return (`Eq, dpf)
        | [], _ :: _ ->
          if strict_len then fail ((`Typing_err : crit_err), dpf) else return (`L, dpf)
        | _ :: _, [] ->
          if strict_len then fail ((`Typing_err : crit_err), dpf) else return (`G, dpf)
      in
      helper
    in
    let inner_call_ee kk1 kk2 dpf (e1, _) (e2, _) =
      ord dpf None None ~ac1 ~ac2 (ThLeaf (kk1, e1)) (ThLeaf (kk2, e2))
    in
    let inner_call_ev kk dpf (e, _) v =
      ord dpf None None ~ac1 ~ac2 (ThLeaf (kk, e)) (V v)
    in
    let inner_call_pe kk dpf p (e, _) = pattern_hnd_l dpf p (ThLeaf (kk, e)) in
    let inner_call_vv dpf v1 v2 = ord dpf None None ~ac1 ~ac2 (V v1) (V v2) in
    let inner_call_pv dpf p v = pattern_hnd_l dpf p (V v) in
    let inner_call_pp ((_, pe_exprs, _) as dpf) p1 p2 =
      let pattern_hnd ac = function
        | Lnk k | P (Some k, _) -> Some k, false, find k pe_exprs
        | P (None, pat) -> None, ac, ThTree pat
      in
      let src1, ac1, pe1 = pattern_hnd ac1 p1 in
      let src2, ac2, pe2 = pattern_hnd ac2 p2 in
      ord dpf src1 src2 ~ac1 ~ac2 pe1 pe2
    in
    let ll_none_hnd_r inner_call1 inner_call2 ll =
      let hd, tl = lazylst_to_cons ll |> conv_res in
      inner_call1 dpf (V hd)
      >>= function
      | `Eq, dpf -> inner_call2 dpf tl
      | res -> return res
    in
    let ord_e_ll kk e1 e2 =
      let inner_call_e e dpf = ord dpf None None ~ac1 ~ac2 (ThLeaf (kk, e)) in
      ll_none_hnd_r (inner_call_e e1) (inner_call_e e2)
    in
    let ord_p_ll p1 p2 =
      ll_none_hnd_r (fun dpf -> pattern_hnd_l dpf p1) (fun dpf -> pattern_hnd_l dpf p2)
    in
    let inner_call_v v dpf = ord dpf None None ~ac1 ~ac2 (V v) in
    let ord_v_ll v1 v2 = ll_none_hnd_r (inner_call_v v1) (inner_call_v v2) in
    let ord_ll_ll ll =
      let hd, tl = lazylst_to_cons ll |> conv_res in
      ll_none_hnd_r (inner_call_v hd) (fun dpf -> ord dpf None None ~ac1 ~ac2 tl)
    in
    let err_hnd = function
      | Ok ok -> ok
      | Error (e, (dfs, pe_exprs, fresh)) ->
        (dfs, src_hnd (Er e) src1 pe_exprs, fresh), Er e
    in
    let complex_hnd res =
      let dpf, pe1' = err_hnd res in
      ord dpf src1 src2 ~ac1:true ~ac2 pe1' pe2
    in
    match (pe1, src1), (src2, pe2) with
    | (Link k, _), _ -> ord dpf (Some k) src2 ~ac1:false ~ac2 (find k pe_exprs) pe2
    | (Er e, _), (_, ThLeaf (_, Const c)) ->
      fail (e, (dfs, econst_before c src2 pe_exprs, fresh))
    | (Er e, _), (_, ThLeaf (_, ENothing)) ->
      fail (e, (dfs, enothing_before src2 pe_exprs, fresh))
    | (Er e, _), (_, ThLeaf (_, BinTreeBld Nul)) ->
      fail (e, (dfs, enul_before src2 pe_exprs, fresh))
    | (Er e, _), (_, ThLeaf (_, ListBld (OrdList (IncomprehensionlList [])))) ->
      fail (e, (dfs, eempty_before src2 pe_exprs, fresh))
    | (Er e, _), (_, (ThLeaf (_, _) | LazyLst _)) ->
      (e, dpf) |> ac_hnd src2 ~ac:ac2 pe2 |> fail
    | (Er e, _), _ -> fail (e, dpf)
    | ( (ThTree (PEConst (NegativePInt x)), _)
      , (_, (ThLeaf (_, Const _) | ThTree (PEConst _) | V (VConst _))) ) ->
      ord dpf src1 src2 ~ac1 ~ac2 (ThTree (PEConst (OrdinaryPConst (Int (-x))))) pe2
    | (ThLeaf (_, Const c1), _), (_, ThLeaf (_, Const c2)) ->
      ord_const
        (dfs, econst_before c1 src1 @@ econst_before c2 src2 pe_exprs, fresh)
        (c1, c2)
    | ( (ThLeaf (_, Const c1), _)
      , (_, (V (VConst c2) | ThTree (PEConst (OrdinaryPConst c2)))) ) ->
      ord_const (dfs, econst_before c1 src1 pe_exprs, fresh) (c1, c2)
    | ( ((V (VConst c1) | ThTree (PEConst (OrdinaryPConst c1))), _)
      , (_, (V (VConst c2) | ThTree (PEConst (OrdinaryPConst c2)))) ) ->
      ord_const (dfs, pe_exprs, fresh) (c1, c2)
    | ( (ThLeaf (_, FunctionApply ((EJust, _), _, [])), _)
      , (_, (V (VMaybe Nothing) | ThTree (PEMaybe Nothing))) )
    | (ThLeaf (_, BinTreeBld (Node _)), _), (_, (V (VTree Nul) | ThTree (PETree Nul)))
    | ( ( ( ThLeaf
              (_, (ListBld (OrdList (IncomprehensionlList (_ :: _))) | Binop (_, Cons, _)))
          | LazyLst _ )
        , _ )
      , (_, (ThTree (PEEnum []) | V (VList []))) ) ->
      (`G, dpf) |> ac_hnd src1 ~ac:ac1 pe1 |> return
    | (ThLeaf (_, FunctionApply ((EJust, _), _, [])), _), (_, ThLeaf (_, ENothing)) ->
      (`G, (dfs, enothing_before src2 pe_exprs, fresh))
      |> ac_hnd src1 ~ac:ac1 pe1
      |> return
    | (ThLeaf (_, BinTreeBld (Node _)), _), (_, ThLeaf (_, BinTreeBld Nul)) ->
      (`G, (dfs, enul_before src2 pe_exprs, fresh)) |> ac_hnd src1 ~ac:ac1 pe1 |> return
    | ( ( ( ThLeaf
              (_, (ListBld (OrdList (IncomprehensionlList (_ :: _))) | Binop (_, Cons, _)))
          | LazyLst _ )
        , _ )
      , (_, ThLeaf (_, ListBld (OrdList (IncomprehensionlList [])))) ) ->
      (`G, (dfs, eempty_before src2 pe_exprs, fresh)) |> ac_hnd src1 ~ac:ac1 pe1 |> return
    | ( (ThLeaf (_, FunctionApply ((EJust, _), e, [])), Some k)
      , ( _
        , ( ThLeaf (_, FunctionApply ((EJust, _), _, []))
          | ThTree (PEMaybe (Just _))
          | V (VMaybe (Just _)) ) ) ) ->
      let (pe_exprs, fresh), p1' = new_th kk fresh pe_exprs e in
      let pe1' = ThTree (PEMaybe (Just p1')) in
      let dpf = dfs, add k pe1' pe_exprs, fresh in
      ord dpf None src2 ~ac1 ~ac2 pe1' pe2
    | ( (ThLeaf (_, BinTreeBld (Node (e1, e2, e3))), Some k)
      , ( _
        , (ThLeaf (_, BinTreeBld (Node _)) | V (VTree (Node _)) | ThTree (PETree (Node _)))
        ) ) ->
      let pat', pe_exprs, fresh = enode_to_pat pe_exprs fresh kk e1 e2 e3 in
      let pe1' = ThTree pat' in
      let dpf = dfs, add k pe1' pe_exprs, fresh in
      ord dpf None src2 ~ac1 ~ac2 pe1' pe2
    | ( (ThLeaf (_, ListBld (OrdList (IncomprehensionlList (_ :: _ as ee)))), Some k)
      , ( _
        , ( ThLeaf
              (_, (ListBld (OrdList (IncomprehensionlList (_ :: _))) | Binop (_, Cons, _)))
          | LazyLst _
          | ThTree (PECons _ | PEEnum (_ :: _))
          | V (VList (_ :: _)) ) ) ) ->
      let (pe_exprs, fresh), pp' =
        List.fold_left_map (fun (pe, f) -> new_th kk f pe) (pe_exprs, fresh) ee
      in
      let pe1' = ThTree (PEEnum pp') in
      let dpf = dfs, add k pe1' pe_exprs, fresh in
      ord dpf None src2 ~ac1 ~ac2 pe1' pe2
    | ( (LazyLst ll, Some k)
      , ( _
        , ( ThLeaf
              (_, (ListBld (OrdList (IncomprehensionlList (_ :: _))) | Binop (_, Cons, _)))
          | LazyLst _
          | ThTree (PECons _ | PEEnum (_ :: _))
          | V (VList (_ :: _)) ) ) ) ->
      let pat', pe_exprs, fresh = lazylst_to_pat pe_exprs fresh ll in
      let pe1' = ThTree pat' in
      let dpf = dfs, add k pe1' pe_exprs, fresh in
      ord dpf None src2 ~ac1 ~ac2 pe1' pe2
    | ( (ThLeaf (_, Binop (e1, Cons, e2)), Some k)
      , ( _
        , ( ThLeaf
              (_, (ListBld (OrdList (IncomprehensionlList (_ :: _))) | Binop (_, Cons, _)))
          | LazyLst _
          | ThTree (PECons _ | PEEnum (_ :: _))
          | V (VList (_ :: _)) ) ) ) ->
      let pat', pe_exprs, fresh = econs_to_pat pe_exprs fresh kk e1 e2 in
      let pe1' = ThTree pat' in
      let dpf = dfs, add k pe1' pe_exprs, fresh in
      ord dpf None src2 ~ac1 ~ac2 pe1' pe2
    | ( (ThLeaf (_, TupleBld (e1, e2, ee)), Some k)
      , (_, (ThLeaf (_, TupleBld _) | V (VTuple _) | ThTree (PETuple _))) ) ->
      let pat', pe_exprs, fresh = etuple_to_pat pe_exprs fresh kk e1 e2 ee in
      let pe1' = ThTree pat' in
      let dpf = dfs, add k pe1' pe_exprs, fresh in
      ord dpf None src2 ~ac1 ~ac2 pe1' pe2
    | ((ThTree (PEMaybe (Just _)) | V (VMaybe (Just _))), _), (_, ThLeaf (_, ENothing)) ->
      return (`G, (dfs, enothing_before src2 pe_exprs, fresh))
    | ((V (VTree (Node _)) | ThTree (PETree (Node _))), _), (_, ThLeaf (_, BinTreeBld Nul))
      -> return (`G, (dfs, enul_before src2 pe_exprs, fresh))
    | ( ((ThTree (PECons _ | PEEnum (_ :: _)) | V (VList (_ :: _))), _)
      , (_, ThLeaf (_, ListBld (OrdList (IncomprehensionlList [])))) ) ->
      return (`G, (dfs, eempty_before src2 pe_exprs, fresh))
    | (ThLeaf (_, ENothing), _), (_, ThLeaf (_, ENothing)) ->
      return (`Eq, (dfs, enothing_before src2 @@ enothing_before src1 pe_exprs, fresh))
    | (ThLeaf (_, BinTreeBld Nul), _), (_, ThLeaf (_, BinTreeBld Nul)) ->
      return (`Eq, (dfs, enul_before src2 @@ enul_before src1 pe_exprs, fresh))
    | ( (ThLeaf (_, ListBld (OrdList (IncomprehensionlList []))), _)
      , (_, ThLeaf (_, ListBld (OrdList (IncomprehensionlList [])))) ) ->
      return (`Eq, (dfs, eempty_before src2 @@ eempty_before src1 pe_exprs, fresh))
    | ( ((ThTree (PEMaybe (Just _)) | V (VMaybe (Just _))), _)
      , (_, (ThTree (PEMaybe Nothing) | V (VMaybe Nothing))) )
    | ( ((V (VTree (Node _)) | ThTree (PETree (Node _))), _)
      , (_, (V (VTree Nul) | ThTree (PETree Nul))) )
    | ( ((ThTree (PECons _ | PEEnum (_ :: _)) | V (VList (_ :: _))), _)
      , (_, (ThTree (PEEnum []) | V (VList []))) ) -> return (`G, dpf)
    | ((ThTree (PEMaybe Nothing) | V (VMaybe Nothing)), _), (_, ThLeaf (_, ENothing)) ->
      return (`Eq, (dfs, enothing_before src2 pe_exprs, fresh))
    | ((V (VTree Nul) | ThTree (PETree Nul)), _), (_, ThLeaf (_, BinTreeBld Nul)) ->
      return (`Eq, (dfs, enul_before src2 pe_exprs, fresh))
    | ( ((ThTree (PEEnum []) | V (VList [])), _)
      , (_, ThLeaf (_, ListBld (OrdList (IncomprehensionlList [])))) ) ->
      return (`Eq, (dfs, eempty_before src2 pe_exprs, fresh))
    | ( ((ThTree (PEMaybe Nothing) | V (VMaybe Nothing)), _)
      , (_, (ThTree (PEMaybe Nothing) | V (VMaybe Nothing))) )
    | ( ((V (VTree Nul) | ThTree (PETree Nul)), _)
      , (_, (V (VTree Nul) | ThTree (PETree Nul))) )
    | ((ThTree (PEEnum []) | V (VList [])), _), (_, (ThTree (PEEnum []) | V (VList [])))
      -> return (`Eq, dpf)
    | ( (ThLeaf (kk1, FunctionApply ((EJust, _), e1, [])), None)
      , (None, ThLeaf (kk2, FunctionApply ((EJust, _), e2, []))) ) ->
      inner_call_ee kk1 kk2 dpf e1 e2
    | ( (ThLeaf (kk1, BinTreeBld (Node (e11, e12, e13))), None)
      , (None, ThLeaf (kk2, BinTreeBld (Node (e21, e22, e23)))) ) ->
      ord_list (inner_call_ee kk1 kk2) dpf ([ e11; e12; e13 ], [ e21; e22; e23 ])
    | ( (ThLeaf (kk1, ListBld (OrdList (IncomprehensionlList (_ :: _ as ee1)))), None)
      , (None, ThLeaf (kk2, ListBld (OrdList (IncomprehensionlList (_ :: _ as ee2))))) )
      -> ord_list ~strict_len:false (inner_call_ee kk1 kk2) dpf (ee1, ee2)
    | (LazyLst ll1, None), (None, LazyLst ll2) -> ord_ll_ll ll1 ll2
    | ( (ThLeaf (kk1, Binop (e11, Cons, e12)), None)
      , (None, ThLeaf (kk2, Binop (e21, Cons, e22))) ) ->
      ord_list (inner_call_ee kk1 kk2) dpf ([ e11; e12 ], [ e21; e22 ])
    | (ThLeaf (kk, Binop ((e1, _), Cons, (e2, _))), None), (None, LazyLst ll) ->
      ord_e_ll kk e1 e2 ll
    | ( (ThLeaf (kk1, Binop (e11, Cons, e12)), None)
      , (None, ThLeaf (kk2, ListBld (OrdList (IncomprehensionlList (e21 :: ee2))))) ) ->
      let e22 = ListBld (OrdList (IncomprehensionlList ee2)), [] in
      ord_list (inner_call_ee kk1 kk2) dpf ([ e11; e12 ], [ e21; e22 ])
    | ( (ThLeaf (kk, ListBld (OrdList (IncomprehensionlList ((e, _) :: ee)))), None)
      , (None, LazyLst ll) ) ->
      ord_e_ll kk e (ListBld (OrdList (IncomprehensionlList ee))) ll
    | ( (ThLeaf (kk1, TupleBld (e11, e12, ee1)), None)
      , (None, ThLeaf (kk2, TupleBld (e21, e22, ee2))) ) ->
      ord_list (inner_call_ee kk1 kk2) dpf (e11 :: e12 :: ee1, e21 :: e22 :: ee2)
    | (ThLeaf (kk, FunctionApply ((EJust, _), (e, _), [])), None), (_, V (VMaybe (Just v)))
      -> ord dpf None None ~ac1 ~ac2 (ThLeaf (kk, e)) (V v)
    | ( (ThLeaf (kk, BinTreeBld (Node (e1, e2, e3))), None)
      , (_, V (VTree (Node (v1, v2, v3)))) ) ->
      ord_list (inner_call_ev kk) dpf ([ e1; e2; e3 ], [ v1; v2; v3 ])
    | ( (ThLeaf (kk, ListBld (OrdList (IncomprehensionlList (_ :: _ as ee)))), None)
      , (_, V (VList (_ :: _ as vv))) ) ->
      ord_list ~strict_len:false (inner_call_ev kk) dpf (ee, vv)
    | (LazyLst ll, None), (_, V (VList (v :: vv))) -> ord_v_ll v (VList vv) ll
    | (ThLeaf (kk, Binop (e1, Cons, e2)), None), (_, V (VList (v :: vv))) ->
      ord_list (inner_call_ev kk) dpf ([ e1; e2 ], [ v; VList vv ])
    | (ThLeaf (kk, TupleBld (e1, e2, ee)), None), (_, V (VTuple (v1, v2, vv))) ->
      ord_list (inner_call_ev kk) dpf (e1 :: e2 :: ee, v1 :: v2 :: vv)
    | (V (VMaybe (Just v1)), _), (_, V (VMaybe (Just v2))) -> inner_call_vv dpf v1 v2
    | (V (VTree (Node (v11, v12, v13))), _), (_, V (VTree (Node (v21, v22, v23)))) ->
      ord_list inner_call_vv dpf ([ v11; v12; v13 ], [ v21; v22; v23 ])
    | (V (VList (_ :: _ as vv1)), _), (_, V (VList (_ :: _ as vv2))) ->
      ord_list ~strict_len:false inner_call_vv dpf (vv1, vv2)
    | (V (VTuple (v11, v12, vv1)), _), (_, V (VTuple (v21, v22, vv2))) ->
      ord_list inner_call_vv dpf (v11 :: v12 :: vv1, v21 :: v22 :: vv2)
    | ( (ThTree (PEMaybe (Just p)), _)
      , (None, ThLeaf (kk, FunctionApply ((EJust, _), e, []))) ) ->
      inner_call_pe kk dpf p e
    | ( (ThTree (PETree (Node (p1, p2, p3))), _)
      , (None, ThLeaf (kk, BinTreeBld (Node (e1, e2, e3)))) ) ->
      ord_list (inner_call_pe kk) dpf ([ p1; p2; p3 ], [ e1; e2; e3 ])
    | (ThTree (PECons (p1, p2)), _), (None, ThLeaf (kk, Binop (e1, Cons, e2))) ->
      ord_list (inner_call_pe kk) dpf ([ p1; p2 ], [ e1; e2 ])
    | ( (ThTree (PECons (p1, p2)), _)
      , (None, ThLeaf (kk, ListBld (OrdList (IncomprehensionlList (e1 :: ee))))) ) ->
      let e2 = ListBld (OrdList (IncomprehensionlList ee)), [] in
      ord_list (inner_call_pe kk) dpf ([ p1; p2 ], [ e1; e2 ])
    | (ThTree (PECons (p1, p2)), _), (None, LazyLst ll) -> ord_p_ll p1 p2 ll
    | ( (ThTree (PEEnum (_ :: _ as pp)), _)
      , (None, ThLeaf (kk, ListBld (OrdList (IncomprehensionlList (_ :: _ as ee))))) ) ->
      ord_list ~strict_len:false (inner_call_pe kk) dpf (pp, ee)
    | (ThTree (PEEnum (p :: pp)), _), (None, LazyLst ll) ->
      ord_p_ll p (P (None, PEEnum pp)) ll
    | (ThTree (PEEnum (p1 :: pp)), _), (None, ThLeaf (kk, Binop (e1, Cons, e2))) ->
      ord_list (inner_call_pe kk) dpf ([ p1; P (None, PEEnum pp) ], [ e1; e2 ])
    | (ThTree (PETuple (p1, p2, pp)), _), (None, ThLeaf (kk, TupleBld (e1, e2, ee))) ->
      ord_list (inner_call_pe kk) dpf (p1 :: p2 :: pp, e1 :: e2 :: ee)
    | (ThTree (PEMaybe (Just p1)), _), (_, ThTree (PEMaybe (Just p2))) ->
      inner_call_pp dpf p1 p2
    | ( (ThTree (PETree (Node (p11, p12, p13))), _)
      , (_, ThTree (PETree (Node (p21, p22, p23)))) ) ->
      ord_list inner_call_pp dpf ([ p11; p12; p13 ], [ p21; p22; p23 ])
    | (ThTree (PECons (p11, p12)), _), (_, ThTree (PECons (p21, p22))) ->
      ord_list inner_call_pp dpf ([ p11; p12 ], [ p21; p22 ])
    | (ThTree (PECons (p1, p2)), _), (_, ThTree (PEEnum (p :: pp))) ->
      ord_list inner_call_pp dpf ([ p1; p2 ], [ p; P (None, PEEnum pp) ])
    | (ThTree (PEEnum (_ :: _ as pp1)), _), (_, ThTree (PEEnum (_ :: _ as pp2))) ->
      ord_list ~strict_len:false inner_call_pp dpf (pp1, pp2)
    | (ThTree (PETuple (p11, p12, pp1)), _), (_, ThTree (PETuple (p21, p22, pp2))) ->
      ord_list inner_call_pp dpf (p11 :: p12 :: pp1, p21 :: p22 :: pp2)
    | (ThTree (PEMaybe (Just p)), _), (_, V (VMaybe (Just v))) -> inner_call_pv dpf p v
    | (ThTree (PETree (Node (p1, p2, p3))), _), (_, V (VTree (Node (v1, v2, v3)))) ->
      ord_list inner_call_pv dpf ([ p1; p2; p3 ], [ v1; v2; v3 ])
    | (ThTree (PEEnum (_ :: _ as pp)), _), (_, V (VList (_ :: _ as vv))) ->
      ord_list ~strict_len:false inner_call_pv dpf (pp, vv)
    | (ThTree (PECons (p1, p2)), _), (_, V (VList (v :: vv))) ->
      ord_list inner_call_pv dpf ([ p1; p2 ], [ v; VList vv ])
    | (ThTree (PETuple (p1, p2, pp)), _), (_, V (VTuple (v1, v2, vv))) ->
      ord_list inner_call_pv dpf (p1 :: p2 :: pp, v1 :: v2 :: vv)
    | ( (ThLeaf (kk, ListBld (LazyList (e1, e2, e3))), _)
      , ( _
        , ( ThLeaf (_, (ListBld _ | Binop (_, Cons, _)))
          | LazyLst _
          | ThTree (PECons _ | PEEnum _)
          | V (VList _) ) ) ) ->
      let (dfs, pe_exprs, fresh), pe1' =
        elazylist_hndl e1 e2 e3 (dfs, pe_exprs, kk) fresh >>| conv_res |> err_hnd
      in
      let pe_exprs =
        match pe1' with
        | V _ -> src_hnd pe1' src1 pe_exprs
        | _ -> pe_exprs
      in
      ord (dfs, pe_exprs, fresh) src1 src2 ~ac1:true ~ac2 pe1' pe2
    | _, (_, Er _)
    | _, (_, Link _)
    | ( ((ThLeaf (_, Const _) | ThTree (PEConst _) | V (VConst _)), _)
      , (_, (ThLeaf (_, Const _) | ThTree (PEConst _))) )
    | ( ( ( ThLeaf (_, (FunctionApply ((EJust, _), _, []) | ENothing))
          | V (VMaybe _)
          | ThTree (PEMaybe _) )
        , _ )
      , ( _
        , ( ThLeaf (_, FunctionApply ((EJust, _), _, []))
          | V (VMaybe _)
          | ThTree (PEMaybe _) ) ) )
    | ( ((ThLeaf (_, BinTreeBld _) | V (VTree _) | ThTree (PETree _)), _)
      , (_, (ThLeaf (_, BinTreeBld _) | V (VTree _) | ThTree (PETree _))) )
    | ( ((ThLeaf (_, TupleBld _) | V (VTuple _) | ThTree (PETuple _)), _)
      , (_, (ThLeaf (_, TupleBld _) | ThTree (PETuple _))) )
    | ( ( ( ThLeaf (_, (ListBld _ | Binop (_, Cons, _)))
          | LazyLst _
          | V (VList _)
          | ThTree (PECons _ | PEEnum _) )
        , _ )
      , ( _
        , ( ThLeaf (_, (ListBld _ | Binop (_, Cons, _)))
          | LazyLst _
          | V (VList _)
          | ThTree (PECons _ | PEEnum _) ) ) ) -> rev ()
    | (ThLeaf (kk, Identificator (Ident n)), _), _ ->
      let ((_, pe_exprs_key) as keys) = NMap.find n kk in
      let dpf, pe1' =
        find_expr dpf keys
        >>| (fun ((dfs, pe_exprs, fresh), pe1') ->
              (dfs, src_hnd (Link pe_exprs_key) src1 pe_exprs, fresh), pe1')
        |> err_hnd
      in
      ord dpf (Some pe_exprs_key) src2 ~ac1:false ~ac2 pe1' pe2
    | (ThLeaf (kk, IfThenEsle (c, th, el)), _), _ ->
      eval_step_ite c th el (dfs, pe_exprs, kk) fresh
      >>| (fun (dpf, (e, _)) -> dpf, ThLeaf (kk, e))
      |> complex_hnd
    | (ThLeaf (kk, Case (e, br, brs)), _), _ ->
      eval_step_case e (dfs, pe_exprs, kk) fresh (br :: brs)
      >>| (fun ((dfs, pe_exprs, kk), fresh, (e, _)) ->
            (dfs, pe_exprs, fresh), ThLeaf (kk, e))
      |> complex_hnd
    | (ThLeaf (kk, FunctionApply (f, a, aa)), _), _ ->
      eval_step_funapp (dfs, pe_exprs, kk) fresh (f, a :: aa) >>| conv_res |> complex_hnd
    | (ThLeaf (kk, InnerBindings (b, bb, (e, _))), _), _ ->
      eval_step_inner_bb (b :: bb) (dfs, pe_exprs, kk) fresh
      >>| (fun ((dfs, pe_exprs, kk), fresh) -> (dfs, pe_exprs, fresh), ThLeaf (kk, e))
      |> complex_hnd
    | (ThLeaf (kk, Binop (e1, op, e2)), _), _ when op <> Cons ->
      eval_arlog (dfs, pe_exprs, kk) fresh e1 e2 op
      >>| (fun (dpf, v) -> dpf, V v)
      |> complex_hnd
    | (ThLeaf (kk, Neg e), _), _ ->
      eval_expr_full (dfs, pe_exprs, kk) fresh e
      >>| (fun (dpf, v) -> dpf, V v)
      |> complex_hnd
    | ( _
      , ( _
        , ThLeaf
            ( _
            , ( IfThenEsle _
              | Case _
              | FunctionApply _
              | InnerBindings _
              | Binop _
              | Neg _
              | Identificator _ ) ) ) ) -> rev ()
    | _ ->
      ord dpf src1 src2 ~ac1 ~ac2 (Er `Typing_err) pe1
      --= fun (_, dpf) -> ord dpf src2 src1 ~ac2 ~ac1 (Er `Typing_err) pe2
  in
  let tru dpf = dpf, VConst (Bool true) in
  let fls dpf = dpf, VConst (Bool false) in
  let ord () =
    ord
      (dfs, pe_exprs, fresh)
      None
      None
      ~ac1:false
      ~ac2:false
      (ThLeaf (kk, Stdlib.fst e1))
      (ThLeaf (kk, Stdlib.fst e2))
  in
  function
  | Plus -> arithm ( + ) return
  | Minus -> arithm ( - ) return
  | Divide ->
    arithm ( / ) (function
      | 0, dpf -> Error (`Div_by_zero, dpf)
      | _ -> Ok ())
  | Mod ->
    arithm Int.rem (function
      | 0, dpf -> Error (`Div_by_zero, dpf)
      | _ -> Ok ())
  | Multiply -> arithm ( * ) return
  | Pow ->
    arithm Base.Int.pow (fun (y, dpf) ->
      if y < 0 then Error (`Negative_exponent, dpf) else Ok ())
  | And -> log ( && )
  | Or -> log ( || )
  | Less ->
    ord ()
    >>| (function
     | `L, dpf -> tru dpf
     | _, dpf -> fls dpf)
  | Greater ->
    ord ()
    >>| (function
     | `G, dpf -> tru dpf
     | _, dpf -> fls dpf)
  | EqualityOrLess ->
    ord ()
    >>| (function
     | `G, dpf -> fls dpf
     | _, dpf -> tru dpf)
  | EqualityOrGreater ->
    ord ()
    >>| (function
     | `L, dpf -> fls dpf
     | _, dpf -> tru dpf)
  | Equality ->
    ord ()
    >>| (function
     | `Eq, dpf -> tru dpf
     | _, dpf -> fls dpf)
  | Inequality ->
    ord ()
    >>| (function
     | `Eq, dpf -> fls dpf
     | _, dpf -> tru dpf)
  | Cons -> Error ((`Typing_err : crit_err), (dfs, pe_exprs, fresh))

and elazylist_hndl fst snd lst ((_, _, kk) as env) fresh =
  let typing_err dpf = Error ((`Typing_err : crit_err), dpf) in
  let e_to_val v_hndl ((dfs, pe_exprs, fresh) as dpf) = function
    | None -> Ok (dpf, None)
    | Some e -> eval_expr_full (dfs, pe_exprs, kk) fresh e >>= v_hndl
  in
  let int_ll dpf start =
    let e_to_val =
      e_to_val
      @@ function
      | dpf, VConst (Int i) -> Ok (dpf, Some i)
      | dpf, _ -> typing_err dpf
    in
    let* dpf, snd_opt = e_to_val dpf snd in
    let+ dpf, fin_opt = e_to_val dpf lst in
    let open Z in
    ( dpf
    , match snd_opt with
      | Some snd ->
        let step = of_int snd - of_int start in
        let incr = step >= zero in
        (match fin_opt with
         | Some fin ->
           if of_int fin - of_int start >= zero <> incr
           then `V (VList [])
           else `LazyLst (IntLL (start, step, fin))
         | None ->
           let fin = if incr then Int.max_int else Int.min_int in
           `LazyLst (IntLL (start, step, fin)))
      | None ->
        let incr, fin =
          match fin_opt with
          | None -> true, Int.max_int
          | Some fin -> of_int fin - of_int start >= zero, fin
        in
        let step = if incr then Z.one else -Z.one in
        `LazyLst (IntLL (start, step, fin)) )
  in
  let bool_ll dpf start =
    let t, f = VConst (Bool true), VConst (Bool false) in
    let e_to_val =
      e_to_val
      @@ function
      | dpf, VConst (Bool b) -> Ok (dpf, Some b)
      | dpf, _ -> typing_err dpf
    in
    let* dpf, snd_opt = e_to_val dpf snd in
    let+ dpf, fin_opt = e_to_val dpf lst in
    ( dpf
    , match start, snd_opt, fin_opt with
      | true, None, (None | Some true) | true, Some false, Some true -> `V (VList [ t ])
      | false, (None | Some true), Some false -> `V (VList [ f ])
      | false, (None | Some true), (None | Some true) -> `V (VList [ f; t ])
      | true, Some false, (None | Some false) | true, None, Some false ->
        `V (VList [ t; f ])
      | true, Some true, (None | Some true) -> `LazyLst (BoolLL true)
      | false, Some false, _ -> `LazyLst (BoolLL false)
      | true, Some true, Some false -> `V (VList []) )
  in
  let unit_ll dpf =
    let e_to_val =
      e_to_val
      @@ function
      | dpf, VConst Unit -> Ok (dpf, Some Unit)
      | dpf, _ -> typing_err dpf
    in
    let* dpf, snd_opt = e_to_val dpf snd in
    let+ dpf, _ = e_to_val dpf lst in
    ( dpf
    , match snd_opt with
      | None -> `V (VList [ VConst Unit ])
      | Some _ -> `LazyLst UnitLL )
  in
  eval_expr_full env fresh fst
  >>= function
  | dpf, VConst (Int i) -> int_ll dpf i
  | dpf, VConst (Bool b) -> bool_ll dpf b
  | dpf, VConst Unit -> unit_ll dpf
  | dpf, _ -> typing_err dpf

and lazylst_to_cons = function
  | BoolLL b as bb -> VConst (Bool b), `LazyLst bb
  | UnitLL as uu -> VConst Unit, `LazyLst uu
  | IntLL (start, step, fin) ->
    let open Z in
    let start' = of_int start + step in
    let pe =
      if step >= zero <> (of_int fin - start' >= zero)
      then `V (VList [])
      else `LazyLst (IntLL (Z.to_int start', step, fin))
    in
    VConst (Int start), pe

and lazylst_to_pat pe_exprs fresh ll =
  let v21, pe22 = lazylst_to_cons ll |> conv_res in
  let p21, pe_exprs, fresh = Lnk fresh, add fresh (V v21) pe_exprs, fresh + 1 in
  let p22, pe_exprs, fresh = Lnk fresh, add fresh pe22 pe_exprs, fresh + 1 in
  PECons (p21, p22), pe_exprs, fresh
;;

let eval = eval_bnds TopLevel
