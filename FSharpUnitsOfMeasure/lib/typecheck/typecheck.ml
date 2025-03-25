(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Types = struct
  open Ast

  type ty =
    | TVar of int
    | TInt
    | TFloat
    | TBool
    | TChar
    | TString
    | TFun of ty * ty
    | TTuple of ty list
    | TList of ty
    | TMeasure of ty * measure option
    | TOption of ty

  type scheme = Scheme of int list * ty

  module IntMap = Map.Make (struct
      type t = int

      let compare = compare
    end)

  type subst = ty IntMap.t

  let empty_subst : subst = IntMap.empty
  let fresh_ty_var counter = TVar counter, counter + 1

  let rec apply_subst (s : subst) (t : ty) : ty =
    match t with
    | TVar n ->
      (try
         let t' = IntMap.find n s in
         apply_subst s t'
       with
       | Not_found -> TVar n)
    | TFun (t1, t2) -> TFun (apply_subst s t1, apply_subst s t2)
    | TTuple ts -> TTuple (List.map (apply_subst s) ts)
    | TList t' -> TList (apply_subst s t')
    | TMeasure (t', m) -> TMeasure (apply_subst s t', m)
    | TOption t' -> TOption (apply_subst s t')
    | _ -> t
  ;;

  let apply_subst_scheme (s : subst) (Scheme (vars, t)) =
    let s' = IntMap.filter (fun k _ -> not (List.mem k vars)) s in
    Scheme (vars, apply_subst s' t)
  ;;

  let compose_subst (s1 : subst) (s2 : subst) : subst =
    let s2' = IntMap.map (fun t -> apply_subst s1 t) s2 in
    IntMap.union (fun _ x _ -> Some x) s1 s2'
  ;;

  let rec ftv (t : ty) : int list =
    match t with
    | TVar n -> [ n ]
    | TFun (t1, t2) -> ftv t1 @ ftv t2
    | TTuple ts -> List.flatten (List.map ftv ts)
    | TList t' -> ftv t'
    | TMeasure (t', _) -> ftv t'
    | TOption t' -> ftv t'
    | _ -> []
  ;;

  let ftv_set t = List.sort_uniq compare (ftv t)
  let ftv_scheme (Scheme (vars, t)) = List.filter (fun n -> not (List.mem n vars)) (ftv t)

  let ftv_env (env : (string * scheme) list) =
    List.fold_left (fun acc (_, sch) -> acc @ ftv_scheme sch) [] env
    |> List.sort_uniq compare
  ;;

  let generalize (env : (string * scheme) list) (t : ty) : scheme =
    let env_ftv = ftv_env env in
    let t_ftv = ftv t in
    let vars = List.filter (fun v -> not (List.mem v env_ftv)) t_ftv in
    Scheme (vars, t)
  ;;

  let rec string_of_measure m =
    match m with
    | Measure_ident s -> s
    | Measure_prod (m1, m2) ->
      Printf.sprintf "%s * %s" (string_of_measure m1) (string_of_measure m2)
    | Measure_div (m1, m2) ->
      Printf.sprintf "%s / %s" (string_of_measure m1) (string_of_measure m2)
    | Measure_pow (m1, n) -> Printf.sprintf "%s^%d" (string_of_measure m1) n
    | Measure_dimless -> "1"
  ;;

  let rec string_of_ty t =
    match t with
    | TVar n -> Printf.sprintf "'a%d" n
    | TInt -> "int"
    | TFloat -> "float"
    | TBool -> "bool"
    | TChar -> "char"
    | TString -> "string"
    | TFun (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_ty t1) (string_of_ty t2)
    | TTuple ts -> "(" ^ String.concat " * " (List.map string_of_ty ts) ^ ")"
    | TList t' -> Printf.sprintf "list<%s>" (string_of_ty t')
    | TMeasure (t', Some m) ->
      Printf.sprintf "%s<%s>" (string_of_ty t') (string_of_measure m)
    | TMeasure (t', None) -> Printf.sprintf "%s<\x5F>" (string_of_ty t')
    | TOption t' -> Printf.sprintf "option<%s>" (string_of_ty t')
  ;;

  let string_of_subst (s : subst) : string =
    IntMap.fold
      (fun k t acc -> acc ^ Printf.sprintf "'a%d -> %s; " k (string_of_ty t))
      s
      ""
  ;;

  let string_of_scheme (Scheme (vars, t)) =
    let vars_str =
      if vars = []
      then ""
      else
        "forall "
        ^ String.concat ", " (List.map (fun n -> Printf.sprintf "'a%d" n) vars)
        ^ ". "
    in
    vars_str ^ string_of_ty t
  ;;

  let string_of_env (env : (string * scheme) list) : string =
    List.fold_left
      (fun acc (name, sch) -> acc ^ Printf.sprintf "%s: %s\n" name (string_of_scheme sch))
      ""
      env
  ;;
end

module Unification = struct
  open Types

  exception UnificationError of string

  let bind n t : subst =
    if t = TVar n
    then empty_subst
    else if List.mem n (ftv_set t)
    then raise (UnificationError (Printf.sprintf "Occurs check failed for variable %d" n))
    else IntMap.singleton n t
  ;;

  let rec unify (t1 : ty) (t2 : ty) : subst =
    match t1, t2 with
    | TFun (l1, r1), TFun (l2, r2) ->
      let s1 = unify l1 l2 in
      let s2 = unify (apply_subst s1 r1) (apply_subst s1 r2) in
      compose_subst s2 s1
    | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
      List.fold_left2
        (fun s t1 t2 ->
          let s' = unify (apply_subst s t1) (apply_subst s t2) in
          compose_subst s' s)
        empty_subst
        ts1
        ts2
    | TList t1, TList t2 -> unify t1 t2
    | TMeasure (t1, m1), TMeasure (t2, m2) ->
      if m1 = m2 then unify t1 t2 else raise (UnificationError "Mismatched measures")
    | TOption t1', TOption t2' -> unify t1' t2'
    | TVar n, t | t, TVar n -> bind n t
    | TInt, TInt | TFloat, TFloat | TBool, TBool | TChar, TChar | TString, TString ->
      empty_subst
    | _ ->
      raise
        (UnificationError
           (Printf.sprintf
              "Cannot unify types: %s and %s"
              (string_of_ty t1)
              (string_of_ty t2)))
  ;;
end

module Inference = struct
  open Types
  open Unification
  open Ast

  exception TypeError of string

  type type_env = (string * scheme) list

  let apply_subst_env (s : subst) (env : type_env) : type_env =
    List.map (fun (name, sch) -> name, apply_subst_scheme s sch) env
  ;;

  let initial_env : type_env = []

  module IntMap = Types.IntMap

  let instantiate (Scheme (vars, t)) counter =
    let s, new_counter =
      List.fold_left
        (fun (subst, cnt) var ->
          let tv, cnt' = fresh_ty_var cnt in
          compose_subst (IntMap.singleton var tv) subst, cnt')
        (empty_subst, counter)
        vars
    in
    apply_subst s t, new_counter
  ;;

  let rec core_type_to_ty (ct : core_type) : ty =
    match ct with
    | Type_ident "int" -> TInt
    | Type_ident "float" -> TFloat
    | Type_ident "bool" -> TBool
    | Type_ident "char" -> TChar
    | Type_ident "string" -> TString
    | Type_ident other -> failwith ("Unknown type: " ^ other)
    | Type_func (t1, t2) -> TFun (core_type_to_ty t1, core_type_to_ty t2)
    | Type_tuple (t1, t2, ts) -> TTuple (List.map core_type_to_ty (t1 :: t2 :: ts))
  ;;

  (* Основная функция инференса по выражению *)
  let rec infer_expr (env : type_env) (expr : expression) (counter : int)
    : subst * ty * int
    =
    match expr with
    | Expr_const c ->
      let t =
        match c with
        | Const_int _ -> TInt
        | Const_float _ -> TFloat
        | Const_bool _ -> TBool
        | Const_char _ -> TChar
        | Const_string _ -> TString
        | Const_unit_of_measure u ->
          (match u with
           | Unit_of_measure (Mnum_int _, m) -> TMeasure (TInt, Some m)
           | Unit_of_measure (Mnum_float _, m) -> TMeasure (TFloat, Some m))
      in
      empty_subst, t, counter
    | Expr_ident_or_op name ->
      (match List.assoc_opt name env with
       | Some sch ->
         let t, counter' = instantiate sch counter in
         empty_subst, t, counter'
       | None -> raise (TypeError ("Unbound variable: " ^ name)))
    | Expr_typed (e, ct) ->
      let s1, t1, counter1 = infer_expr env e counter in
      let tAnn = core_type_to_ty ct in
      let s2 = unify (apply_subst s1 t1) tAnn in
      let final_s = compose_subst s2 s1 in
      final_s, apply_subst final_s tAnn, counter1
    | Expr_tuple (e1, e2, es) ->
      let s1, t1, counter1 = infer_expr env e1 counter in
      let env1 = apply_subst_env s1 env in
      let s2, t2, counter2 = infer_expr env1 e2 counter1 in
      let env2 = apply_subst_env s2 env1 in
      let sRest, ts, counter3 =
        List.fold_left
          (fun (sAcc, types, cnt) e ->
            let sE, tE, cnt' = infer_expr env2 e cnt in
            compose_subst sE sAcc, types @ [ tE ], cnt')
          (empty_subst, [], counter2)
          es
      in
      let final_s = compose_subst sRest (compose_subst s2 s1) in
      final_s, apply_subst final_s (TTuple (t1 :: t2 :: ts)), counter3
    | Expr_list es ->
      let tv, counter1 = fresh_ty_var counter in
      let s, counter2 =
        List.fold_left
          (fun (sAcc, cnt) e ->
            let sE, tE, cnt' = infer_expr env e cnt in
            let sU = unify (apply_subst sE tv) tE in
            compose_subst sU (compose_subst sE sAcc), cnt')
          (empty_subst, counter1)
          es
      in
      s, apply_subst s (TList tv), counter2
    | Expr_lam (p, e) ->
      let s1, env_ext, tP, counter1 = infer_pattern env p counter in
      let env' = env_ext @ env in
      let s2, tBody, counter2 = infer_expr env' e counter1 in
      let final_s = compose_subst s2 s1 in
      final_s, apply_subst final_s (TFun (apply_subst s2 tP, tBody)), counter2
    | Expr_let (flag, bind, binds, e2) ->
      (match flag with
       | Nonrecursive ->
         let s1, t1, counter1 =
           infer_expr
             env
             (match bind with
              | Ast.Bind (_, e) -> e)
             counter
         in
         let sch = generalize env (apply_subst s1 t1) in
         let var =
           pattern_name
             (match bind with
              | Ast.Bind (p, _) -> p)
         in
         let env' = (var, sch) :: env in
         let sBinds, env'', counter2 =
           List.fold_left
             (fun (sAcc, envAcc, cnt) (Ast.Bind (p, e)) ->
               let sE, tE, cnt' = infer_expr envAcc e cnt in
               let schE = generalize envAcc (apply_subst sE tE) in
               compose_subst sE sAcc, (pattern_name p, schE) :: envAcc, cnt')
             (s1, env', counter1)
             binds
         in
         let s2, t2, counter3 = infer_expr env'' e2 counter2 in
         let final_s = compose_subst s2 sBinds in
         final_s, apply_subst final_s t2, counter3
       | Recursive ->
         let all_bindings = bind :: binds in
         let extract_id p =
           match p with
           | Pattern_ident_or_op name -> name
           | _ -> failwith "Only simple identifier allowed in let rec binding"
         in
         let rec_env, counter1 =
           List.fold_left
             (fun (envAcc, cnt) (Ast.Bind (p, _)) ->
               let tv, cnt' = fresh_ty_var cnt in
               (extract_id p, Scheme ([], tv)) :: envAcc, cnt')
             ([], counter)
             all_bindings
         in
         let extended_env = rec_env @ env in
         let s_rec, counter2 =
           List.fold_left
             (fun (s, cnt) (Ast.Bind (p, e)) ->
               let id = extract_id p in
               let s_e, t_e, cnt' = infer_expr extended_env e cnt in
               let (Scheme (_, tv)) = List.assoc id rec_env in
               let s_u = unify (apply_subst s tv) t_e in
               compose_subst s_u s, cnt')
             (empty_subst, counter1)
             all_bindings
         in
         let rec_env' =
           List.map
             (fun (id, _) ->
               let (Scheme (_, tv)) = List.assoc id rec_env in
               id, generalize extended_env (apply_subst s_rec tv))
             rec_env
         in
         let final_env = rec_env' @ env in
         let s_body, t_body, counter3 = infer_expr final_env e2 counter2 in
         let final_s = compose_subst s_body s_rec in
         final_s, apply_subst final_s t_body, counter3)
    | Expr_ifthenelse (e1, e2, e3Opt) ->
      let s1, t1, counter1 = infer_expr env e1 counter in
      let sBool = unify t1 TBool in
      let env1 = apply_subst_env sBool env in
      let s2, t2, counter2 = infer_expr env1 e2 counter1 in
      (match e3Opt with
       | Some e3 ->
         let s3, t3, counter3 = infer_expr env1 e3 counter2 in
         let s4 = unify t2 t3 in
         let final_s = compose_subst s4 (compose_subst s3 s2) in
         final_s, apply_subst final_s t2, counter3
       | None ->
         let final_s = compose_subst sBool s1 in
         final_s, apply_subst final_s t2, counter2)
    | Expr_apply (e1, e2) ->
      let s1, t1, counter1 = infer_expr env e1 counter in
      let env1 = apply_subst_env s1 env in
      let s2, t2, counter2 = infer_expr env1 e2 counter1 in
      let tv, counter3 = fresh_ty_var counter2 in
      let s3 = unify (apply_subst s2 t1) (TFun (t2, tv)) in
      let final_s = compose_subst s3 (compose_subst s2 s1) in
      final_s, apply_subst final_s tv, counter3
    | Expr_match (scrut_expr, rule1, rules) ->
      let sScrut, tScrut, counter1 = infer_expr env scrut_expr counter in
      let envScrut = apply_subst_env sScrut env in
      let tMatch, counter2 = fresh_ty_var counter1 in
      let infer_one_branch (Rule (pat, body)) cnt =
        let sp, envP, tP, cnt' = infer_pattern envScrut pat cnt in
        let tScrut' = apply_subst sp tScrut in
        let tP' = apply_subst sp tP in
        let su = unify tScrut' tP' in
        let spU = compose_subst su sp in
        let envBranch = apply_subst_env spU (envP @ envScrut) in
        let se, tE, cnt'' = infer_expr envBranch body cnt' in
        let sBranch = compose_subst se spU in
        let tE' = apply_subst sBranch tE in
        let tMatch' = apply_subst sBranch tMatch in
        let sR = unify tE' tMatch' in
        compose_subst sR sBranch, cnt''
      in
      let s1, counter3 = infer_one_branch rule1 counter2 in
      let sAll, counter4 =
        List.fold_left
          (fun (sAcc, cnt) rule ->
            let sBranch, cnt' = infer_one_branch rule cnt in
            compose_subst sBranch sAcc, cnt')
          (s1, counter3)
          rules
      in
      let sFinal = compose_subst sAll sScrut in
      let tFinal = apply_subst sFinal tMatch in
      sFinal, tFinal, counter4
    | Expr_function (rule, rules) ->
      let tv, counter1 = fresh_ty_var counter in
      let arg = Expr_ident_or_op "x" in
      let match_expr = Expr_match (arg, rule, rules) in
      let lam_expr = Expr_lam (Pattern_ident_or_op "x", match_expr) in
      infer_expr env lam_expr counter1
    | Expr_option None ->
      let tv, counter1 = fresh_ty_var counter in
      empty_subst, TOption tv, counter1
    | Expr_option (Some e_inner) ->
      let s, t_inner, counter1 = infer_expr env e_inner counter in
      s, TOption t_inner, counter1

  (* Инференс типов для паттернов.
     Возвращает: подстановку, расширение окружения (ассоциации имя → схема) и тип паттерна. *)
  and infer_pattern (env : type_env) (p : Ast.pattern) (counter : int)
    : subst * (string * scheme) list * ty * int
    =
    match p with
    | Pattern_ident_or_op name ->
      let tv, counter1 = fresh_ty_var counter in
      empty_subst, [ name, Scheme ([], tv) ], tv, counter1
    | Pattern_const c ->
      let _, t, counter1 = infer_expr env (Expr_const c) counter in
      empty_subst, [], t, counter1
    | Pattern_wild ->
      let tv, counter1 = fresh_ty_var counter in
      empty_subst, [], tv, counter1
    | Pattern_typed (p, ct) ->
      let s, env_ext, tP, counter1 = infer_pattern env p counter in
      let tAnn = core_type_to_ty ct in
      let s2 = unify tP tAnn in
      compose_subst s2 s, env_ext, tAnn, counter1
    | Pattern_tuple (p1, p2, ps) ->
      let s1, env1, t1, counter1 = infer_pattern env p1 counter in
      let s2, env2, t2, counter2 = infer_pattern env p2 counter1 in
      let sRest, envRest, ts, counter3 =
        List.fold_left
          (fun (sAcc, envAcc, types, cnt) p ->
            let sP, envP, tP, cnt' = infer_pattern env p cnt in
            compose_subst sP sAcc, envAcc @ envP, types @ [ tP ], cnt')
          (empty_subst, [], [], counter2)
          ps
      in
      ( compose_subst sRest (compose_subst s2 s1)
      , env1 @ env2 @ envRest
      , TTuple (t1 :: t2 :: ts)
      , counter3 )
    | Pattern_list ps ->
      let tv, counter1 = fresh_ty_var counter in
      let s, env_ext, ts, counter2 =
        List.fold_left
          (fun (sAcc, envAcc, types, cnt) p ->
            let sP, envP, tP, cnt' = infer_pattern env p cnt in
            compose_subst sP sAcc, envAcc @ envP, types @ [ tP ], cnt')
          (empty_subst, [], [], counter1)
          ps
      in
      let sU =
        List.fold_left
          (fun sAcc t -> compose_subst (unify (apply_subst sAcc tv) t) sAcc)
          empty_subst
          ts
      in
      compose_subst sU s, env_ext, TList tv, counter2
    | Pattern_or (p1, p2) ->
      let s1, env1, t1, counter1 = infer_pattern env p1 counter in
      let s2, env2, t2, counter2 = infer_pattern env p2 counter1 in
      let sU = unify (apply_subst s1 t1) t2 in
      compose_subst sU (compose_subst s2 s1), env1 @ env2, t1, counter2
    | Pattern_option None ->
      let tv, counter1 = fresh_ty_var counter in
      empty_subst, [], TOption tv, counter1
    | Pattern_option (Some p_inner) ->
      let s, env_ext, t_inner, counter1 = infer_pattern env p_inner counter in
      s, env_ext, TOption t_inner, counter1

  (* Для let–связей допускается только простой идентификатор *)
  and pattern_name (p : Ast.pattern) : string =
    match p with
    | Pattern_ident_or_op name -> name
    | _ -> failwith "Only simple identifier allowed in let-binding"
  ;;

  let infer env expr =
    let _s, t, _ = infer_expr env expr 0 in
    _s, t
  ;;
end
