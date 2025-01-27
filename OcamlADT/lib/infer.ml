open InferTypes

module MInfer = struct
  open Base

  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
    let fold_right xs ~init ~f =
      Base.List.fold_right xs ~init ~f:(fun x acc ->
        let open Syntax in
        let* acc = acc in
        f x acc)
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh_var = int

module Type = struct
  type t = typ

  let rec occurs_check tvar = function
    | Typ_prim _ -> false
    | Typ_var binder -> binder = tvar
    | Typ_arrow (l, r) -> occurs_check tvar l || occurs_check tvar r
    | Typ_tuple t -> List.fold_left (fun acc h -> acc || occurs_check tvar h) false t
    | Typ_list l -> occurs_check tvar l
  ;;

  let free_vars =
    let rec helper acc = function
      | Typ_prim _ -> acc
      | Typ_var binder -> VarSet.add binder acc
      | Typ_arrow (l, r) -> helper (helper acc l) r
      | Typ_tuple t -> List.fold_left (fun acc h -> helper acc h) acc t
      | Typ_list l -> helper acc l
    in
    helper VarSet.empty
  ;;
end

module Substitution = struct
  open MInfer
  open MInfer.Syntax
  open Base

  type t = (binder, typ, Int.comparator_witness) Map.t
(* 
  let pp ppf subst =
    let open Stdlib.Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k pprint_type v))
      subst
  ;; *)

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_check k v then fail `Occurs_check else return (k, v)

  let singleton k vm =
    let* k, vm = mapping k vm in
    return (Base.Map.singleton (module Base.Int) k vm)
  ;;

  let find_exn (map : t) (k : binder) : typ = Map.find_exn map k
  let find (map : t) (k : binder) : typ option = Map.find map k
  let remove (map : t) (k : binder) : t = Map.remove map k

  let apply sub =
    let rec helper = function
      | Typ_prim _ as typ -> typ
      | Typ_var b as typ ->
        (match find_exn sub b with
         | exception Not_found_s _ -> typ
         | _ -> typ)
      | Typ_arrow (l, r) -> Typ_arrow (helper l, helper r)
      | Typ_tuple t -> Typ_tuple (List.map t ~f:(fun el -> helper el))
      | Typ_list l -> Typ_list (helper l)
    in
    helper
  ;;

  let fold mp init f =
    Map.fold mp ~init ~f:(fun ~key:k ~data:vm acc ->
      let* acc = acc in
      f k vm acc)
  ;;

  let rec unify l r =
    match l, r with
    | Typ_prim l, Typ_prim r when String.equal l r -> return empty
    | Typ_prim _, Typ_prim _ -> fail (`Unification_failed (l, r))
    | Typ_var a, Typ_var b when Int.equal a b -> return empty
    | Typ_var b, t | t, Typ_var b -> singleton b t
    | Typ_arrow (l1, r1), Typ_arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | Typ_tuple(l1),Typ_tuple(l2) ->
        (match (l1,l2) with
        | [],[] -> return empty
        | hd1::tl1,hd2::tl2 -> 
          let* subb = unify hd1 hd2 in
          let* uni_sub = unify (apply subb (Typ_tuple tl1)) (apply subb (Typ_tuple tl2)) in
          compose subb uni_sub
        | _ -> fail (`Unification_failed (l, r)))
    | _ -> fail (`Unification_failed (l, r))
    (* and unify_many l r =  *)


  and extend k v s =
    match Map.find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      fold s (return s2) (fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.set acc ~key:k ~data:v))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = fold s2 (return s1) extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

module Scheme = struct
  type t = scheme

  let occurs_check v = function
    | Forall (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_check v t
  ;;

  let free_vars = function
    | Forall (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply subst (Forall (binder_set, typ)) =
    let s2 = VarSet.fold (fun k s -> Substitution.remove s k) binder_set subst in
    Forall (binder_set, Substitution.apply s2 typ)
  ;;

  let pp_scheme fmt = function
    | Forall (st, typ) ->
      if VarSet.is_empty st
      then Format.fprintf fmt "%a" pprint_type typ
      else Format.fprintf fmt "%a. %a" VarSet.pp st pprint_type typ
  ;;
end

module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  let extend env name scheme = 
    (* let () = print_endline name in *)
    Map.set env ~key:name ~data:scheme
  let empty = Map.empty (module String)
  let fold f init mp = Map.fold mp ~init ~f:(fun ~key:k ~data:v acc -> f k v acc)

  let free_vars : t -> VarSet.t =
    fold (fun _ s acc -> VarSet.union acc (Scheme.free_vars s)) VarSet.empty
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find name xs = Map.find xs name

  let pp_env fmt environment =
    Map.iteri environment ~f:(fun ~key ~data ->
      Stdlib.Format.fprintf fmt "%S: %a\n" key Scheme.pp_scheme data)
  ;;
end

open MInfer
open MInfer.Syntax

let fresh_var = fresh >>| fun n -> Typ_var n

let instantiate : scheme -> typ MInfer.t =
  fun (Forall (bs, t)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* f1 = fresh_var in
      let* s = Substitution.singleton name f1 in
      return (Substitution.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  Forall (free, ty)
;;

open Ast.Constant
open Ast.Expression
open Ast.Pattern
open Scheme
let rec infer_pat pat env =
  match pat with 
  |Pat_any ->
    let* fresh = fresh_var in
    return(env,fresh)
  |Pat_var ident ->
    let* fresh = fresh_var in
    let new_env = TypeEnv.extend env ident (Forall (VarSet.empty, fresh)) in
    (* let _ = Stdlib.Format.printf "DEBUG: env in Pat_var:%a" TypeEnv.pp_env new_env in *)
    return (new_env, fresh)
  |Pat_constant const ->
    (match const with 
    |Const_char _ -> return(env, ch_typ)
    |Const_integer _  -> return (env, int_typ)
    |Const_string _ -> return (env, string_typ)
    )
  |Pat_tuple (pat1,pat2,rest) -> 
    let* env1,typ1 = infer_pat pat1 env in
    let* env2,typ2 = infer_pat pat2 env1 in
    let* env3,typ3 = 
    RList.fold_right ~f:(
      fun pat acc ->
        let* env_acc, typ_list = return acc in
        let* env, typ = infer_pat pat env_acc in
        return (env, typ :: typ_list))
         ~init:(return(env2,[])) rest
      in return (env3, Typ_tuple(typ1::typ2::typ3))
  | _ -> failwith "aboba"
let rec infer_exp exp env =
  match exp with
   | Exp_ident varname ->
    (match TypeEnv.find varname env with
     | None -> fail (`Unbound_variable varname)
     | Some x ->
       let* typ = instantiate x in
       return (Substitution.empty, typ))
  | Exp_constant const ->
    (match const with
     | Const_char _ -> return (Substitution.empty, Typ_prim "char")
     | Const_integer _ -> return (Substitution.empty, int_typ)
     | Const_string _ -> return (Substitution.empty, string_typ))
   | Exp_apply (Exp_ident op, Exp_tuple(exp1,exp2,_)) ->
    let* sub1,typ1 = infer_exp exp1 env in
    let* sub2,typ2 = infer_exp exp2 (TypeEnv.apply sub1 env) in
    let* arg_typ, res_typ =
    match op with
    | "*" | "/" | "+" | "-" -> return (int_typ, int_typ)
    | "<" |">" |"="| "<>" | "<=" | ">=" -> 
      let* fresh = fresh_var in return (fresh, bool_typ)
    | "&&" | "||" -> return (bool_typ, bool_typ)
    | _ -> failwith "aboba"
    in
    let* unif_sub1 = Substitution.unify (Substitution.apply sub2 typ1) arg_typ in
    let* unif_sub2 = Substitution.unify (Substitution.apply unif_sub1 typ2) arg_typ in
    let* comp_sub = Substitution.compose_all [sub1;sub2;unif_sub1;unif_sub2] in
    return (comp_sub, res_typ)
  | Exp_apply (exp1, exp2) -> 
    (match exp1 with
    | Exp_ident op when op = "+" || op = "-" ->
      let* sub1, typ1 = infer_exp exp2 env in
      let* unif_sub = Substitution.unify typ1 int_typ in
      let* comp_sub = Substitution.compose sub1 unif_sub in
      return (comp_sub,int_typ )
    | _ -> 
      let* sub1, typ1 = infer_exp exp1 env in
      let* sub2, typ2 = infer_exp exp2 (TypeEnv.apply sub1 env) in
      let* fresh = fresh_var in
      let* unif_sub = Substitution.unify (Substitution.apply sub2 typ1) (Typ_arrow (typ2,fresh)) in
      let* comp_sub =  Substitution.compose_all [sub1;sub2;unif_sub] in
      let res_typ = Substitution.apply comp_sub fresh in
      return(comp_sub,res_typ))
  | Exp_fun ((pattern,patterns),expr) -> 
    let* new_env, typ1 = infer_pat pattern env in
    let* sub1, typ2 =
    (* let _ = Stdlib.Format.printf "DEBUG: env in Exp_fun:%a" TypeEnv.pp_env new_env in *)
    match patterns with 
    | [] -> infer_exp expr new_env
    | hd :: tl -> infer_exp (Exp_fun ((hd,tl),expr)) new_env
    in 
    return (sub1, Typ_arrow (Substitution.apply sub1 typ1,typ2))
  | Exp_tuple (exp1,exp2,rest) ->
    let* sub1,typ1 = infer_exp exp1 env in
    let new_env = TypeEnv.apply sub1 env in
    let* sub2,typ2 = infer_exp exp2 new_env in
    let new_env = TypeEnv.apply sub2 new_env in
    (* let* comp_sub = Substitution.compose sub1 sub2 in *)
    let* sub3,typ3 = 
    RList.fold_right ~f:( fun exp acc ->
      let* sub_acc, typ_list = return acc in
      let new_env = TypeEnv.apply sub_acc new_env in
      let* sub, typ = infer_exp exp new_env in
      let* sub_acc = Substitution.compose sub_acc sub in
      return (sub_acc, typ::typ_list)
      )
        ~init:(return (Substitution.empty, [])) rest
       in
    let* fin_sub = Substitution.compose_all [sub1;sub2;sub3] in
    let typ1 = Substitution.apply fin_sub typ1 in
    let typ2 = Substitution.apply fin_sub typ2 in
    let typ3 = List.map (fun typ -> Substitution.apply fin_sub typ) typ3 in
    return(fin_sub,Typ_tuple(typ1::typ2::typ3))

  | Exp_if (ifexp,thenexp,Some elseexp) -> 
    let* sub1,typ1 = infer_exp ifexp env in
    let* uni_sub1 = Substitution.unify typ1 bool_typ in
    let new_env = TypeEnv.apply uni_sub1 env in 
    let* sub2, typ2 = infer_exp thenexp new_env in
    let new_env = TypeEnv.apply sub2 new_env in 
    let* sub3, typ3 = infer_exp elseexp new_env in
    let* uni_sub2 = Substitution.unify typ2 typ3 in
    let new_env = TypeEnv.apply uni_sub2 new_env in
    let* comp_sub = Substitution.compose_all [sub1;uni_sub1;sub2;sub3;uni_sub2] in
    return (comp_sub,typ3)
  | Exp_if (ifexp,thenexp,None) ->
    let* sub1,typ1 = infer_exp ifexp env in
      let* uni_sub1 = Substitution.unify typ1 bool_typ in
      let new_env = TypeEnv.apply uni_sub1 env in 
      let* sub2, typ2 = infer_exp thenexp new_env in
      let new_env = TypeEnv.apply sub2 new_env in 
      let* comp_sub = Substitution.compose_all [sub1;uni_sub1;sub2] in
      return (comp_sub,typ2)
  | Exp_function (case1, case2::rest) -> 
    let* env1, typ1 = infer_pat case1.first env in
    let* sub1, typ2 = infer_exp case1.second env1 in
    let new_env = TypeEnv.apply sub1 env1 in
    let* res_env,res_sub,res_typexp,res_typepat =
    infer_cases new_env (case2::rest) typ2 typ1 sub1 in
    (* let* env2, typ3 = infer_pat case2.first new_env in
    let* sub2, typ4 = infer_exp case2.second env2 in
    let* uni_sub1 = Substitution.unify typ1 typ3 in
    let* uni_sub2 = Substitution.unify typ2 typ4 in
    let* comp_sub = Substitution.compose_all [sub1;sub2;uni_sub1;uni_sub2] in
    let new_env = TypeEnv.apply comp_sub env2 in *)
    return (res_sub, res_typexp)
  | Exp_match (expr, (case1,case2::rest)) -> 
    let* subexpr, typexpr = infer_exp expr env in
    let new_env = TypeEnv.apply subexpr env in
    let* env1, typ1 = infer_pat case1.first new_env in
    let* uni_subexpr = Substitution.unify typ1 typexpr in
    let* sub1, typ2 = infer_exp case1.second env1 in
    let new_env = TypeEnv.apply sub1 env1 in
    let* res_env,res_sub,res_typexp,res_typepat =
    infer_cases new_env (case2::rest) typ2 typ1 sub1 in
    (* let* env2, typ3 = infer_pat case2.first new_env in
    let* sub2, typ4 = infer_exp case2.second env2 in
    let* uni_sub1 = Substitution.unify typ1 typ3 in
    let* uni_sub2 = Substitution.unify typ2 typ4 in
    let* comp_sub = Substitution.compose_all [subexpr;sub1;uni_subexpr;sub2;uni_sub1;uni_sub2] in
    let new_env = TypeEnv.apply comp_sub env2 in *)
    return (res_sub, res_typexp)
  | Exp_let (Nonrecursive, ({pat;expr}, _), exp) ->
    (* let _ = Stdlib.Format.printf "DEBUG: env in Exp_let!:%a" TypeEnv.pp_env env in *)
    (match pat with
      | Pat_var var_name ->
      let* fresh_var = fresh_var in
      let new_env = TypeEnv.extend  env var_name (Forall (VarSet.empty, fresh_var)) in
      let* sub1, typ1 = infer_exp expr new_env in
      let* subst2 = Substitution.unify typ1 fresh_var in
      let* subst3 = Substitution.compose sub1 subst2 in
      let typ1 = Substitution.apply subst3 typ1 in
      let new_env = TypeEnv.apply subst3 new_env in
      let gen_scheme = generalize (TypeEnv.apply subst3 env) typ1 in
      let new_env = TypeEnv.extend new_env var_name gen_scheme  in
      let* subb, typp = infer_exp exp new_env in
      return (subb, typp)
      | _ -> 
      failwith "Unsupported pattern in let binding")  
(* | Exp_let (Recursive, ({pat;expr}, _), exp) -> (*TODO: VB*)
          (match pat with
           | Pat_var var_name ->
            let* sub1, typ1 = infer_exp expr env in
            let new_env = TypeEnv.apply sub1 env in
            let new_scheme = generalize new_env typ1 in
            let new_env = TypeEnv.extend env var_name new_scheme in
            let new_env = TypeEnv.apply sub1 new_env in
            let* sub2, typ2 = infer_exp exp new_env in
            let* comp_sub = Substitution.compose sub1 sub2 in
            return (comp_sub,typ2) *)
              (*let* new_env, typ = infer_pat pat env in
             let* sub1, typ1 = infer_exp expr new_env in
             let applied_type = Substitution.apply sub1 typ1 in
             let new_scheme = generalize new_env applied_type in
             let extended_env = TypeEnv.extend new_env var_name new_scheme in
             let* sub2, typ2 = infer_exp exp extended_env in
             let* new_subst = Substitution.compose sub2 sub1 in
      
             let _ = Stdlib.Format.printf "DEBUG: env in Exp_let:%a" TypeEnv.pp_env extended_env in
             
             return (new_subst, typ2) *)
      
            (* | _ -> failwith "Unsupported pattern in let binding") *)
           (*TODO: Recursive + VB*)
  | _ ->  failwith "unlucky"



and infer_cases env cases tyexp typat subst =
  let* env, sub, typexp,typpat = RList.fold_right  ~f:( fun case acc ->
    let* env_acc, sub_acc, tyexp,typat = return acc in
    let* new_env, typepat = infer_pat case.first env_acc in
    let* new_sub, typeexp = infer_exp case.second new_env in
    let* uni_sub_exp = Substitution.unify tyexp typeexp in
    let* uni_sub_pat = Substitution.unify typat typepat in
    let* comp_sub = Substitution.compose_all [sub_acc;new_sub;uni_sub_exp;uni_sub_pat] in
    let new_env = TypeEnv.apply comp_sub new_env in
    return (new_env,comp_sub,typeexp,typepat)
  )   ~init:(return (env,subst,tyexp,typat)) cases
 in
  return (env,sub,typexp,typpat)

(* let infer_value_bindings env vb = *)



open Ast.Pattern
open Ast.Structure


let infer_value_binding env vb =
  match vb with
    | {pat;expr} ->
    match pat with 
    | Pat_var var_name ->
       let* new_env, _ = infer_pat pat env in
       let* sub1, typ1 = infer_exp expr new_env in
       let applied_type = Substitution.apply sub1 typ1 in
       let new_scheme = generalize new_env applied_type in
       let extended_env = TypeEnv.extend new_env var_name new_scheme in
       return extended_env
    | Pat_any -> failwith "aboba"
    | Pat_constant _ -> failwith "aboba"
    | Pat_construct(_,_) -> failwith "aboba"
    | Pat_constraint (_,_)-> failwith "aboba"
    | Pat_tuple (_,_,_) -> failwith "aboba"
  

let infer_value_binding_list env vb_list = 
  let* env = RList.fold_left vb_list ~init:(return (env)) ~f:(infer_value_binding) in

  return env


let infer_structure_item env item =
  match item with
  | Str_eval exp ->
    (* let _ = Stdlib.Format.printf "DEBUG: env in Str_eval!!!" in *)
    let* _,typ = infer_exp exp env in (* maybe create empty env there *)
    let new_env = TypeEnv.extend env "_" (Forall (VarSet.empty, typ)) in
    (* let _ = Stdlib.Format.printf "DEBUG: env in Str_eval:%a" pprint_type typ in *)
    return new_env
  | Str_value (Nonrecursive, (value_binding,rest)) -> (*TODO: VB*)
    let* new_env = infer_value_binding_list env (value_binding :: rest) in
    return new_env
(* | Str_value (Recursive,(value_binding,value_bindingtl)) -> *)
  | _ -> failwith "Unsupported pattern in let binding"
;;

let infer_program program env =
  let* env = RList.fold_left program ~init:(return (env)) ~f:(infer_structure_item) in

   return env
;; 

(*for expr test*)
let run_infer_expr (program : Ast.Expression.t) env  = run (infer_exp program env)

(*for str item test*)
let run_infer_program (program : Ast.program) env = run (infer_program program env)