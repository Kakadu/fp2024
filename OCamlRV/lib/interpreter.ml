(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Stdlib

type builtin =
  | BInt of (int -> unit)
  | BString of (string -> unit)

type environment =
  (string, value, Base.Comparator.Make(Base.String).comparator_witness) Base.Map.t

and value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VTuple of value list
  | VNil
  | VOption of value option
  | VFun of pattern * rec_flag * expression * environment
  | VMutualFun of pattern * rec_flag * expression * environment
  | VCycle of string
  | VBuiltin of builtin * environment

let rec vvv = VUnit :: VUnit :: vvv

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | VInt x -> fprintf ppf "%d" x
  | VBool b -> fprintf ppf "%b" b
  | VString s -> fprintf ppf "%s" s
  | VUnit -> fprintf ppf "()"
  | VList _ -> fprintf ppf "<VList>"
  (* | VList vl ->
     fprintf
     ppf
     "[%a]"
     (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_value)
     vl *)
  | VTuple vl ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
      vl
  | VNil -> fprintf ppf "[]"
  | VFun _ -> fprintf ppf "<fun>"
  | VBuiltin _ -> fprintf ppf "<builtin>"
  | VMutualFun _ -> fprintf ppf "<VMutualFun>"
  | VCycle s -> fprintf ppf "<cycle> %s" s
  | VOption vo ->
    (match vo with
     | Some v -> fprintf ppf "Some %a" pp_value v
     | None -> fprintf ppf "None")
;;

let vint i = VInt i
let vbool b = VBool b
let vstring s = VString s
let vunit = VUnit
let vnil = VNil
let vfun p rf e env = VFun (p, rf, e, env)

type error =
  | Pattern_matching_failed
  | Apply_failed
  | Wrong_type of value
  | Unbound_variable
  | Evaluationg_Need_ToBeReplaced
  | BuiltinEvaluatingError

let pp_error ppf =
  let open Stdlib.Format in
  function
  | Pattern_matching_failed -> fprintf ppf "Pattern_matching_failed"
  | Apply_failed -> fprintf ppf "Apply_failed"
  | Wrong_type _ -> fprintf ppf "Wrong_type"
  | Unbound_variable -> fprintf ppf "Unbound_variable"
  | Evaluationg_Need_ToBeReplaced -> fprintf ppf "Evaluationg_Need_ToBeReplaced"
  | BuiltinEvaluatingError -> fprintf ppf "BuiltinEvaluatingError"
;;

module type MONAD_FAIL = sig
  include Monad.S2

  val fail : error -> ('a, error) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

let print_env env =
  Format.printf "\nDEBUG ENV:\n";
  Base.Map.iteri
    ~f:(fun ~key ~data ->
      Format.fprintf Format.std_formatter "%s : some_type = %a\n" key pp_value data)
    env
;;

module Env (M : MONAD_FAIL) = struct
  open M

  let empty =
    Base.Map.empty
      (module struct
        include String
        include Base.Comparator.Make (Base.String)
      end)
  ;;

  let extend env k v = Base.Map.update env k ~f:(fun _ -> v)

  let find env name =
    match Base.Map.find env name with
    | Some x -> return x
    | None -> fail Unbound_variable
  ;;
end

module Eval (M : MONAD_FAIL) = struct
  open M
  open Env (M)

  let rec check_match env = function
    | PAny, _ -> Some env
    | PConstant (CInt i1), VInt i2 when i1 = i2 -> Some env
    | PConstant (CBool b1), VBool b2 when Bool.equal b1 b2 -> Some env
    | PConstant (CString s1), VString s2 when String.equal s1 s2 -> Some env
    | PConstant CNil, VList [] -> Some env
    | PConstant CNil, VNil -> Some env
    | PConstant CUnit, VUnit -> Some env
    | PVar x, v -> Some (extend env x v)
    | PTuple (p1, p2, rest), VTuple vl ->
      let pl = p1 :: p2 :: rest in
      let env =
        Base.List.fold2
          pl
          vl
          ~f:(fun env p v ->
            match env with
            | Some e -> check_match e (p, v)
            | None -> None)
          ~init:(Some env)
      in
      (match env with
       | Ok env -> env
       | _ -> None)
    | PList (p1, rest), VList vl ->
      let pl = p1 :: rest in
      let env =
        Base.List.fold2
          pl
          vl
          ~f:(fun env p v ->
            match env with
            | Some e -> check_match e (p, v)
            | None -> None)
          ~init:(Some env)
      in
      (match env with
       | Ok env -> env
       | _ -> None)
    | PCons (p1, p2), VList (v :: vl) ->
      let env = check_match env (p2, VList vl) in
      (match env with
       | Some env -> check_match env (p1, v)
       | None -> None)
    | POption None, VOption None -> Some env
    | POption (Some p), VOption (Some v) ->
      let env = check_match env (p, v) in
      (match env with
       | Some env -> Some env
       | None -> None)
    | _ -> None
  ;;

  let eval_binop (op, v1, v2) =
    match op, v1, v2 with
    | Add, VInt x, VInt y -> return (vint (x + y))
    | Sub, VInt x, VInt y -> return (vint (x - y))
    | Mul, VInt x, VInt y -> return (vint (x * y))
    | Div, VInt x, VInt y -> return (vint (x / y))
    | Lt, VInt x, VInt y -> return (vbool (x < y))
    | Gt, VInt x, VInt y -> return (vbool (x > y))
    | Eq, VInt x, VInt y -> return (vbool (x = y))
    | Neq, VInt x, VInt y -> return (vbool (x <> y))
    | Lte, VInt x, VInt y -> return (vbool (x <= y))
    | Gte, VInt x, VInt y -> return (vbool (x >= y))
    | And, VBool x, VBool y -> return (vbool (x && y))
    | Or, VBool x, VBool y -> return (vbool (x || y))
    | _ -> fail Evaluationg_Need_ToBeReplaced
  ;;

  let eval_unop (op, v) =
    match op, v with
    | UnaryPlus, VInt x -> return (vint (Stdlib.( ~+ ) x))
    | UnaryMinus, VInt x -> return (vint (-x))
    | UnaryNeg, VBool x -> return (vbool (not x))
    | _ -> fail Evaluationg_Need_ToBeReplaced
  ;;

  let eval_tuple_binding env pl vl =
    return
      (match vl with
       | VTuple tl ->
         let a =
           Base.List.fold2
             pl
             tl
             ~f:(fun env p v ->
               match p with
               | PVar s -> extend env s v
               | POption (Some (PVar s)) ->
                 (match v with
                  | VOption (Some vo) -> extend env s vo
                  | _ -> env)
               | _ -> env)
             ~init:env
         in
         (match a with
          | Ok r -> r
          | Unequal_lengths -> env)
       | _ -> env)
  ;;

  let rec eval_expr (env : environment) = function
    | ExprConstant c ->
      (match c with
       | CInt i -> return (vint i)
       | CBool b -> return (vbool b)
       | CString s -> return (vstring s)
       | CUnit -> return vunit
       | CNil -> return vnil)
    | ExprVariable x ->
      let* v = find env x in
      let v =
        match v with
        | VFun (p, Rec, e, env) -> VFun (p, Rec, e, extend env x v)
        | VBuiltin (f, env) -> VBuiltin (f, extend env x v)
        | _ -> v
      in
      return v
    | ExprBinOperation (op, e1, e2) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      eval_binop (op, v1, v2)
    | ExprUnOperation (op, e) ->
      let* v = eval_expr env e in
      eval_unop (op, v)
    | ExprIf (cond, t, Some f) ->
      let* cv = eval_expr env cond in
      (match cv with
       | VBool true -> eval_expr env t
       | VBool false -> eval_expr env f
       | _ -> fail (Wrong_type cv))
    | ExprLet (NonRec, b, bl, e) ->
      let bindings = b :: bl in
      let* env2 = eval_non_rec_binding_list env bindings in
      eval_expr env2 e
    | ExprLet (Rec, (PVar x, e1), [], e) ->
      let* v = eval_expr env e1 in
      let env1 = extend env x v in
      let v =
        match v with
        | VFun (p, _, e, _) -> VFun (p, Rec, e, env1)
        | _ -> v
      in
      let env2 = extend env x v in
      eval_expr env2 e
    | ExprLet (Rec, b, bl, e) ->
      let bindings = b :: bl in
      let* env2 = eval_rec_binding_list env bindings in
      eval_expr env2 e
    | ExprFun (p, e) -> return (vfun p NonRec e env)
    | ExprMatch (e, c, cl) ->
      let* v = eval_expr env e in
      let rec match_helper env v = function
        | (p, e) :: tl ->
          let env' = check_match env (p, v) in
          (match env' with
           | Some env -> eval_expr env e
           | None -> match_helper env v tl)
        | [] -> fail Pattern_matching_failed
      in
      match_helper env v (c :: cl)
    | ExprApply (e1, e2) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      (match v1 with
       | VFun (p, _, e, env) ->
         let* env' =
           match check_match env (p, v2) with
           | Some env -> return env
           | None -> fail Apply_failed
         in
         eval_expr env' e
       | VMutualFun (p, _, e, _) ->
         let* env' =
           match check_match env (p, v2) with
           | Some env -> return env
           | None -> fail Apply_failed
         in
         eval_expr env' e
       | VBuiltin (b, _) ->
         let status =
           match b, v2 with
           | BInt b, VInt i ->
             b i;
             Ok VUnit
           | BString b, VString s ->
             b s;
             Ok VUnit
           | _, _ -> Error VUnit
         in
         (match status with
          | Ok _ -> return VUnit
          | Error _ -> fail (Wrong_type v1))
       | _ -> fail BuiltinEvaluatingError)
    | ExprTuple (e1, e2, el) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      let* vl =
        Base.List.fold_left
          ~f:(fun acc e ->
            let* acc = acc in
            let* v = eval_expr env e in
            return (v :: acc))
          ~init:(return [])
          el
      in
      return (VTuple (v1 :: v2 :: List.rev vl))
    | ExprList (hd, tl) ->
      let* vhd = eval_expr env hd in
      let* vtl =
        Base.List.fold_left
          ~f:(fun acc e ->
            let* acc = acc in
            let* v = eval_expr env e in
            return (v :: acc))
          ~init:(return [])
          tl
      in
      return (VList (vhd :: List.rev vtl))
    | ExprCons (h, tl) ->
      let* hv = eval_expr env h in
      let* tlv = eval_expr env tl in
      (match tlv with
       | VList vl -> return (VList (hv :: vl))
       | VNil -> return (VList [ hv ])
       | VCycle _ -> return (VList [ hv ])
       | t -> fail (Wrong_type t))
    | ExprOption opt_expr ->
      (match opt_expr with
       | None -> return (VOption None)
       | Some e ->
         let* v = eval_expr env e in
         return (VOption (Some v)))
    | ExprType (e, _) -> eval_expr env e
    | _ -> fail Evaluationg_Need_ToBeReplaced

  and eval_non_rec_binding_list env bl =
    let* env2 =
      Base.List.fold_left
        ~f:(fun env b ->
          let* env = env in
          let p, e = b in
          match p with
          | PVar name ->
            let* v = eval_expr env e in
            return (extend env name v)
          | PType (PVar name, _) ->
            let* v = eval_expr env e in
            return (extend env name v)
          | PAny ->
            let _ = eval_expr env e in
            return env
          | PConstant CUnit ->
            let _ = eval_expr env e in
            return env
          | PTuple (p1, p2, pl) ->
            let* vl = eval_expr env e in
            let pl = p1 :: p2 :: pl in
            eval_tuple_binding env pl vl
          | POption (Some (PVar var)) ->
            let* v = eval_expr env e in
            (match v with
             | VOption (Some vo) -> return (extend env var vo)
             | _ -> fail Evaluationg_Need_ToBeReplaced)
          | _ -> return env)
        ~init:(return env)
        bl
    in
    return env2

  and eval_rec_binding_list env bl =
    let* env2 =
      Base.List.fold_left
        ~f:(fun env b ->
          let* env = env in
          let p, e = b in
          match p with
          | PVar name ->
            (match e with
             | ExprFun (p1, e1) ->
               return (extend env name (VMutualFun (p1, Rec, e1, env)))
             | ExprCons _ ->
               let env2 = extend env name (VCycle name) in
               let* v = eval_expr env2 e in
               let l =
                 match v with
                 | VList l -> l
                 | _ -> exit 1
               in
               (* https://stackoverflow.com/questions/26475516/how-do-i-write-a-function-to-create-a-circular-version-of-a-list-in-ocaml *)
               let cycle l =
                 if l = []
                 then invalid_arg "cycle"
                 else (
                   let l' = List.map (fun x -> x) l in
                   (* copy the list *)
                   let rec aux = function
                     | [] -> assert false
                     | [ _ ] as lst ->
                       (* find the last cons cell *)
                       (* and set the last pointer to the beginning of the list *)
                       Obj.set_field (Obj.repr lst) 1 (Obj.repr l')
                     | _ :: t -> aux t
                   in
                   aux l';
                   l')
               in
               let env3 = extend env2 name (VList (cycle l)) in
               return env3
             | _ -> return env)
          | _ -> return env)
        ~init:(return env)
        bl
    in
    return env2
  ;;

  let eval_structure_item (env : environment) structure_item =
    let env = extend env "print_int" (VBuiltin (BInt print_int, env)) in
    let env = extend env "print_endline" (VBuiltin (BString print_endline, env)) in
    match structure_item with
    | SEval e ->
      let _ = eval_expr env e in
      return env
    | SValue (NonRec, b, bl) ->
      let bindings = b :: bl in
      eval_non_rec_binding_list env bindings
    | SValue (Rec, b, bl) ->
      let bindings = b :: bl in
      eval_rec_binding_list env bindings
  ;;

  let eval_structure (s : structure) =
    Base.List.fold_left
      ~f:(fun env item ->
        let* env = env in
        let* env = eval_structure_item env item in
        return env)
      ~init:(return empty)
      s
  ;;
end

module Interpret = Eval (struct
    include Base.Result

    let ( let* ) m f = bind m ~f
  end)

let test_interpret s =
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    (match Interpret.eval_structure parsed with
     | Ok _ -> ()
     (* | Ok env -> print_env env *)
     | Error e -> fprintf std_formatter "%a\n" pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;
