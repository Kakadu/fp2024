(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

(* Template: https://gitlab.com/Kakadu/fp2020course-materials/-/tree/master/code/miniml?ref_type=heads*)

open Base
open Typ
open Ast

type var_type = int

module Res = struct
  (* тип int выражает состояние *)
  type ('a, 'error) t = int -> int * ('a, 'error) Result.t

  (* функция для создания Ok-монады из значения *)
  let return : 'a -> ('a, 'error) t = fun x st -> st, Result.Ok x

  (* функция для создания Error-монады из значения *)
  let fail : 'error -> ('a, 'error) t = fun err state -> state, Result.Error err

  (* блин оч странно, зачем нужна анонимная функци, если можно просто прописать
     [let fail err state = state, Result.Error err] *)
  let fresh_num old_var = old_var + 1, Result.Ok old_var

  (* функция для связывания двух вычислительных операторов *)
  let ( >>= ) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t =
    fun first_operator sec_operator state ->
    let state, res = first_operator state in
    match res with
    | Result.Ok value -> sec_operator value state
    | Result.Error error -> fail error state
  ;;

  module SyntSugar = struct
    let ( let* ) = ( >>= )
  end

  (* берём результат применения оератора к нулю (то есть второе значение) *)
  let run operator = snd (operator 0)
end

(* эт чоооо, выходит, мы переменные чисто нумеруем... имена нас ВАЩЕ не волнуют *)
(* тип множества целых чисел с функциональностью из Base *)
(* отсюда вопрос... почему не взять тупо сразу Base.Set, а через определение своего типа?
   ответ: так можно сделать удобный интерфейс для взаимодействия с модулем *)
module VarSet = struct
  include Set
  (* неверно писать [open Base.Set], [open] делает видимым содержимое модуля
     при [include] текущий модуль наследует всё содержимое инклюднутого модуля *)

  (* тип *)
  type t = (var_type, Int.comparator_witness) Set.t

  (* если не объявить отдеально, каждый раз придётся передавать тип компаратора... не удобно *)
  let empty = Set.empty (module Int)
end

module Type = struct
  (* проверяет, встречается ли эта переменная в типе... гпт пишет, что это нужно для предотвращения
     циклических зависимостей при попытке вывести тип переменной *)
  let rec occurs_var var = function
    | TypConst _ -> false
    | TypVar x -> x = var
    | TypArrow (l, r) -> occurs_var var l || occurs_var var r
    | TypTuple (typ1, typ2, typs) ->
      List.exists (typ1 :: typ2 :: typs) ~f:(occurs_var var)
    | TypList typ -> occurs_var var typ
    | TypOption typ -> occurs_var var typ
  ;;

  (* возвращает множество свободных переменных в типе (важно для схемы)
     свободные переменные -- те, которые не связаны с конкретными значениями *)
  let free_vars typ =
    let rec helper acc = function
      | TypConst _ -> acc
      | TypVar typ -> VarSet.add acc typ
      | TypArrow (l, r) ->
        let left_vars = helper acc l in
        helper left_vars r
      | TypTuple (typ1, typ2, typs) ->
        let typ_list = typ1 :: typ2 :: typs in
        let collect_free_vars acc typ = VarSet.union acc (helper VarSet.empty typ) in
        List.fold typ_list ~init:acc ~f:(fun acc typ -> collect_free_vars acc typ)
      | TypList typ -> helper acc typ
      | TypOption typ -> helper acc typ
    in
    helper VarSet.empty typ
  ;;

  (* сравнивает два типа на равенство
     для базовых типов сравниваем их совпадения, для var возращаем true, для стрелочных
     рекурсивно сравниваем их части *)
  let equal =
    let rec cmp l r =
      match l, r with
      | TypConst l, TypConst r -> String.equal l r
      | TypVar _, TypVar _ -> true
      | TypArrow (l1, r1), TypArrow (l2, r2) -> cmp l1 l2 && cmp r1 r2
      | TypTuple (l1, l2, ls), TypTuple (r1, r2, rs) ->
        let cmp_base = cmp l1 r1 && cmp l2 r2 in
        let cmp_lists =
          match List.fold2 ls rs ~init:true ~f:(fun acc l r -> acc && cmp l r) with
          | Ok res -> res
          | Unequal_lengths -> false
        in
        cmp_base && cmp_lists
      | TypList l, TypList r -> cmp l r
      | TypOption l, TypOption r -> cmp l r
      | _ -> false
    in
    cmp
  ;;
end

module Substitutions = struct
  open Res
  open Res.SyntSugar

  (* тип монады -- мапа из свободных переменных в типчики *)
  type t = (var_type, typ, Int.comparator_witness) Map.t

  (* пустое замещение *)
  let empty : t = Map.empty (module Int)

  (* замещение с одной заменой *)
  (* важно, что input -- это кортеж *)
  let singleton (var, typ) =
    if Type.occurs_var var typ
    then fail (OccursCheckFailed (var, typ))
    else return (Map.singleton (module Int) var typ)
  ;;

  let apply subst =
    let rec try_apply_to = function
      | TypConst x -> TypConst x (* потому что нет свободных переменных *)
      | TypVar var ->
        (* ищем такой ключ (как содержимое нашего типа) в подстановке *)
        (match Map.find subst var with
         | Some typ -> typ
         | None -> TypVar var)
      | TypArrow (l, r) -> TypArrow (try_apply_to l, try_apply_to r)
      | TypTuple (typ1, typ2, typs) ->
        let applyed_list = List.map typs ~f:try_apply_to in
        TypTuple (try_apply_to typ1, try_apply_to typ2, applyed_list)
      | TypList typ -> TypList (try_apply_to typ)
      | TypOption typ -> TypOption (try_apply_to typ)
    in
    try_apply_to
  ;;

  (* возвращает substitution, если типы не совпадают *)
  let rec unify l r =
    match l, r with
    | TypConst typ1, TypConst typ2 when String.equal typ1 typ2 -> return empty
    | TypConst _, TypConst _ -> fail (UnificationFailed (l, r))
    | TypVar l, TypVar r when l = r -> return empty
    | TypVar var, typ | typ, TypVar var -> singleton (var, typ)
    | TypArrow (l1, r1), TypArrow (l2, r2) ->
      let* subst1 = unify l1 l2 in
      (* как в статье, для общей подстановки S1 применяем к S2, затем наоборот и комбинируем *)
      let* subst2 = unify (apply subst1 r1) (apply subst1 r2) in
      compose subst1 subst2
    | TypTuple (l1, l2, ls), TypTuple (r1, r2, rs) ->
      if List.length ls <> List.length rs
      then fail (UnificationFailed (l, r))
      else
        Base.List.fold_left
          (Base.List.zip_exn (l1 :: l2 :: ls) (r1 :: r2 :: rs))
          ~f:(fun subst (l, r) ->
            let* new_subst = unify l r in
            let* curr_subst = subst in
            compose curr_subst new_subst)
          ~init:(return empty)
    | TypList l, TypList r -> unify l r
    | TypOption l, TypOption r -> unify l r
    | _ -> fail (UnificationFailed (l, r))

  and extend subst (new_var, new_typ) =
    match Map.find subst new_var with
    (* если нашлась такая переменная, то нужно применить новую замену *)
    | Some typ ->
      let* new_subst = unify new_typ typ in
      compose subst new_subst
    (* чтобы применить compose, нужно обязательно перед этим сделать binding *)
    (* если не нашлась такая замена, то нужно всё равно применить и ещё и добавить её к мапе существующих *)
    | None ->
      let new_typ = apply subst new_typ in
      let* new_subst = singleton (new_var, new_typ) in
      Map.fold subst ~init:(return new_subst) ~f:(fun ~key:next_var ~data:next_typ map ->
        let next_typ = apply new_subst next_typ in
        (* проверка ниже нужна по той же причине, по которой мы её проводит, когда создаём новую подстановку *)
        (* выходит, что новую подстановку добавляем только через унификацию *)
        if Type.occurs_var next_var next_typ
        then fail (OccursCheckFailed (next_var, next_typ))
        else
          let* map = map in
          return (Map.set map ~key:next_var ~data:next_typ))

  and compose subst1 subst2 =
    Map.fold subst1 ~init:(return subst2) ~f:(fun ~key:next_var ~data:next_typ map ->
      let* map = map in
      extend map (next_var, next_typ))
  ;;

  let compose_all subst_map =
    List.fold_left subst_map ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module Scheme = struct
  (* то есть получается.. тип схемы -- это S(мн-во, его_тип) *)
  type t = S of VarSet.t * typ

  (* возвращаем список свободных переменных, то есть уже определённых *)
  let free_vars (S (var_list, ty)) = VarSet.diff (Type.free_vars ty) var_list

  let apply (S (s, ty)) subst =
    let subst2 = VarSet.fold s ~init:subst ~f:(fun acc k -> Base.Map.remove acc k) in
    S (s, Substitutions.apply subst2 ty)
  ;;

  let equal (S (s1, ty1)) (S (s2, ty2)) = VarSet.equal s1 s2 && Type.equal ty1 ty2
end

module TypeEnv = struct
  include Base.Map

  type t = (id, Scheme.t, Base.String.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.String)

  (* собирает все свободные переменные среды *)
  let free_vars env =
    Base.Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:sch acc ->
      VarSet.union acc (Scheme.free_vars sch))
  ;;

  let apply env subst = Base.Map.map env ~f:(fun sch -> Scheme.apply sch subst)
  let extend env (id, sch) = Base.Map.set env ~key:id ~data:sch
  let equal = Base.Map.equal Scheme.equal

  (* Returns list of variable id's which occur only in one of two enviroments *)
  let vars_diff : t -> t -> id list =
    fun env1 env2 ->
    Base.Map.fold2 env1 env2 ~init:[] ~f:(fun ~key:id ~data:v acc ->
      match v with
      | `Left _ | `Right _ -> id :: acc
      | `Both _ -> acc)
  ;;

  (* Returns list of variables with different type schemes in two enviroments *)
  let schemes_diff env1 env2 =
    Base.Map.fold2 env1 env2 ~init:[] ~f:(fun ~key:id ~data:v acc ->
      match v with
      | `Both (l, r) when not (Scheme.equal l r) -> (id, l, r) :: acc
      | _ -> acc)
  ;;

  (* Returns list of variables with different types in two enviroments *)
  let types_diff env1 env2 =
    Base.List.fold_left
      (schemes_diff env1 env2)
      ~init:[]
      ~f:(fun acc (id, S (_, ty1), S (_, ty2)) ->
        if Type.equal ty1 ty2 then acc else (id, ty1, ty2) :: acc)
  ;;
end
