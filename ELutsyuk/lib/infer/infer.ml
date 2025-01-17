(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

(* Template: https://gitlab.com/Kakadu/fp2020course-materials/-/tree/master/code/miniml?ref_type=heads*)

open Base
open Typ

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
  let update_state state = state + 1, Result.Ok state

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

  (* тип монады -- мапа из свободных переменных в типчики *)
  type t = (var_type, typ, Int.comparator_witness) Map.t

  (* пустое замещение *)
  let empty : t = Map.empty (module Int)

  (* замещение с одной заменой *)
  let singleton var typ =
    if Type.occurs_var var typ
    then fail (OccursCheckFailed (var, typ))
    else return (Map.singleton (module Int) var typ)
  ;;

  (* let apply subst =
     let rec helper = function
     | TypConst x -> TypConst x
     | TypVar b ->
     (match Map.find subst b with
     | Some v -> v
     | None -> TypVar b)
     | TypArrow (l, r) ->
     let l' = helper in
     let r' = helper in
     TypArrow (l', r')
     in
     helper
     ;; *)
end
