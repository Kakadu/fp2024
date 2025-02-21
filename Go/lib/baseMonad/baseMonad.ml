(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

type ('st, 'a) t = 'st -> 'st * ('a, Errors.error) Result.t

let return x st = st, Result.Ok x
let fail e st = st, Result.Error e

let ( >>= ) x f st =
  let st1, x1 = x st in
  match x1 with
  | Result.Ok x -> f x st1
  | Result.Error x -> fail x st1
;;

let ( let* ) = ( >>= )
let ( *> ) x1 x2 = x1 >>= fun _ -> x2

let ( >>| ) x f st =
  let st, x = x st in
  match x with
  | Result.Ok x -> return (f x) st
  | Result.Error er -> fail er st
;;

let iter f =
  let f acc el = acc *> f el *> return () in
  List.fold_left f (return ())
;;

let iter2 f =
  let f acc el1 el2 = acc *> f el1 el2 *> return () in
  List.fold_left2 f (return ())
;;

let map f list =
  let f acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
  List.fold_left f (return []) list >>| List.rev
;;

let fold_left f acc l =
  let f' acc a = acc >>= fun acc -> f acc a >>= return in
  List.fold_left f' (return acc) l
;;

let read st = return st st
let write st_new _ = st_new, Result.Ok ()
let run f = f
