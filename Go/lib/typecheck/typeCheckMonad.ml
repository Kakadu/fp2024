(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open TypeCheckErrors

type error = TypeCheckError of type_check_error [@@deriving show { with_path = false }]

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

type global_env = type' MapIdent.t
type local_env = type' MapIdent.t
type current_func = ident
type type_check = global_env * local_env * current_func option

module BaseMonad = struct
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t

  let return : 'a -> ('st, 'a) t = fun x st -> st, Result.Ok x
  let fail : 'a -> ('st, 'b) t = fun e st -> st, Result.Error e

  let ( >>= ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Result.Ok x -> f x st1
    | Result.Error x -> fail x st1
  ;;

  let ( *> ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'b) t = fun x1 x2 -> x1 >>= fun _ -> x2

  let ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t =
    fun x f st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> return (f x) st
    | Result.Error er -> fail er st
  ;;

  let iter : ('a -> ('st, unit) t) -> 'a list -> ('st, unit) t =
    fun f list ->
    let f acc el = acc *> f el *> return () in
    List.fold_left f (return ()) list
  ;;

  let iter2 : ('a -> 'b -> ('st, unit) t) -> 'a list -> 'b list -> ('st, unit) t =
    fun f list1 list2 ->
    let f acc el1 el2 = acc *> f el1 el2 *> return () in
    List.fold_left2 f (return ()) list1 list2
  ;;

  let map : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t =
    fun f list ->
    let f acc el = acc >>= fun acc -> f el >>= fun el -> return (el :: acc) in
    List.fold_left f (return []) list >>| List.rev
  ;;

  let fold_left : ('a -> 'b -> ('st, 'a) t) -> 'a -> 'b list -> ('st, 'a) t =
    fun f acc l ->
    let f' acc a = acc >>= fun acc -> f acc a >>= return in
    List.fold_left f' (return acc) l
  ;;

  let read : ('st, 'st) t = fun st -> return st st
  let write : 'st -> ('st, _) t = fun st_new _ -> st_new, Result.Ok ()
  let run : ('st, 'a) t -> 'st -> 'st * ('a, error) Result.t = fun f st -> f st
end

module CheckMonad = struct
  open TypeCheckErrors
  open Format
  include BaseMonad

  type 'a t = (type_check, 'a) BaseMonad.t

  type env_t =
    | Loc
    | Glob

  let concat = String.concat ""

  let sep_by sep list print =
    let rec helper acc = function
      | fst :: snd :: tl ->
        let acc = concat [ acc; print fst; sep ] in
        helper acc (snd :: tl)
      | fst :: _ -> acc ^ print fst
      | [] -> acc
    in
    helper "" list
  ;;

  let sep_by_comma list print = sep_by ", " list print

  let rec print_type = function
    | Type_int -> "int"
    | Type_string -> "string"
    | Type_bool -> "bool"
    | Type_array (size, type') -> asprintf "[%i]%s" size (print_type type')
    | Type_func (arg_types, return_types) ->
      let print_returns =
        match return_types with
        | _ :: _ :: _ -> asprintf " (%s)" (sep_by_comma return_types print_type)
        | type' :: _ -> " " ^ print_type type'
        | [] -> ""
      in
      asprintf "func(%s)%s" (sep_by_comma arg_types print_type) print_returns
    | Type_chan (chan_dir, t) ->
      let print_chan_dir =
        match chan_dir with
        | Chan_bidirectional -> "chan"
        | Chan_receive -> "<-chan"
        | Chan_send -> "chan<-"
      in
      let print_type =
        match t with
        | Type_chan (Chan_receive, _) -> asprintf "(%s)" (print_type t)
        | _ -> asprintf "%s" (print_type t)
      in
      asprintf "%s %s" print_chan_dir print_type
  ;;

  let retrieve_paris_first args = List.map (fun (x, _) -> x) args
  let retrieve_paris_second args = List.map (fun (_, x) -> x) args

  let retrieve_anon_func x =
    let args = retrieve_paris_second x.args in
    match x.returns with
    | Some x ->
      (match x with
       | Only_types (x, y) -> Type_func (args, x :: y)
       | Ident_and_types (x, y) -> Type_func (args, retrieve_paris_second (x :: y)))
    | None -> Type_func (args, [])
  ;;

  let retrieve_type_const x =
    match x with
    | Const_array (x, y, _) -> return (Type_array (x, y))
    | Const_int _ -> return Type_int
    | Const_string _ -> return Type_string
    | Const_func x -> return (retrieve_anon_func x)
  ;;

  let read_local : 'a MapIdent.t t =
    read
    >>= function
    | _, local, _ -> return local
  ;;

  let read_local_ident ident =
    read_local >>= fun local -> MapIdent.find_opt ident local |> return
  ;;

  let read_global : 'a MapIdent.t t =
    read
    >>= function
    | global, _, _ -> return global
  ;;

  let read_global_ident ident =
    read_global >>= fun global -> MapIdent.find_opt ident global |> return
  ;;

  let write_local new_local =
    read
    >>= function
    | global, _, fc -> write (global, new_local, fc)
  ;;

  let write_local_ident el_env el_ident =
    read_local >>= fun local -> write_local (MapIdent.add el_ident el_env local)
  ;;

  let write_global new_global =
    read
    >>= function
    | _, local, fc -> write (new_global, local, fc)
  ;;

  let write_global_ident el_env el_ident =
    read_global >>= fun global -> write_global (MapIdent.add el_ident el_env global)
  ;;

  let save_local_ident_r env ident =
    read_local_ident ident
    >>= function
    | None -> write_local_ident env ident
    | Some _ ->
      fail
        (TypeCheckError
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" ident (print_type env))))
  ;;

  let save_local_ident_l env ident =
    read_local_ident env
    >>= function
    | None -> write_local_ident ident env
    | Some _ ->
      fail
        (TypeCheckError
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" env (print_type ident))))
  ;;

  let save_global_ident_r ident t =
    read_global_ident t
    >>= function
    | None -> write_global_ident ident t
    | Some _ ->
      fail
        (TypeCheckError
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" t (print_type ident))))
  ;;

  let save_global_ident_l ident t =
    read_global_ident ident
    >>= function
    | None -> write_global_ident t ident
    | Some _ ->
      fail
        (TypeCheckError
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" ident (print_type t))))
  ;;

  let read_ident ident =
    read_local_ident ident
    >>= function
    | Some x -> return x
    | None ->
      read_global_ident ident
      >>= (function
       | Some x -> return x
       | None ->
         fail
           (TypeCheckError (Undefined_ident (Printf.sprintf "%s is not defined" ident))))
  ;;

  let seek_ident ident =
    read_global_ident ident
    >>= function
    | Some _ -> return ()
    | None ->
      read_local_ident ident
      >>= (function
       | Some _ -> return ()
       | None ->
         fail
           (TypeCheckError (Undefined_ident (Printf.sprintf "%s is not defined" ident))))
  ;;

  let get_func_name : current_func t =
    read
    >>= function
    | _, _, Some n -> return n
    | _ -> fail (TypeCheckError Check_failed)
  ;;

  let write_func_name func =
    read
    >>= function
    | g, l, _ -> write (g, l, Some func)
  ;;
end
