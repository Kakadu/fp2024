(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

type ctype =
  | Ctype of type'
  | Ctuple of type' list
[@@deriving show { with_path = false }, eq]

type global_env = ctype MapIdent.t
type local_env = ctype MapIdent.t list
type current_funcs = ctype list
type type_check = global_env * local_env * current_funcs

module CheckMonad = struct
  open Errors
  open Format
  include BaseMonad

  type 'a t = (type_check, 'a) BaseMonad.t

  let print_type = function
    | Ctype x -> PpType.print_type x
    | Ctuple x -> asprintf "(%s)" (String.concat ", " (List.map PpType.print_type x))
  ;;

  let read_local =
    read
    >>= function
    | _, local, _ -> return local
  ;;

  let seek_local_definition_ident ident =
    read_local >>= fun local -> MapIdent.find_opt ident (List.hd local) |> return
  ;;

  let delete_func =
    read
    >>= function
    | g, l, fl -> write (g, l, List.tl fl)
  ;;

  let write_func func =
    read
    >>= function
    | g, l, fl -> write (g, l, func :: fl)
  ;;

  let read_local_ident ident =
    read_local
    >>= fun local ->
    match List.find_opt (fun x -> MapIdent.mem ident x) local with
    | None -> return None
    | Some x -> return (MapIdent.find_opt ident x)
  ;;

  let read_global =
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

  let write_local_ident el_type el_ident =
    read_local
    >>= fun local ->
    write_local (MapIdent.add el_ident el_type (List.hd local) :: List.tl local)
  ;;

  let write_global new_global =
    read
    >>= function
    | _, local, fc -> write (new_global, local, fc)
  ;;

  let write_global_ident el_type el_ident =
    read_global >>= fun global -> write_global (MapIdent.add el_ident el_type global)
  ;;

  let save_local_ident env ident =
    seek_local_definition_ident env
    >>= function
    | None -> write_local_ident ident env
    | Some _ ->
      fail
        (Type_check_error
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" env (print_type ident))))
  ;;

  let save_global_ident ident t =
    read_global_ident ident
    >>= function
    | None -> write_global_ident t ident
    | Some _ ->
      fail
        (Type_check_error
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" ident (print_type t))))
  ;;

  let retrieve_ident ident =
    read_local_ident ident
    >>= function
    | Some x -> return x
    | None ->
      read_global_ident ident
      >>= (function
       | Some x -> return x
       | None ->
         fail
           (Type_check_error (Undefined_ident (Printf.sprintf "%s is not defined" ident))))
  ;;

  let get_func_return_type =
    read
    >>= function
    | _, _, [] -> fail (Type_check_error Check_failed)
    | _, _, fn -> return (List.hd fn)
  ;;

  let write_env = read_local >>= fun x -> write_local (MapIdent.empty :: x)
  let delete_env = read_local >>= fun x -> write_local (List.tl x)
end
