(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

(*Is used to check multiple returns. Go has no tuple type,
  multiple returns put the result bytes in stack
  https://stackoverflow.com/questions/18622706/what-exactly-is-happening-when-go-returns-multiple-values*)
type ctype =
  | Ctype of type'
  | Ctuple of type' list
[@@deriving show { with_path = false }, eq]

type global_env = ctype MapIdent.t
type local_env = ctype MapIdent.t list
type current_func = ident
type type_check = global_env * local_env * current_func option

module CheckMonad = struct
  open Errors
  open Format
  include BaseMonad

  type 'a t = (type_check, 'a) BaseMonad.t

  type env_t =
    | Loc
    | Glob

  let print_type = function
    | Ctype x -> PpType.print_type x
    | Ctuple x -> asprintf "(%s)" (String.concat ", " (List.map PpType.print_type x))
  ;;

  let retrieve_paris_first args = List.map (fun (x, _) -> x) args
  let retrieve_paris_second args = List.map (fun (_, x) -> x) args

  let retrieve_anon_func x =
    let args = retrieve_paris_second x.args in
    match x.returns with
    | Some (Only_types (x, y)) -> Type_func (args, x :: y)
    | Some (Ident_and_types (x, y)) -> Type_func (args, retrieve_paris_second (x :: y))
    | None -> Type_func (args, [])
  ;;

  let retrieve_type_const x =
    match x with
    | Const_array (x, y, _) -> return (Type_array (x, y))
    | Const_int _ -> return Type_int
    | Const_string _ -> return Type_string
    | Const_func x -> return (retrieve_anon_func x)
  ;;

  let read_local : 'a MapIdent.t list t =
    read
    >>= function
    | _, local, _ -> return local
  ;;

  let seek_local_definition_ident ident =
    read_local >>= fun local -> MapIdent.find_opt ident (List.hd local) |> return
  ;;

  let read_local_ident ident =
    read_local
    >>= fun local ->
    match List.find_opt (fun x -> MapIdent.mem ident x) local with
    | None -> return None
    | Some x -> return (MapIdent.find_opt ident x)
  ;;

  let read_global : 'a MapIdent.t t =
    read
    >>= function
    | global, _, _ -> return global
  ;;

  let read_global_ident ident =
    read_global >>= fun global -> MapIdent.find_opt ident global |> return
  ;;

  let rec fix f x = f (fix f) (return x)

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

  let save_local_ident_r env ident =
    seek_local_definition_ident ident
    >>= function
    | None -> write_local_ident env ident
    | Some _ ->
      fail
        (Type_check_error
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" ident (print_type env))))
  ;;

  let save_local_ident_l env ident =
    seek_local_definition_ident env
    >>= function
    | None -> write_local_ident ident env
    | Some _ ->
      fail
        (Type_check_error
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" env (print_type ident))))
  ;;

  let save_global_ident_r ident t =
    read_global_ident t
    >>= function
    | None -> write_global_ident ident t
    | Some _ ->
      fail
        (Type_check_error
           (Multiple_declaration
              (Printf.sprintf "%s is redeclared in %s" t (print_type ident))))
  ;;

  let save_global_ident_l ident t =
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

  let seek_ident ident =
    read_local_ident ident
    >>= function
    | Some _ -> return ()
    | None ->
      read_global_ident ident
      >>= (function
       | Some _ -> return ()
       | None ->
         fail
           (Type_check_error (Undefined_ident (Printf.sprintf "%s is not defined" ident))))
  ;;

  let get_func_name : current_func t =
    read
    >>= function
    | _, _, Some n -> return n
    | _ -> fail (Type_check_error Check_failed)
  ;;

  let write_env = read_local >>= fun x -> write_local (MapIdent.empty :: x)
  let delete_env = read_local >>= fun x -> write_local (List.tl x)

  let write_func_name func =
    read
    >>= function
    | g, l, _ -> write (g, l, Some func)
  ;;
end
