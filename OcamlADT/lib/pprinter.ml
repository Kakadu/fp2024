(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Angstrom
open Ast
open Format

let pprint_constant fmt =
  let open Constant in
  function
  | Const_integer n -> fprintf fmt "%d" n
  | Const_char c -> fprintf fmt "'%c'" c
  | Const_string s -> fprintf fmt "\"%s\"" s
;;

let rec pprint_type fmt =
  let open TypeExpr in
  function
  | Type_arrow (tye1, tye2) -> fprintf fmt "%a -> %a" pprint_type tye1 pprint_type tye2
  | Type_var id -> fprintf fmt "%s" id
  | Type_tuple (tye1, tye2, tyel) ->
    fprintf
      fmt
      "%a * %a%s"
      pprint_type
      tye1
      pprint_type
      tye2
      (if List.is_empty tyel
       then ""
       else
         " * "
         ^ String.concat
             ~sep:" * "
             (List.map tyel ~f:(fun t -> asprintf "%a" pprint_type t)))
;;

let rec pprint_pattern fmt =
  let open Pattern in
  function
  | Pat_constraint (p, tye) -> fprintf fmt "(%a : %a)" pprint_pattern p pprint_type tye
  | Pat_any -> fprintf fmt "_"
  | Pat_var id -> fprintf fmt "%s" id
  | Pat_constant c -> pprint_constant fmt c
  | Pat_tuple (p1, p2, pl) ->
    fprintf
      fmt
      "(%a, %a%s)"
      pprint_pattern
      p1
      pprint_pattern
      p2
      (if List.is_empty pl
       then ""
       else
         ", "
         ^ String.concat
             ~sep:", "
             (List.map pl ~f:(fun p -> asprintf "%a" pprint_pattern p)))
  | Pat_construct (id, None) -> fprintf fmt "%s" id
  | Pat_construct (id, Some p) ->
    (match p with
     | Pat_tuple _ -> fprintf fmt "%s (%a)" id pprint_pattern p
     | _ -> fprintf fmt "%s %a" id pprint_pattern p)
;;

let pprint_rec fmt =
  let open Expression in
  function
  | Nonrecursive -> fprintf fmt ""
  | Recursive -> fprintf fmt "rec "
;;

let rec pprint_expression fmt =
  let open Expression in
  function
  | Exp_ident id -> fprintf fmt "%s" id
  | Exp_constant ct -> pprint_constant fmt ct
  | Exp_tuple (ex1, ex2, exl) ->
    fprintf
      fmt
      "(%a, %a%s)"
      pprint_expression
      ex1
      pprint_expression
      ex2
      (if List.is_empty exl
       then ""
       else
         ", "
         ^ String.concat
             ~sep:", "
             (List.map exl ~f:(fun ex -> asprintf "%a" pprint_expression ex)))
  | Exp_function (cs1, csl) ->
    fprintf
      fmt
      "function \n| %a\n| %s"
      pprint_case
      cs1
      (String.concat
         ~sep:"\n  | "
         (List.map csl ~f:(fun c -> asprintf "%a" pprint_case c)))
  | Exp_fun (ptl1, exp) ->
    let pt1, ptl = ptl1 in
    fprintf
      fmt
      "fun %a%s -> %a"
      pprint_pattern
      pt1
      (String.concat ~sep:"" (List.map ptl ~f:(fun p -> asprintf " %a" pprint_pattern p)))
      pprint_expression
      exp
  | Exp_apply (ex1, ex2)
    when Expression.equal (Exp_ident "+") ex1
         || Expression.equal (Exp_ident "-") ex1
         || Expression.equal (Exp_ident "*") ex1
         || Expression.equal (Exp_ident "/") ex1 ->
    (match ex2 with
     | Expression.Exp_tuple list2 ->
       let first, second, _ = list2 in
       if Expression.equal (Exp_ident "+") ex1 || Expression.equal (Exp_ident "-") ex1
       then
         fprintf
           fmt
           "(%a %a %a)"
           pprint_expression
           first
           pprint_expression
           ex1
           pprint_expression
           second
       else
         fprintf
           fmt
           "%a %a %a"
           pprint_expression
           first
           pprint_expression
           ex1
           pprint_expression
           second
     | _ -> fprintf fmt "Invalid operator application")
  | Exp_apply (ex1, ex2) ->
    fprintf fmt "%a(%a)" pprint_expression ex1 pprint_expression ex2
  | Exp_match (ex, csl1) ->
    let cs, csl = csl1 in
    fprintf
      fmt
      "match %a with\n  %a%s"
      pprint_expression
      ex
      pprint_case
      cs
      (if List.is_empty csl
       then ""
       else
         " | "
         ^ String.concat
             ~sep:"\n  | "
             (List.map csl ~f:(fun cs -> asprintf "%a" pprint_case cs)))
  | Exp_constraint (ex, tye) ->
    fprintf fmt "(%a : %a)" pprint_expression ex pprint_type tye
  | Exp_if (ex1, ex2, None) ->
    fprintf fmt "if %a\n  then  %a" pprint_expression ex1 pprint_expression ex2
  | Exp_if (ex1, ex2, Some ex3) ->
    fprintf
      fmt
      "if %a\n  then %a\n  else %a"
      pprint_expression
      ex1
      pprint_expression
      ex2
      pprint_expression
      ex3
  | Exp_let (rec_fl, vbindl1, ex) ->
    let vbind1, vbindl = vbindl1 in
    fprintf
    fmt
    "let %a%a %s%s in %a"
    pprint_rec
    rec_fl
    pprint_value_binding
    vbind1
    (if List.length vbindl > 0 then " and " else "")
    (String.concat
    ~sep:" and "
    (List.map vbindl ~f:(fun vb -> asprintf "%a" pprint_value_binding vb)))
    pprint_expression
    ex
  | Exp_construct (id, None) -> fprintf fmt "%s" id
  | Exp_construct (id, Some exp) -> fprintf fmt "%s %a" id pprint_expression exp

and pprint_value_binding fmt vb =
  let open Expression in
  fprintf fmt "%a = %a" pprint_pattern vb.pat pprint_expression vb.expr

and pprint_case fmt case =
  let open Expression in
  fprintf fmt "%a -> %a" pprint_pattern case.first pprint_expression case.second
;;

let pprint_structure_item fmt =
  let open Structure in
  function
  | Str_eval exp -> fprintf fmt "%a ;;\n" pprint_expression exp
  | Str_value (rec_flag, vbindl1) ->
    let vbind1, vbindl = vbindl1 in
    let bindings_str =
      match vbindl with
      | [] -> ""
      | _ ->
        String.concat
          ~sep:" and\n  "
          (List.map vbindl ~f:(fun vb -> asprintf "%a" pprint_value_binding vb))
    in
    fprintf
      fmt
      "let %a %a %s;;\n\n"
      pprint_rec
      rec_flag
      pprint_value_binding
      vbind1
      (if phys_equal bindings_str "" then "" else "and\n  " ^ bindings_str)
;;

(* | Str_adt (id, id_t_l) ->
   (* Stub: just using the variables id and id_t_l (ftm) *)
   let _ = id in
   let _ = id_t_l in
   () *)

let pprint_program fmt = List.iter ~f:(pprint_structure_item fmt)

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error _ -> print_endline "Syntax error"
;;
