(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Angstrom
open Ast
open Stdlib.Format

let get_op_pr id =
  let open Expression in
  match id with
  | Exp_ident "&&" -> 3
  | Exp_ident "||" -> 2
  | Exp_ident ">"
  | Exp_ident "<"
  | Exp_ident ">="
  | Exp_ident "<="
  | Exp_ident "<>"
  | Exp_ident "=" -> 4
  | Exp_ident "+" | Exp_ident "-" -> 5
  | Exp_ident "*" | Exp_ident "/" -> 6
  | Exp_if (_, _, _) -> 1
  | Exp_let (_, _, _)
  | Exp_match (_, _)
  | Exp_function _
  | Exp_fun (_, _)
  | Exp_constant _ | Exp_ident _ -> 0
  | Exp_apply (_, _) | Exp_construct _ -> 7
  | _ -> 0
;;

let pprint_constant fmt =
  let open Constant in
  function
  | Const_integer n -> fprintf fmt "%d" n
  | Const_char c -> fprintf fmt "'%c'" c
  | Const_string s -> fprintf fmt "%S" s
;;

let rec pprint_type fmt =
  let open TypeExpr in
  function
  | Type_arrow (tye1, tye2) -> fprintf fmt "(%a -> %a)" pprint_type tye1 pprint_type tye2
  | Type_var id -> fprintf fmt "%s" id
  | Type_tuple (tye1, tye2, tyel) ->
    fprintf
      fmt
      "(%a * %a%s)"
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
  | Pat_construct (id, None) -> fprintf fmt "(%s)" id
  | Pat_construct (id, Some p) ->
    (match p with
     | Pat_tuple _ -> fprintf fmt "(%s (%a))" id pprint_pattern p
     | _ -> fprintf fmt "%s %a" id pprint_pattern p)
;;

let pprint_rec fmt =
  let open Expression in
  function
  | Nonrecursive -> fprintf fmt ""
  | Recursive -> fprintf fmt "rec "
;;

let rec pprint_expression fmt (n : int) =
  let open Expression in
  function
  | Exp_ident id -> fprintf fmt "%s" id
  | Exp_constant ct -> pprint_constant fmt ct
  | Exp_tuple (ex1, ex2, exl) ->
    let op_pr1 = get_op_pr ex1 in
    let op_pr2 = get_op_pr ex2 in
    fprintf
      fmt
      "(%a, %a%s)"
      (fun fmt -> pprint_expression fmt (op_pr1 + 1))
      ex1
      (fun fmt -> pprint_expression fmt (op_pr2 + 1))
      ex2
      (if List.is_empty exl
       then ""
       else
         ", "
         ^ String.concat
             ~sep:", "
             (List.map exl ~f:(fun ex ->
                let op_pr_t = get_op_pr ex in
                asprintf "%a" (fun fmt -> pprint_expression fmt (op_pr_t + 1)) ex)))
  | Exp_function (cs1, csl) when n > 0 ->
    fprintf fmt "(%a)" pprint_function_with_cases (cs1, csl, n + 1)
  | Exp_function (cs1, csl) ->
    fprintf fmt "%a" pprint_function_with_cases (cs1, csl, n + 1)
  | Exp_fun (ptl1, exp) ->
    let pt1, ptl = ptl1 in
    let if_string =
      asprintf
        "fun %a%s -> %a"
        pprint_pattern
        pt1
        (String.concat
           ~sep:""
           (List.map ptl ~f:(fun p -> asprintf " %a" pprint_pattern p)))
        (fun fmt -> pprint_expression fmt n)
        exp
    in
    if n > 0 then fprintf fmt "(%s)" if_string else fprintf fmt "%s" if_string
  | Exp_apply (ex1, ex2) ->
    let op_pr = get_op_pr ex1 in
    (match op_pr with
     | 2 | 3 ->
       (match ex2 with
        | Expression.Exp_tuple (first, second, _) ->
          let apply_binop_r_string =
            asprintf
              "%a %a %a"
              (fun fmt -> pprint_expression fmt (op_pr + 1))
              first
              (fun fmt -> pprint_expression fmt (op_pr + 1))
              ex1
              (fun fmt -> pprint_expression fmt op_pr)
              second
          in
          if n > op_pr
          then fprintf fmt "(%s)" apply_binop_r_string
          else fprintf fmt "%s" apply_binop_r_string
        | _ -> fprintf fmt "not covered\n")
     | 5 | 6 | 4 ->
       (match ex2 with
        | Expression.Exp_tuple (first, second, _) ->
          let apply_binop_l_string =
            asprintf
              "%a %a %a"
              (fun fmt -> pprint_expression fmt op_pr)
              first
              (fun fmt -> pprint_expression fmt (op_pr + 1))
              ex1
              (fun fmt -> pprint_expression fmt (op_pr + 1))
              second
          in
          if n > op_pr
          then fprintf fmt "(%s)" apply_binop_l_string
          else fprintf fmt "%s" apply_binop_l_string
        | _ -> fprintf fmt "not covered\n")
     | _ ->
       let apply_string =
         asprintf
           "%a %a"
           (fun fmt -> pprint_expression fmt (op_pr + 1))
           ex1
           (fun fmt -> pprint_expression fmt (op_pr + 1))
           ex2
       in
       fprintf fmt "(%s)" apply_string)
  | Exp_match (ex, csl1) ->
    let cs, csl = csl1 in
    let op_pr1 = get_op_pr ex in
    let match_string =
      asprintf
        "match %a with\n  | %a%s"
        (fun fmt -> pprint_expression fmt (op_pr1 + 1))
        ex
        (fun fmt -> pprint_case fmt n)
        cs
        (if List.is_empty csl
         then ""
         else
           " | "
           ^ String.concat
               ~sep:"\n  | "
               (List.map csl ~f:(fun cs ->
                  asprintf "%a" (fun fmt -> pprint_case fmt n) cs)))
    in
    if n > 0 then fprintf fmt "(%s)" match_string else fprintf fmt "%s" match_string
  | Exp_constraint (ex, tye) ->
    fprintf fmt "(%a : %a)" (fun fmt -> pprint_expression fmt (n + 1)) ex pprint_type tye
  | Exp_if (ex1, ex2, None) ->
    let if_string =
      asprintf
        "if %a\n  then %a"
        (fun fmt -> pprint_expression fmt (n + 1))
        ex1
        (fun fmt -> pprint_expression fmt (n + 1))
        ex2
    in
    if n > 0 then fprintf fmt "(%s)" if_string else fprintf fmt "%s" if_string
  | Exp_if (ex1, ex2, Some ex3) ->
    let if_string =
      asprintf
        "if %a\n  then %a\n  else %a"
        (fun fmt -> pprint_expression fmt (n + 1))
        ex1
        (fun fmt -> pprint_expression fmt (n + 1))
        ex2
        (fun fmt -> pprint_expression fmt (n + 1))
        ex3
    in
    if n > 0 then fprintf fmt "(%s)" if_string else fprintf fmt "%s" if_string
  | Exp_let (rec_fl, vbindl1, ex) ->
    let vbind1, vbindl = vbindl1 in
    let let_string =
      asprintf
        "let %a%a%s in %a"
        pprint_rec
        rec_fl
        (fun fmt -> pprint_value_binding fmt n)
        vbind1
        (if List.is_empty vbindl
         then ""
         else
           " and "
           ^ String.concat
               ~sep:" and "
               (List.map vbindl ~f:(fun vb ->
                  asprintf "%a" (fun fmt -> pprint_value_binding fmt n) vb)))
        (fun fmt -> pprint_expression fmt (n + 1))
        ex
    in
    if n > 0 then fprintf fmt "(%s)" let_string else fprintf fmt "%s" let_string
  | Exp_construct (id, None) -> fprintf fmt "(%s)" id
  | Exp_construct (id, Some exp) ->
    fprintf fmt "(%s (%a))" id (fun fmt -> pprint_expression fmt (n + 1)) exp

and pprint_value_binding fmt n vb =
  let open Expression in
  fprintf
    fmt
    "%a = %a"
    pprint_pattern
    vb.pat
    (fun fmt -> pprint_expression fmt (n + 1))
    vb.expr

and pprint_case fmt n case =
  let open Expression in
  fprintf
    fmt
    "%a -> %a"
    pprint_pattern
    case.first
    (fun fmt -> pprint_expression fmt (n + 1))
    case.second

and pprint_function_with_cases fmt (cs, csl, n) =
  fprintf
    fmt
    "function \n  | %a%s"
    (fun fmt -> pprint_case fmt n)
    cs
    (String.concat
       (List.map csl ~f:(fun c -> asprintf "\n  | %a" (fun fmt -> pprint_case fmt n) c)))
;;

let pprint_structure_item fmt n =
  let open Structure in
  function
  | Str_eval exp -> fprintf fmt "%a ;;\n" (fun fmt -> pprint_expression fmt n) exp
  | Str_value (rec_flag, vbindl1) ->
    let vbind1, vbindl = vbindl1 in
    let bindings_str =
      match vbindl with
      | [] -> ""
      | _ ->
        " and "
        ^ String.concat
            ~sep:" and\n  "
            (List.map vbindl ~f:(fun vb ->
               asprintf "%a" (fun fmt -> pprint_value_binding fmt n) vb))
    in
    fprintf
      fmt
      "let %a %a%s;;\n\n"
      pprint_rec
      rec_flag
      (fun fmt -> pprint_value_binding fmt n)
      vbind1
      bindings_str
;;

(* | Str_adt (id, id_t_l) ->
   (* Stub: just using the variables id and id_t_l (ftm) *)
   let _ = id in
   let _ = id_t_l in
   () *)

let pprint_program fmt = List.iter ~f:(pprint_structure_item fmt 0)

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer std_formatter res
  | Error _ -> Stdio.print_endline "Syntax error"
;;
