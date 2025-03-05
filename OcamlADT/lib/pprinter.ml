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
  | Type_var id -> fprintf fmt "'%s" id
  | Type_tuple (tye1, tye2, tyel) ->
    fprintf
      fmt
      "(%s)"
      (String.concat
         ~sep:" * "
         (List.map (tye1 :: tye2 :: tyel) ~f:(fun t -> asprintf "%a" pprint_type t)))
  | Type_construct (id, tyel) ->
    let tyel_str =
      String.concat
        ~sep:", "
        (List.map tyel ~f:(fun t ->
           match t with
           | Type_var tye -> asprintf "'%s" tye
           | Type_tuple (t1, t2, rest) ->
             let tuple_types = t1 :: t2 :: rest in
             let tuple_str = String.concat ~sep:" * " (List.map tuple_types ~f:show) in
             "(" ^ tuple_str ^ ")"
           | _ -> show t))
    in
    let tyel_strf =
      match List.length tyel with
      | 0 -> ""
      | 1 -> tyel_str ^ " "
      | _ -> "(" ^ tyel_str ^ ") "
    in
    fprintf fmt "%s%s" tyel_strf id
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
      "(%s)"
      (String.concat
         ~sep:", "
         (List.map (p1 :: p2 :: pl) ~f:(fun p -> asprintf "%a" pprint_pattern p)))
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

let rec pprint_expression fmt n =
  let open Expression in
  function
  | Exp_ident id -> fprintf fmt "%s" id
  | Exp_constant ct -> pprint_constant fmt ct
  | Exp_tuple (ex1, ex2, exl) ->
    fprintf
      fmt
      "(%s)"
      (String.concat
         ~sep:", "
         (List.map (ex1 :: ex2 :: exl) ~f:(fun ex ->
            let op_pr_t = get_op_pr ex in
            asprintf "%a" (fun fmt -> pprint_expression fmt (op_pr_t + 1)) ex)))
  | Exp_function (cs1, csl) when n > 0 ->
    fprintf fmt "(%a)" pprint_function_with_cases (cs1, csl, n + 1)
  | Exp_function (cs1, csl) ->
    fprintf fmt "%a" pprint_function_with_cases (cs1, csl, n + 1)
  | Exp_fun ((pt1, ptl), exp) ->
    let if_string =
      asprintf
        "fun%s -> %a"
        (String.concat
           ~sep:""
           (List.map (pt1 :: ptl) ~f:(fun p -> asprintf " %a" pprint_pattern p)))
        (fun fmt -> pprint_expression fmt n)
        exp
    in
    if n > 0 then fprintf fmt "(%s)" if_string else fprintf fmt "%s" if_string
  | Exp_apply (ex1, ex2) ->
    let op_pr = get_op_pr ex1 in
    let format_apply =
      match ex2 with
      | Expression.Exp_tuple (first, second, _)
        when List.mem [ 2; 3; 4; 5; 6 ] op_pr ~equal:Int.equal ->
        let left_pr, right_pr =
          if List.mem [ 2; 3 ] op_pr ~equal:Int.equal
          then op_pr + 1, op_pr
          else op_pr, op_pr + 1
        in
        asprintf
          "%a %a %a"
          (fun fmt -> pprint_expression fmt left_pr)
          first
          (fun fmt -> pprint_expression fmt op_pr)
          ex1
          (fun fmt -> pprint_expression fmt right_pr)
          second
      | _ ->
        asprintf
          "%a %a"
          (fun fmt -> pprint_expression fmt (op_pr + 1))
          ex1
          (fun fmt -> pprint_expression fmt (op_pr + 1))
          ex2
    in
    if n > op_pr then fprintf fmt "(%s)" format_apply else fprintf fmt "%s" format_apply
  | Exp_match (ex, (cs, csl)) ->
    let op_pr1 = get_op_pr ex in
    let match_string =
      asprintf
        "match %a with\n  | %s"
        (fun fmt -> pprint_expression fmt (op_pr1 + 1))
        ex
        (String.concat
           ~sep:"\n  | "
           (List.map (cs :: csl) ~f:(fun cs ->
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
  | Exp_let (rec_fl, (vbind1, vbindl), ex) ->
    let let_string =
      asprintf
        "let %a%s in %a"
        pprint_rec
        rec_fl
        (String.concat
           ~sep:" and "
           (List.map (vbind1 :: vbindl) ~f:(fun vb ->
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
    "function %s"
    (String.concat
       (List.map (cs :: csl) ~f:(fun c ->
          asprintf "\n  | %a" (fun fmt -> pprint_case fmt n) c)))
;;

let pprint_structure_item fmt n =
  let open Structure in
  function
  | Str_eval exp -> fprintf fmt "%a ;;\n" (fun fmt -> pprint_expression fmt n) exp
  | Str_value (rec_flag, (vbind1, vbindl)) ->
    let bindings_str =
      match vbind1 :: vbindl with
      | [] -> ""
      | _ ->
        String.concat
          ~sep:" and\n  "
          (List.map (vbind1 :: vbindl) ~f:(fun vb ->
             asprintf "%a" (fun fmt -> pprint_value_binding fmt n) vb))
    in
    fprintf fmt "let %a%s;;\n\n" pprint_rec rec_flag bindings_str
  | Str_adt (tparam, id, (constr1, constrl)) ->
    let tparam_ident_str =
      match List.length tparam with
      | 0 -> ""
      | 1 -> asprintf "'%s " (List.hd_exn tparam)
      | _ ->
        "('"
        ^ String.concat ~sep:", '" (List.map tparam ~f:(fun param -> asprintf "%s" param))
        ^ ") "
    in
    let var_t_str =
      match constr1 :: constrl with
      | [] -> ""
      | _ ->
        "  | "
        ^ String.concat
            ~sep:"\n  | "
            (List.map (constr1 :: constrl) ~f:(fun (id, typ) ->
               match typ with
               | Some t -> asprintf "%s of %a" id pprint_type t
               | None -> asprintf "%s" id))
    in
    fprintf fmt "type %s%s =\n%s\n;;\n\n" tparam_ident_str id var_t_str
;;

let pprint_program fmt = List.iter ~f:(pprint_structure_item fmt 0)

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer std_formatter res
  | Error _ -> Stdio.print_endline "Syntax error"
;;
