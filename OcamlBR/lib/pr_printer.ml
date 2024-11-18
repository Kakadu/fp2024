
(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast
open Format

let pp_const ppf = function
  | Int i -> fprintf ppf "%d" i
  | String s -> fprintf ppf "\"%s\"" s
  | Bool b -> fprintf ppf "%b" b
  | Unit -> fprintf ppf "()"
;;

let pp_bin_op ppf = function
  | Add -> fprintf ppf "+"
  | Mult -> fprintf ppf "*"
  | Sub -> fprintf ppf "-"
  | Div -> fprintf ppf "/"
  | Gt -> fprintf ppf ">"
  | Lt -> fprintf ppf "<"
  | Eq -> fprintf ppf "="
  | Neq -> fprintf ppf "<>"
  | Gte -> fprintf ppf ">="
  | Lte -> fprintf ppf "<="
  | And -> fprintf ppf "&&"
  | Or -> fprintf ppf "||"
;;

let pp_un_op ppf = function
  | Negative -> fprintf ppf "-"
  | Positive -> fprintf ppf "+"
  | Not -> fprintf ppf "not"
;;

let pp_rec_flag ppf = function
  | Recursive -> fprintf ppf "rec"
  | Non_recursive -> ()
;;

let pp_pattern ppf = function
  | PVar id -> fprintf ppf "%s" id
  | PConst c -> pp_const ppf c
  | PAny -> fprintf ppf "_"
;;

let rec pp_expr ppf = function
  | Econst c -> pp_const ppf c
  | Evar id -> fprintf ppf "%s" id
  | Eif_then_else (e1, e2, None) ->
    fprintf ppf "if %a then %a" pp_expr e1 pp_expr e2
  | Eif_then_else (e1, e2, Some e3) ->
    fprintf ppf "if %a then %a else %a" pp_expr e1 pp_expr e2 pp_expr e3
  | Ematch (exp, case_list) ->
    let case_list_str =
      String.concat
        ~sep:" "
        (List.map
            ~f:(fun c -> asprintf "| %a -> %a" pp_pattern c.left pp_expr c.right)
            case_list)
    in
    fprintf ppf "match %a with %s" pp_expr exp case_list_str
  | Eoption (Some e) -> fprintf ppf "Some %a" pp_expr e
  | Eoption None -> fprintf ppf "None"
  | Etuple (e1, e2, es) ->
    fprintf ppf "(%a, %a%a)"
      pp_expr e1
      pp_expr e2
      (fun ppf -> List.iter ~f:(fprintf ppf ", %a" pp_expr)) es
  | Elist es ->
    fprintf ppf "[%a]"
      (fun ppf -> List.iteri ~f:(fun i e -> if i > 0 then fprintf ppf "; %a" pp_expr e else pp_expr ppf e)) es
  | Efun (patterns, e) ->
    fprintf ppf "fun %a -> %a"
      (fun ppf -> List.iter ~f:(fprintf ppf "%a " pp_pattern)) patterns
      pp_expr e
  | Ebin_op (op, e1, e2) ->
    fprintf ppf "(%a %a %a)" pp_expr e1 pp_bin_op op pp_expr e2
  | Eun_op (op, e) -> fprintf ppf "%a%a" pp_un_op op pp_expr e
  | Elet (rec_flag, vb, vb_l, e) ->
    fprintf ppf "let %a %a in %a"
      pp_rec_flag rec_flag
      (fun ppf () ->
         fprintf ppf "%a" pp_value_binding vb;
         List.iter vb_l ~f:(fun vb' ->
           fprintf ppf " and %a" pp_value_binding vb')) ()
      pp_expr e
  | Efun_application (e1, e2) ->
    fprintf ppf "%a %a" pp_expr e1 pp_expr e2

and pp_value_binding ppf = function
  | Evalue_binding (id, e) -> fprintf ppf "%s = %a" id pp_expr e
;;

let pp_structure_item ppf (item : structure_item) =
  match item with
  | SEval e -> fprintf ppf "%a;" pp_expr e
  | SValue (rec_flag, vb, vb_l, e) ->
    fprintf ppf "let %a %a in %a"
      pp_rec_flag rec_flag
      (fun ppf () ->
         fprintf ppf "%a" pp_value_binding vb;
         List.iter vb_l ~f:(fun vb' ->
           fprintf ppf " and %a" pp_value_binding vb')) ()
      pp_expr e
;;
(*
let prpr_structure fmt structure =
  List.iter ~f:(fun item -> fprintf fmt "%a@." pp_structure_item item) structure
;; *)

let pp_escape_sequence ppf () = fprintf ppf "\n"

let prpr_structure ppf =
  fprintf ppf "%a" (pp_print_list ~pp_sep:pp_escape_sequence pp_structure_item)
;;
















(*
(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast

let pp_const = function
  | Int i -> Stdlib.string_of_int i
  | String s -> "\"" ^ s ^ "\""
  | Bool b -> Stdlib.string_of_bool b
  | Unit -> "()"
;;

let pp_bin_op = function
  | Add -> "+"
  | Mult -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Gt -> ">"
  | Lt -> "<"
  | Eq -> "="
  | Neq -> "<>"
  | Gte -> ">="
  | Lte -> "<="
  | And -> "&&"
  | Or -> "||"
;;

let pp_un_op = function
  | Negative -> "-"
  | Positive -> "+"
  | Not -> "not"
;;

let pp_rec_flag = function
  | Recursive -> "rec"
  | Non_recursive -> ""
;;

let pp_pattern = function
  | PVar id -> id
  | PConst c -> pp_const c
  | PAny -> "_"
;;

let rec pp_expr = function
  | Econst c -> pp_const c
  | Evar id -> id
  | Eif_then_else (e1, e2, None) -> "if " ^ pp_expr e1 ^ " then " ^ pp_expr e2
  | Eif_then_else (e1, e2, Some e3) ->
    "if " ^ pp_expr e1 ^ " then " ^ pp_expr e2 ^ " else " ^ pp_expr e3
  | Eoption (Some e) -> "Some " ^ pp_expr e
  | Eoption None -> "None "
  | Etuple es -> "(" ^ String.concat ~sep:", " (List.map ~f:pp_expr es) ^ ")"
  | Elist es -> "[" ^ String.concat ~sep:"; " (List.map ~f:pp_expr es) ^ "]"
  | Efun (patterns, e) ->
    "fun " ^ String.concat ~sep:" " (List.map ~f:pp_pattern patterns) ^ " -> " ^ pp_expr e
  | Ebin_op (op, e1, e2) -> "(" ^ pp_expr e1 ^ " " ^ pp_bin_op op ^ " " ^ pp_expr e2 ^ ")"
  | Eun_op (op, e) -> pp_un_op op ^ pp_expr e
  | Elet (rec_flag, id, e1, e2) ->
    "let " ^ pp_rec_flag rec_flag ^ " " ^ id ^ " = " ^ pp_expr e1 ^ " in " ^ pp_expr e2
  | Efun_application (e1, e2) -> pp_expr e1 ^ " " ^ pp_expr e2
;;

let pp_structure_item (item : structure_item) : string =
  match item with
  | SEval e -> pp_expr e ^ ";"
  | SValue (rec_flag, id, e1, e2) ->
    "let " ^ pp_rec_flag rec_flag ^ " " ^ id ^ " = " ^ pp_expr e1 ^ " in " ^ pp_expr e2
;;

let prpr_structure fmt structure =
  List.iter
    ~f:(fun item -> Stdlib.Format.fprintf fmt "%s@." (pp_structure_item item))
    structure
;;

*)























(*



(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast

let pp_const fmt = function
  | Int i -> fprintf fmt "%d" i
  | String s -> fprintf fmt "\"%s\"" s
  | Bool b -> fprintf ppf "%b" b
  | Unit -> fprintf fmt "()"
;;

let pp_bin_op ppf = function
  | Add -> fprintf ppf "+"
  | Mult -> fprintf ppf "*"
  | Sub -> fprintf ppf "-"
  | Div -> fprintf ppf "/"
  | Gt -> fprintf ppf ">"
  | Lt -> fprintf ppf "<"
  | Eq -> fprintf ppf "="
  | Neq -> fprintf ppf "<>"
  | Gte -> fprintf ppf ">="
  | Lte -> fprintf ppf "<="
  | And -> fprintf ppf "&&"
  | Or -> fprintf ppf "||"

  let pp_un_op ppf = function
  | Negative -> fprintf ppf "-"
  | Positive -> fprintf ppf "+"
  | Not -> fprintf ppf "not"

  let pp_rec_flag ppf = function
  | Recursive -> fprintf ppf "rec"
  | Non_recursive -> ()

  let pp_pattern ppf = function
  | PVar id -> fprintf ppf "%s" id
  | PConst c -> pp_const ppf c
  | PAny -> fprintf ppf "_"

let rec pp_expr ppf = function
  | Econst c -> pp_const c
  | Evar id -> id
  | Eif_then_else (e1, e2, None) -> "if " ^ pp_expr e1 ^ " then " ^ pp_expr e2
  | Eif_then_else (e1, e2, Some e3) ->
    "if " ^ pp_expr e1 ^ " then " ^ pp_expr e2 ^ " else " ^ pp_expr e3
  | Eoption (Some e) -> "Some " ^ pp_expr e
  | Eoption None -> "None "
  | Etuple (e1, e2, es) ->
    "("
    ^ pp_expr e1
    ^ ", "
    ^ pp_expr e2
    ^ ", "
    ^ String.concat ~sep:", " (List.map ~f:pp_expr es)
    ^ ")"
  | Elist es -> "[" ^ String.concat ~sep:"; " (List.map ~f:pp_expr es) ^ "]"
  | Efun (patterns, e) ->
    "fun " ^ String.concat ~sep:" " (List.map ~f:pp_pattern patterns) ^ " -> " ^ pp_expr e
  | Ebin_op (op, e1, e2) -> "(" ^ pp_expr e1 ^ " " ^ pp_bin_op op ^ " " ^ pp_expr e2 ^ ")"
  | Eun_op (op, e) -> pp_un_op op ^ pp_expr e
  | Elet (rec_flag, vb, vb_l, e) ->
    "let "
    ^ pp_rec_flag rec_flag
    ^ " "
    ^ pp_value_binding vb
    ^ String.concat ~sep:" and " (List.map ~f:pp_value_binding vb_l)
    ^ " in "
    ^ pp_expr e
  | Efun_application (e1, e2) -> pp_expr e1 ^ " " ^ pp_expr e2

and pp_value_binding = function
  | Evalue_binding (id, e) -> id ^ " = " ^ pp_expr e
;;

let pp_structure_item (item : structure_item) : string =
  match item with
  | SEval e -> pp_expr e ^ ";"
  | SValue (rec_flag, vb, vb_l, e2) ->
    "let "
    ^ pp_rec_flag rec_flag
    ^ " "
    ^ pp_value_binding vb
    ^ String.concat ~sep:" and " (List.map ~f:pp_value_binding vb_l)
    ^ " in "
    ^ pp_expr e2
;;

let prpr_structure fmt structure =
  List.iter
    ~f:(fun item -> Stdlib.Format.fprintf fmt "%s@." (pp_structure_item item))
    structure
;;

*)


(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 **)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


(*

open Ast
open Format

let is_operator = function
  | "*" | "/" | "+" | "-" | ">=" | "<=" | "<>" | "=" | ">" | "<" | "&&" | "||" -> true
  | _ -> false
;;

(* Operators that may require parentheses *)
let is_operator1 = function
  | "+" | "-" -> true
  | _ -> false
;;

let let_flag_str = function
  | Recursive -> "let rec"
  | Nonrecursive -> "let"
;;

let pp_escape_sequence ppf () = fprintf ppf "\n"
let pp_space ppf () = fprintf ppf " "
let pp_comma ppf () = fprintf ppf ", "
let pp_ident ppf id = fprintf ppf "%s" id

let pp_constant ppf = function
  | Const_integer n -> fprintf ppf "%d" n
  | Const_char c -> fprintf ppf "'%c'" c
  | Const_string s -> fprintf ppf "%S" s
;;

let rec pp_pattern ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var id -> pp_ident ppf id
  | Pat_constant const -> pp_constant ppf const
  | Pat_tuple pat_list ->
    fprintf ppf "(%a)" (pp_print_list ~pp_sep:pp_comma pp_pattern) pat_list
  | Pat_construct (_, None) -> fprintf ppf "[]"
  | Pat_construct (_, Some pat) ->
    (match pat with
     | Pat_tuple [ head; tail ] ->
       fprintf ppf "[%a" pp_pattern head;
       let rec pp_tail = function
         | Pat_construct (_, None) -> fprintf ppf "]"
         | Pat_construct (_, Some pat_tail) ->
           (match pat_tail with
            | Pat_tuple [ next_head; next_tail ] ->
              fprintf ppf "; %a" pp_pattern next_head;
              pp_tail next_tail
            | _ -> ())
         | _ -> ()
       in
       pp_tail tail
     | _ -> ())
;;

let rec pp_expression ppf = function
  | Exp_ident id -> pp_ident ppf id
  | Exp_constant const -> pp_constant ppf const
  | Exp_let (rec_flag, value_binding_list, exp) ->
    let bindings_str =
      String.concat
        " and "
        (List.map
           (fun b -> asprintf "%a = %a" pp_pattern b.pat pp_expression b.exp)
           value_binding_list)
    in
    fprintf ppf "%s %s in %a" (let_flag_str rec_flag) bindings_str pp_expression exp
  | Exp_fun (pat_list, exp) ->
    fprintf
      ppf
      "fun %a -> %a"
      (pp_print_list ~pp_sep:pp_space pp_pattern)
      pat_list
      pp_expression
      exp
  | Exp_apply (exp, exp_list) ->
    let expression = asprintf "%a" pp_expression exp in
    let handle_exp_list = function
      | [] -> fprintf ppf "%s" expression
      | [ expr ] -> fprintf ppf "%s %a" expression pp_expression expr
      | _ ->
        let first_exp = List.hd exp_list in
        let rest_exp = List.tl exp_list in
        let needs_parens = function
          | Exp_apply (Exp_ident op, _) when is_operator1 op -> true
          | _ -> false
        in
        if is_operator expression
        then fprintf ppf "%a %s" pp_expression first_exp expression
        else fprintf ppf "%s %a" expression pp_expression first_exp;
        List.iter
          (fun arg ->
            if needs_parens arg
            then fprintf ppf " (%a)" pp_expression arg
            else fprintf ppf " %a" pp_expression arg)
          rest_exp
    in
    handle_exp_list exp_list
  | Exp_match (exp, case_list) ->
    let case_list_str =
      String.concat
        " "
        (List.map
           (fun c -> asprintf "| %a -> %a" pp_pattern c.left pp_expression c.right)
           case_list)
    in
    fprintf ppf "match %a with %s" pp_expression exp case_list_str
  | Exp_tuple exp_list ->
    fprintf ppf "(%a)" (pp_print_list ~pp_sep:pp_comma pp_expression) exp_list
  | Exp_construct (_, None) -> fprintf ppf "[]"
  | Exp_construct (_, Some exp) ->
    (match exp with
     | Exp_tuple [ head; tail ] ->
       fprintf ppf "[%a" pp_expression head;
       let rec print_tail = function
         | Exp_construct (_, None) -> fprintf ppf "]"
         | Exp_construct (_, Some exp_tail) ->
           (match exp_tail with
            | Exp_tuple [ next_head; next_tail ] ->
              fprintf ppf "; %a" pp_expression next_head;
              print_tail next_tail
            | _ -> ())
         | _ -> ()
       in
       print_tail tail
     | _ -> ())
  | Exp_ifthenelse (exp1, exp2, None) ->
    fprintf ppf "if %a then %a" pp_expression exp1 pp_expression exp2
  | Exp_ifthenelse (exp1, exp2, Some exp3) ->
    fprintf
      ppf
      "if %a then %a else %a"
      pp_expression
      exp1
      pp_expression
      exp2
      pp_expression
      exp3
  | Exp_sequence (exp1, exp2) ->
    fprintf ppf "%a; %a" pp_expression exp1 pp_expression exp2
;;

let pp_structure_item ppf = function
  | Struct_eval exp -> fprintf ppf "%a;;" pp_expression exp
  | Struct_value (rec_flag, value_binding_list) ->
    let bindings_str =
      String.concat
        " and "
        (List.map
           (fun value -> asprintf "%a = %a" pp_pattern value.pat pp_expression value.exp)
           value_binding_list)
    in
    fprintf ppf "%s %s;;" (let_flag_str rec_flag) bindings_str
;;

let pp_structure ppf =
  fprintf ppf "%a" (pp_print_list ~pp_sep:pp_escape_sequence pp_structure_item)
;;
*)