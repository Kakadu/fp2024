(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility
open Format

let pp_identifier ff = fprintf ff "%s"

let pp_literal ff = function
  | IntLiteral i -> fprintf ff "%d" i
  | BoolLiteral b -> fprintf ff "%b" b
  | UnitLiteral -> fprintf ff "()"
;;

let pp_unary_operator ff = function
  | Negate -> fprintf ff "~-"
  | Positive -> fprintf ff "~+"
;;

let pp_binary_operator ff = function
  | Add -> fprintf ff "+"
  | Subtract -> fprintf ff "-"
  | Multiply -> fprintf ff "*"
  | Division -> fprintf ff "/"
  | Equals -> fprintf ff "="
  | Unequals -> fprintf ff "<>"
  | Gt -> fprintf ff ">"
  | Lt -> fprintf ff "<"
  | Gte -> fprintf ff ">="
  | Lte -> fprintf ff "<="
  | And -> fprintf ff "&&"
  | Or -> fprintf ff "||"
;;

let pp_recursive_type ff = function
  | Recursive -> fprintf ff "rec"
  | Nonrecursive -> fprintf ff ""
;;

let pp_tuple_sep ff () = fprintf ff ", "
let pp_expr_block_sep ff () = fprintf ff "; "
let pp_list_sep ff () = fprintf ff "; "
let pp_definition_sep ff () = fprintf ff " and "
let pp_struct_item_sep ff () = fprintf ff "\n"

let rec pp_pattern ff = function
  | PVar id -> pp_identifier ff id
  | PTuple l -> fprintf ff "(%a)" (pp_print_list ~pp_sep:pp_tuple_sep pp_pattern) l
  | PUnit -> fprintf ff "()"
;;

let rec pp_expression ff = function
  | Const l -> pp_literal ff l
  | Variable id -> pp_identifier ff id
  | Unary (op, ex) -> fprintf ff "(%a%a)" pp_unary_operator op pp_expression ex
  | Binary (left, op, right) ->
    fprintf ff "(%a %a %a)" pp_expression left pp_binary_operator op pp_expression right
  | Tuple l -> fprintf ff "(%a)" (pp_print_list ~pp_sep:pp_tuple_sep pp_expression) l
  | ExpressionsList l ->
    fprintf ff "[%a]" (pp_print_list ~pp_sep:pp_list_sep pp_expression) l
  | Construct (name, None) -> fprintf ff "(%a)" pp_identifier name
  | Construct (name, Some ex) -> fprintf ff "(%a %a)" pp_identifier name pp_expression ex
  | ExpressionBlock l ->
    fprintf ff "(%a)" (pp_print_list ~pp_sep:pp_expr_block_sep pp_expression) l
  | Apply (ex, l) ->
    fprintf
      ff
      "((%a) %a)"
      pp_expression
      ex
      (pp_print_list ~pp_sep:pp_print_space pp_expression)
      l
  | If (ex, then_ex, else_branch) ->
    (match else_branch with
     | Some else_ex ->
       fprintf
         ff
         "(if %a then %a else %a)"
         pp_expression
         ex
         pp_expression
         then_ex
         pp_expression
         else_ex
     | None -> fprintf ff "(if %a then %a)" pp_expression ex pp_expression then_ex)
  | Lambda (pl, ex) ->
    fprintf
      ff
      "(fun %a -> %a)"
      (pp_print_list ~pp_sep:pp_print_space pp_pattern)
      pl
      pp_expression
      ex
  | Define (defines, ex) ->
    fprintf ff "(let %a in %a)" pp_definition defines pp_expression ex

and pp_value_binding ff (p, ex) = fprintf ff "%a = %a" pp_pattern p pp_expression ex

and pp_definition ff (r_type, vbl) =
  fprintf
    ff
    "%a %a"
    pp_recursive_type
    r_type
    (pp_print_list ~pp_sep:pp_definition_sep pp_value_binding)
    vbl
;;

let pp_struct_item ff = function
  | DefineItem defines -> fprintf ff "let %a;;" pp_definition defines
  | EvalItem ex -> fprintf ff "%a;;" pp_expression ex
;;

let pp_program ff p = pp_print_list ~pp_sep:pp_struct_item_sep pp_struct_item ff p

let pp_parse_result ff pp = function
  | ParseFail -> fprintf ff "Parse process failed"
  | ParseError (msg, state) ->
    fprintf ff "ParseError(line=%d pos=%d): %s" state.line state.inline msg
  | ParseSuccess (r, _) -> pp ff r
;;
