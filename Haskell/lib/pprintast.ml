(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

let pp_list sep pp_item = pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf sep) pp_item

let pp_brackets fmt list =
  fprintf fmt "%s" (String.make (max 0 (List.length list - 1)) '(')
;;

let pp_const fmt const =
  fprintf
    fmt
    "%s"
    (match const with
     | Int n -> Int.to_string n
     | Bool b -> if b then "True" else "False"
     | Unit -> "()")
;;

let rec pp_functype fmt (FuncT (first, second, list)) =
  fprintf fmt "%a" (pp_list " -> " pp_part_of_functype) (first :: second :: list)

and pp_part_of_functype fmt tp =
  fprintf
    fmt
    (match tp with
     | FunctionType _ -> "(%a)"
     | _ -> "%a")
    pp_tp
    tp

and pp_tp fmt = function
  | TUnit -> fprintf fmt "()"
  | TInt -> fprintf fmt "Int"
  | TBool -> fprintf fmt "Bool"
  | MaybeParam tp ->
    fprintf
      fmt
      (match tp with
       | MaybeParam _ | FunctionType _ -> "Maybe (%a)"
       | _ -> "Maybe %a")
      pp_tp
      tp
  | TreeParam tp -> fprintf fmt "{%a}" pp_tp tp
  | ListParam tp -> fprintf fmt "[%a]" pp_tp tp
  | TupleParams (first, second, list) ->
    fprintf fmt "(%a)" (pp_list ", " pp_tp) (first :: second :: list)
  | FunctionType functype -> pp_functype fmt functype
;;

let pp_binop fmt = function
  | And -> fprintf fmt "&&"
  | Or -> fprintf fmt "||"
  | Plus -> fprintf fmt "+"
  | Minus -> fprintf fmt "-"
  | Divide -> fprintf fmt "`div`"
  | Mod -> fprintf fmt "`mod`"
  | Cons -> fprintf fmt ":"
  | Multiply -> fprintf fmt "*"
  | Equality -> fprintf fmt "=="
  | Pow -> fprintf fmt "^"
  | Inequality -> fprintf fmt "/="
  | Less -> fprintf fmt "<"
  | Greater -> fprintf fmt ">"
  | EqualityOrLess -> fprintf fmt "<="
  | EqualityOrGreater -> fprintf fmt ">="
;;

let pp_ident fmt (Ident ident) = fprintf fmt "%s" ident

type paransed_cases =
  | Tp_and_some_constrs
  | Tp_only
  | No_cases

let rec pp_pattern fmt ((list, pat, tp_list) : pattern) =
  fprintf fmt "%a" pp_brackets tp_list;
  (match list with
   | [] -> ()
   | _ -> fprintf fmt "%a@" (pp_list "@" pp_ident) list);
  fprintf
    fmt
    (match pat with
     | PMaybe (Just _) | PList (PCons _) | PConst (NegativePInt _) ->
       (match list with
        | [] -> "%a"
        | _ -> "(%a)")
     | _ -> "%a")
    pp_pat
    pat;
  match tp_list with
  | [] -> ()
  | _ -> fprintf fmt " :: %a" (pp_list ") :: " pp_tp) (List.rev tp_list)

and pp_pattern_sometimes_parensed cases fmt pattern =
  fprintf
    fmt
    (match cases, pattern with
     | ( Tp_and_some_constrs
       , ([], (PMaybe (Just _) | PList (PCons _) | PConst (NegativePInt _)), _) )
     | (Tp_and_some_constrs | Tp_only), (_, _, _ :: _) -> "(%a)"
     | _ -> "%a")
    pp_pattern
    pattern

and pp_pat fmt = function
  | PWildcard -> fprintf fmt "_"
  | PConst (OrdinaryPConst const) -> pp_const fmt const
  | PConst (NegativePInt n) -> fprintf fmt "-%s" (Int.to_string n)
  | PIdentificator ident -> pp_ident fmt ident
  | PTuple (first, second, list) ->
    fprintf fmt "(%a)" (pp_list ", " pp_pattern) (first :: second :: list)
  | PMaybe Nothing -> fprintf fmt "Nothing"
  | PMaybe (Just pattern) ->
    fprintf fmt "Just %a" (pp_pattern_sometimes_parensed Tp_and_some_constrs) pattern
  | PTree PNul -> fprintf fmt "$"
  | PTree (PNode (node, left_son, right_son)) ->
    fprintf fmt "(%a; %a; %a)" pp_pattern node pp_pattern left_son pp_pattern right_son
  | PList (PEnum list) -> fprintf fmt "[%a]" (pp_list ", " pp_pattern) list
  | PList (PCons (first, second)) ->
    fprintf
      fmt
      (match first with
       | [], PList (PCons _), [] -> "(%a) : %a"
       | _ -> "%a : %a")
      (pp_pattern_sometimes_parensed Tp_only)
      first
      (pp_pattern_sometimes_parensed Tp_only)
      second
;;

let get_prior = function
  | Binop (_, Or, _) -> 2
  | Binop (_, And, _) -> 3
  | Binop
      (_, (Equality | Inequality | Less | EqualityOrLess | Greater | EqualityOrGreater), _)
    -> 4
  | Binop (_, Cons, _) -> 5
  | Neg _ | Binop (_, (Plus | Minus), _) -> 6
  | Binop (_, (Multiply | Divide | Mod), _) -> 7
  | Binop (_, Pow, _) -> 8
  | _ -> 10
;;

let rec pp_comprehension fmt = function
  | Condition expr -> pp_expr fmt expr
  | Generator (pattern, expr) -> fprintf fmt "%a <- %a" pp_pattern pattern pp_expr expr

and pp_ordinarylistbld fmt = function
  | ComprehensionList (expr, comprehension, list) ->
    fprintf
      fmt
      "[%a | %a]"
      pp_expr
      expr
      (pp_list ", " pp_comprehension)
      (comprehension :: list)
  | IncomprehensionlList list -> fprintf fmt "[%a]" (pp_list ", " pp_expr) list

and pp_listbld fmt = function
  | LazyList (first, step, last) ->
    fprintf fmt "[%a" pp_expr first;
    (match step with
     | None -> ()
     | Some step -> fprintf fmt ", %a" pp_expr step);
    fprintf fmt " .. ";
    (match last with
     | None -> ()
     | Some last -> pp_expr fmt last);
    fprintf fmt "]"
  | OrdList ordinarylistbld -> pp_ordinarylistbld fmt ordinarylistbld

and pp_binding fmt = function
  | VarsDef (pattern, bindingbody, list) ->
    pp_pattern fmt pattern;
    (match bindingbody with
     | Guards _ -> ()
     | OrdBody _ -> fprintf fmt " = ");
    pp_bindingbody fmt bindingbody;
    (match list with
     | [] -> ()
     | _ -> fprintf fmt " where %a" (pp_list "; " pp_binding) list)
  | FunDef (name, parameter, parameters_list, bindingbody, binding_list) ->
    fprintf
      fmt
      "%a %a%s"
      pp_ident
      name
      (pp_list " " (pp_pattern_sometimes_parensed Tp_and_some_constrs))
      (parameter :: parameters_list)
      (match bindingbody with
       | OrdBody _ -> " = "
       | _ -> "");
    pp_bindingbody fmt bindingbody;
    (match binding_list with
     | [] -> ()
     | _ -> fprintf fmt " where %a" (pp_list "; " pp_binding) binding_list)
  | Decl (ident, tp) -> fprintf fmt "%a :: %a" pp_ident ident pp_tp tp

and pp_condition_branch sep fmt (condition, branch) =
  fprintf fmt "%a%s%a" pp_expr_parenced_tp condition sep pp_expr branch

and pp_bindingbody fmt = function
  | Guards (cb, list) ->
    fprintf fmt " | %a" (pp_list " | " (pp_condition_branch " = ")) (cb :: list)
  | OrdBody expr -> pp_expr fmt expr

and pp_binary_tree_bld fmt = function
  | Nul -> fprintf fmt "$"
  | Node (node, left_son, right_son) ->
    fprintf fmt "(%a; %a; %a)" pp_expr node pp_expr left_son pp_expr right_son

and pp_case_branch fmt (case, branch) =
  pp_pattern_sometimes_parensed Tp_only fmt case;
  match branch with
  | OrdBody _ -> fprintf fmt " -> %a" pp_bindingbody branch
  | Guards (cb, list) ->
    fprintf fmt "| %a" (pp_list " | " (pp_condition_branch " -> ")) (cb :: list)

and pp_expression fmt expression =
  match expression with
  | Const const -> pp_const fmt const
  | Identificator ident -> pp_ident fmt ident
  | TupleBld (first, second, list) ->
    fprintf fmt "(%a)" (pp_list ", " pp_expr) (first :: second :: list)
  | ENothing -> fprintf fmt "Nothing"
  | EJust -> fprintf fmt "Just"
  | ListBld listbld -> pp_listbld fmt listbld
  | Binop (((expression1, tp1) as first), binop, ((expresion2, tp2) as second)) ->
    fprintf
      fmt
      (match tp1, get_prior expression1 <= get_prior expression, expression1 with
       | [], true, _ | [], _, (IfThenEsle _ | Lambda _ | Case _) -> "(%a) %a "
       | _ -> "%a %a ")
      pp_expr_parenced_tp
      first
      pp_binop
      binop;
    fprintf
      fmt
      (match
         tp2, Int.compare (get_prior expresion2) (get_prior expression), expresion2
       with
       | [], k, _ when k < 0 -> "(%a)"
       | [], 0, Neg _ -> "(%a)"
       | _ -> "%a")
      pp_expr_parenced_tp
      second
  | Neg expr ->
    fprintf
      fmt
      (match expr with
       | expression1, [] when get_prior expression1 <= get_prior expression -> "- (%a)"
       | _ -> "- %a")
      pp_expr_parenced_tp
      expr
  | IfThenEsle (condition, case, else_case) ->
    fprintf fmt "if %a then %a else %a" pp_expr condition pp_expr case pp_expr else_case
  | FunctionApply (fn, argument, arguments) ->
    fprintf
      fmt
      (match fn with
       | Const _, _
       | Identificator _, _
       | TupleBld _, _
       | ENothing, _
       | EJust, _
       | ListBld _, _
       | BinTreeBld _, _
       | _, _ :: _ -> "%a %a"
       | _ -> "(%a) %a")
      pp_expr_parenced_tp
      fn
      (pp_list " " pp_expr_apl_arg)
      (argument :: arguments)
  | Lambda (argument, arguments, body) ->
    fprintf
      fmt
      "\\ %a -> %a"
      (pp_list " " (pp_pattern_sometimes_parensed Tp_and_some_constrs))
      (argument :: arguments)
      pp_expr
      body
  | BinTreeBld binary_tree_bld -> pp_binary_tree_bld fmt binary_tree_bld
  | Case (expr, cb, list) ->
    fprintf fmt "case %a of %a" pp_expr expr (pp_list "; " pp_case_branch) (cb :: list)
  | InnerBindings (binding, binding_list, expr) ->
    fprintf fmt "let %a" (pp_list "; " pp_binding) (binding :: binding_list);
    fprintf fmt " in %a" pp_expr expr

and pp_expr_parenced_tp fmt ((_, tp) as expr) =
  fprintf
    fmt
    (match tp with
     | [] -> "%a"
     | _ -> "(%a)")
    pp_expr
    expr

and pp_expr_apl_arg fmt expr =
  fprintf
    fmt
    (match expr with
     | (Binop _ | Neg _ | FunctionApply _), [] -> "(%a)"
     | _ -> "%a")
    pp_expr_parenced_tp
    expr

and pp_expr fmt ((expression, tp_list) : expr) =
  fprintf
    fmt
    (match expression, tp_list with
     | (IfThenEsle _ | Lambda _ | Case _ | InnerBindings _), [] -> "(%a%a)"
     | (IfThenEsle _ | Lambda _ | Case _ | InnerBindings _), _ -> "(%a(%a))"
     | _ -> "%a%a")
    pp_brackets
    tp_list
    pp_expression
    expression;
  match tp_list with
  | [] -> ()
  | _ -> fprintf fmt " :: %a" (pp_list ") :: " pp_tp) (List.rev tp_list)
;;

let i_const x = Const (Int x), []
