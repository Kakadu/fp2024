(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

let pp_list sep pp_item =
  Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf sep) pp_item
;;

let rec pp_brackets fmt = function
  | [] -> ()
  | _ :: t ->
    fprintf fmt "(";
    pp_brackets fmt t
;;

let pp_const fmt const =
  fprintf
    fmt
    "%s"
    (match const with
     | Integer n -> Integer.Nonnegative_integer.to_string n
     | Bool b ->
       (match b with
        | true -> "True"
        | false -> "False")
     | Unit -> "()")
;;

let%test "pp Integer" =
  asprintf "%a" pp_const (Integer (Integer.Nonnegative_integer.of_int 18)) = "18"
;;

let%test "pp const Bool" = asprintf "%a" pp_const (Bool true) = "True"
let%test "pp const Unit" = asprintf "%a" pp_const Unit = "()"

let rec pp_functype fmt (FuncT (first, second, list)) =
  fprintf fmt "%a -> %a" pp_tp first pp_tp second;
  match list with
  | [] -> ()
  | _ -> fprintf fmt " -> %a" (pp_list " -> " pp_tp) list

and pp_tp fmt = function
  | TUnit -> fprintf fmt "()"
  | TInteger -> fprintf fmt "Integer"
  | TBool -> fprintf fmt "Bool"
  | TreeParam tp -> fprintf fmt "{%a}" pp_tp tp
  | ListParam tp -> fprintf fmt "[%a]" pp_tp tp
  | TupleParams (first, second, list) ->
    fprintf fmt "(%a, %a" pp_tp first pp_tp second;
    (match list with
     | [] -> ()
     | _ -> fprintf fmt ", %a" (pp_list ", " pp_tp) list);
    fprintf fmt ")"
  | FunctionType functype -> pp_functype fmt functype
;;

let%test "pp functype" =
  asprintf "%a" pp_functype (FuncT (TInteger, TBool, [ TBool; TUnit ]))
  = "Integer -> Bool -> Bool -> ()"
;;

let%test "pp tp TUnit" = asprintf "%a" pp_tp TUnit = "()"
let%test "pp tp TInteger" = asprintf "%a" pp_tp TInteger = "Integer"
let%test "pp tp TBool" = asprintf "%a" pp_tp TBool = "Bool"
let%test "pp tp TreeParam" = asprintf "%a" pp_tp (TreeParam TInteger) = "{Integer}"
let%test "pp tp ListParam" = asprintf "%a" pp_tp (ListParam TBool) = "[Bool]"

let%test "pp tp TupleParams" =
  asprintf "%a" pp_tp (TupleParams (TInteger, TBool, [ TBool; TInteger ]))
  = "(Integer, Bool, Bool, Integer)"
;;

let%test "pp tp FunctionType" =
  asprintf "%a" pp_tp (FunctionType (FuncT (TBool, TUnit, []))) = "Bool -> ()"
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

let%test "pp binop And" = asprintf "%a" pp_binop And = "&&"
let%test "pp binop Or" = asprintf "%a" pp_binop Or = "||"
let%test "pp binop Plus" = asprintf "%a" pp_binop Plus = "+"
let%test "pp binop Minus" = asprintf "%a" pp_binop Minus = "-"
let%test "pp binop Divide" = asprintf "%a" pp_binop Divide = "`div`"
let%test "pp binop Mod" = asprintf "%a" pp_binop Mod = "`mod`"
let%test "pp binop Cons" = asprintf "%a" pp_binop Cons = ":"
let%test "pp binop Multiply" = asprintf "%a" pp_binop Multiply = "*"
let%test "pp binop Equality" = asprintf "%a" pp_binop Equality = "=="
let%test "pp binop Inequality" = asprintf "%a" pp_binop Inequality = "/="
let%test "pp binop Greater" = asprintf "%a" pp_binop Greater = ">"
let%test "pp binop EqualityOrLess" = asprintf "%a" pp_binop EqualityOrLess = "<="
let%test "pp binop EqualityOrGreater" = asprintf "%a" pp_binop EqualityOrGreater = ">="
let pp_ident fmt (Ident ident) = fprintf fmt "%s" ident
let%test "pp ident" = asprintf "%a" pp_ident (Ident "x") = "x"

let rec pp_pattern fmt ((list, pat, tp_list) : pattern) =
  fprintf fmt "%a(" pp_brackets tp_list;
  (match list with
   | [] -> ()
   | _ -> fprintf fmt "%a@" (pp_list "@" pp_ident) list);
  fprintf fmt "%a)" pp_pat pat;
  match tp_list with
  | [] -> ()
  | _ -> fprintf fmt " :: %a)" (pp_list ") :: " pp_tp) tp_list

and pp_listpat fmt = function
  | PCons (first, second) -> fprintf fmt "(%a%s%a)" pp_pattern first ":" pp_pattern second
  | PEnum list -> fprintf fmt "[%a]" (pp_list ", " pp_pattern) list

and pp_treepat fmt = function
  | PNul -> fprintf fmt "$"
  | PNode (node, left_son, right_son) ->
    fprintf fmt "(%a; %a; %a)" pp_pattern node pp_pattern left_son pp_pattern right_son

and pp_pconst fmt = function
  | OrdinaryPConst const -> pp_const fmt const
  | NegativePInteger n -> fprintf fmt "(-%s)" (Integer.Nonnegative_integer.to_string n)

and pp_pat fmt = function
  | PWildcard -> fprintf fmt "_"
  | PConst pconst -> pp_pconst fmt pconst
  | PIdentificator ident -> pp_ident fmt ident
  | PList listpat -> pp_listpat fmt listpat
  | PTuple (first, second, list) ->
    fprintf fmt "(%a, %a" pp_pattern first pp_pattern second;
    (match list with
     | [] -> ()
     | _ -> fprintf fmt ", %a" (pp_list ", " pp_pattern) list);
    fprintf fmt ")"
  | PMaybe pattern ->
    (match pattern with
     | Nothing -> fprintf fmt "Nothing"
     | Just pattern -> fprintf fmt "(Just %a)" pp_pattern pattern)
  | PTree treepat -> pp_treepat fmt treepat
;;

let%test "pp listpat PCons" =
  asprintf
    "%a"
    pp_listpat
    (PCons (([], PIdentificator (Ident "x"), []), ([], PIdentificator (Ident "xs"), [])))
  = "((x):(xs))"
;;

let%test "pp listpat PEnum" =
  asprintf
    "%a"
    pp_listpat
    (PEnum
       [ [], PIdentificator (Ident "x"), []
       ; [], PIdentificator (Ident "y"), []
       ; [], PIdentificator (Ident "z"), []
       ])
  = "[(x), (y), (z)]"
;;

let%test "pp treepat PNul" = asprintf "%a" pp_treepat PNul = "$"

let%test "pp treepat PNode" =
  asprintf
    "%a"
    pp_treepat
    (PNode
       ( ([], PIdentificator (Ident "x"), [])
       , ([], PIdentificator (Ident "y"), [])
       , ([], PIdentificator (Ident "z"), []) ))
  = "((x); (y); (z))"
;;

let%test "pp pconst OrdinaryPConst" =
  asprintf "%a" pp_pconst (OrdinaryPConst (Bool true)) = "True"
;;

let%test "pp pconst NegativePInteger" =
  asprintf "%a" pp_pconst (NegativePInteger (Integer.Nonnegative_integer.of_int 18))
  = "(-18)"
;;

let%test "pp pat PWildcard" = asprintf "%a" pp_pat PWildcard = "_"

let%test "pp pat PConst" =
  asprintf "%a" pp_pat (PConst (OrdinaryPConst (Bool true))) = "True"
;;

let%test "pp pat PIdentificator" = asprintf "%a" pp_pat (PIdentificator (Ident "x")) = "x"

let%test "pp pat PList" =
  asprintf
    "%a"
    pp_pat
    (PList
       (PCons (([], PIdentificator (Ident "x"), []), ([], PIdentificator (Ident "xs"), []))))
  = "((x):(xs))"
;;

let%test "pp pat PTuple" =
  asprintf
    "%a"
    pp_pat
    (PTuple
       ( ([], PIdentificator (Ident "a"), [])
       , ([], PIdentificator (Ident "b"), [])
       , [ [], PIdentificator (Ident "c"), []; [], PIdentificator (Ident "d"), [] ] ))
  = "((a), (b), (c), (d))"
;;

let%test "pp pat PMaybe Nothing" = asprintf "%a" pp_pat (PMaybe Nothing) = "Nothing"

let%test "pp pat PMaybe Just" =
  asprintf "%a" pp_pat (PMaybe (Just ([], PIdentificator (Ident "x"), []))) = "(Just (x))"
;;

let%test "pp pat PTree" =
  asprintf
    "%a"
    pp_pat
    (PTree
       (PNode
          ( ([], PIdentificator (Ident "x"), [])
          , ([], PIdentificator (Ident "y"), [])
          , ([], PIdentificator (Ident "z"), []) )))
  = "((x); (y); (z))"
;;

let%test "pp pattern without capture, without type" =
  asprintf "%a" pp_pattern ([], PIdentificator (Ident "x"), []) = "(x)"
;;

let%test "pp pattern with capture, without type" =
  asprintf
    "%a"
    pp_pattern
    ([ Ident "my"; Ident "first"; Ident "variable" ], PIdentificator (Ident "x"), [])
  = "(my@first@variable@x)"
;;

let%test "pp pattern with capture, with type" =
  asprintf
    "%a"
    pp_pattern
    ( [ Ident "my"; Ident "first"; Ident "variable" ]
    , PIdentificator (Ident "x")
    , [ TInteger ] )
  = "((my@first@variable@x) :: Integer)"
;;

let rec pp_comprehension fmt = function
  | Condition expr -> pp_expr fmt expr
  | Generator (pattern, expr) -> fprintf fmt "%a <- %a" pp_pattern pattern pp_expr expr

and pp_ordinarylistbld fmt = function
  | ComprehensionList (expr, comprehension, list) ->
    fprintf fmt "[%a | %a" pp_expr expr pp_comprehension comprehension;
    (match list with
     | [] -> ()
     | _ -> fprintf fmt ", %a" (pp_list ", " pp_comprehension) list);
    fprintf fmt "]"
  | IncomprehensionlList list -> fprintf fmt "[%a]" (pp_list ", " pp_expr) list

and pp_listbld fmt = function
  | LazyList (first, step, last) ->
    fprintf fmt "[%a" pp_expr first;
    (match step with
     | None -> ()
     | Some step -> fprintf fmt ", %a" pp_expr step);
    fprintf fmt "..";
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
    fprintf fmt "%a %a" pp_ident name pp_pattern parameter;
    (match parameters_list with
     | [] -> ()
     | _ -> fprintf fmt " %a" (pp_list " " pp_pattern) parameters_list);
    (match bindingbody with
     | OrdBody _ -> fprintf fmt " = "
     | _ -> fprintf fmt " ");
    pp_bindingbody fmt bindingbody;
    (match binding_list with
     | [] -> ()
     | _ -> fprintf fmt " where %a" (pp_list "; " pp_binding) binding_list)
  | Decl (pattern, tp) -> fprintf fmt "%a :: %a" pp_pattern pattern pp_tp tp

and pp_condition_branch sep fmt (condition, branch) =
  fprintf fmt "%a%s%a" pp_expr condition sep pp_expr branch

and pp_bindingbody fmt = function
  | Guards ((condition, branch), list) ->
    fprintf fmt "|";
    pp_condition_branch " = " fmt (condition, branch);
    (match list with
     | [] -> ()
     | _ -> fprintf fmt "%a" (pp_list " | " (pp_condition_branch " = ")) list)
  | OrdBody expr -> pp_expr fmt expr

and pp_binary_tree_bld fmt = function
  | Nul -> fprintf fmt "$"
  | Node (node, left_son, right_son) ->
    fprintf fmt "(%a; %a; %a)" pp_expr node pp_expr left_son pp_expr right_son

and pp_case_branch fmt (case, branch) =
  pp_pattern fmt case;
  match branch with
  | OrdBody _ -> fprintf fmt " -> %a" pp_bindingbody branch
  | Guards ((condition, branch), list) ->
    fprintf fmt "|";
    pp_condition_branch " -> " fmt (condition, branch);
    (match list with
     | [] -> ()
     | _ -> fprintf fmt "%a" (pp_list " | " (pp_condition_branch " -> ")) list)

and pp_expression fmt = function
  | Const const -> pp_const fmt const
  | Identificator ident -> pp_ident fmt ident
  | TupleBld (first, second, list) ->
    fprintf fmt "(%a, %a" pp_expr first pp_expr second;
    (match list with
     | [] -> ()
     | _ -> fprintf fmt ", %a" (pp_list ", " pp_expr) list);
    fprintf fmt ")"
  | OptionBld expr ->
    (match expr with
     | Nothing -> fprintf fmt "Nothing"
     | Just expr -> fprintf fmt "Just (%a)" pp_expr expr)
  | ListBld listbld -> pp_listbld fmt listbld
  | Binop (first, binop, second) ->
    fprintf fmt "%a%a%a" pp_expr first pp_binop binop pp_expr second
  | Neg expr -> fprintf fmt "(-%a)" pp_expr expr
  | IfThenEsle (condition, case, else_case) ->
    fprintf fmt "if %a then %a else %a" pp_expr condition pp_expr case pp_expr else_case
  | FunctionApply (name, argument, arguments) ->
    fprintf fmt "%a %a %a" pp_expr name pp_expr argument (pp_list " " pp_expr) arguments
  | Lambda (argument, arguments, body) ->
    fprintf
      fmt
      "\\%a %a -> %a"
      pp_pattern
      argument
      (pp_list " " pp_pattern)
      arguments
      pp_expr
      body
  | BinTreeBld binary_tree_bld -> pp_binary_tree_bld fmt binary_tree_bld
  | Case (expr, (case, branch), list) ->
    fprintf fmt "case %a of %a" pp_expr expr pp_case_branch (case, branch);
    (match list with
     | [] -> ()
     | _ -> fprintf fmt "; %a" (pp_list "; " pp_case_branch) list)
  | InnerBindings (binding, binding_list, expr) ->
    fprintf fmt "let %a" pp_binding binding;
    (match binding_list with
     | [] -> ()
     | _ -> fprintf fmt "; %a" (pp_list "; " pp_binding) binding_list);
    fprintf fmt " in %a" pp_expr expr

and pp_expr fmt ((expression, tp_list) : expr) =
  fprintf fmt "%a(%a)" pp_brackets tp_list pp_expression expression;
  match tp_list with
  | [] -> ()
  | _ -> fprintf fmt " :: %a)" (pp_list ") :: " pp_tp) tp_list
;;
