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
    fprintf fmt "%s" "(";
    pp_brackets fmt t
;;

let pp_const fmt = function
  | Integer n -> fprintf fmt "%s" (Integer.Nonnegative_integer.to_string n)
  | Bool b ->
    (match b with
     | true -> fprintf fmt "%s" "True"
     | false -> fprintf fmt "%s" "False")
  | Unit -> fprintf fmt "%s" "()"
;;

let%test "pp Integer" =
  asprintf "%a" pp_const (Integer (Integer.Nonnegative_integer.of_int 18)) = "18"
;;

let%test "pp const Bool" = asprintf "%a" pp_const (Bool true) = "True"
let%test "pp const Unit" = asprintf "%a" pp_const Unit = "()"

let rec pp_functype fmt (FuncT (first, second, list)) =
  pp_tp fmt first;
  fprintf fmt "%s" " -> ";
  pp_tp fmt second;
  match list with
  | [] -> ()
  | _ ->
    fprintf fmt " -> ";
    fprintf fmt "%a" (pp_list " -> " pp_tp) list

and wrap_into fmt left right ppf pp_item =
  fprintf fmt "%s" left;
  ppf fmt pp_item;
  fprintf fmt "%s" right

and pp_tp fmt = function
  | TUnit -> fprintf fmt "%s" "()"
  | TInteger -> fprintf fmt "%s" "Integer"
  | TBool -> fprintf fmt "%s" "Bool"
  | TreeParam tp -> wrap_into fmt "{" "}" pp_tp tp
  | ListParam tp -> wrap_into fmt "[" "]" pp_tp tp
  | TupleParams (first, second, list) ->
    fprintf fmt "(";
    pp_tp fmt first;
    fprintf fmt ", ";
    pp_tp fmt second;
    (match list with
     | [] -> ()
     | _ ->
       fprintf fmt "%s" ", ";
       fprintf fmt "%a" (pp_list ", " pp_tp) list);
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
  | And -> fprintf fmt "%s" "&&"
  | Or -> fprintf fmt "%s" "||"
  | Plus -> fprintf fmt "%s" "+"
  | Minus -> fprintf fmt "%s" "-"
  | Divide -> fprintf fmt "%s" "`div`"
  | Mod -> fprintf fmt "%s" "`mod`"
  | Cons -> fprintf fmt "%s" ":"
  | Multiply -> fprintf fmt "%s" "*"
  | Equality -> fprintf fmt "%s" "=="
  | Pow -> fprintf fmt "%s" "^"
  | Inequality -> fprintf fmt "%s" "/="
  | Less -> fprintf fmt "%s" "<"
  | Greater -> fprintf fmt "%s" ">"
  | EqualityOrLess -> fprintf fmt "%s" "<="
  | EqualityOrGreater -> fprintf fmt "%s" ">="
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
  pp_brackets fmt tp_list;
  fprintf fmt "%s" "(";
  (match list with
   | [] -> ()
   | _ ->
     fprintf fmt "%a" (pp_list "@" pp_ident) list;
     fprintf fmt "%s" "@");
  pp_pat fmt pat;
  fprintf fmt "%s" ")";
  match tp_list with
  | [] -> ()
  | _ ->
    fprintf fmt "%s" " :: ";
    fprintf fmt "%a" (pp_list ") :: " pp_tp) tp_list;
    fprintf fmt "%s" ")"

and pp_listpat fmt = function
  | PCons (first, second) ->
    fprintf fmt "%s" "(";
    pp_pattern fmt first;
    fprintf fmt "%s" ":";
    pp_pattern fmt second;
    fprintf fmt "%s" ")"
  | PEnum list -> fprintf fmt "[%a]" (pp_list ", " pp_pattern) list

and pp_treepat fmt = function
  | PNul -> fprintf fmt "%s" "$"
  | PNode (node, left_son, right_son) ->
    fprintf fmt "%s" "(";
    pp_pattern fmt node;
    fprintf fmt "%s" "; ";
    pp_pattern fmt left_son;
    fprintf fmt "%s" "; ";
    pp_pattern fmt right_son;
    fprintf fmt "%s" ")"

and pp_pconst fmt = function
  | OrdinaryPConst const -> pp_const fmt const
  | NegativePInteger n ->
    fprintf fmt "%s" "(-";
    fprintf fmt "%s" (Integer.Nonnegative_integer.to_string n);
    fprintf fmt "%s" ")"

and pp_pat fmt = function
  | PWildcard -> fprintf fmt "%s" "_"
  | PConst pconst -> pp_pconst fmt pconst
  | PIdentificator ident -> pp_ident fmt ident
  | PList listpat -> pp_listpat fmt listpat
  | PTuple (first, second, list) ->
    fprintf fmt "%s" "(";
    pp_pattern fmt first;
    fprintf fmt "%s" ", ";
    pp_pattern fmt second;
    (match list with
     | [] -> ()
     | _ ->
       fprintf fmt "%s" ", ";
       fprintf fmt "%a" (pp_list ", " pp_pattern) list);
    fprintf fmt "%s" ")"
  | PMaybe pattern ->
    (match pattern with
     | Nothing -> fprintf fmt "%s" "Nothing"
     | Just pattern ->
       fprintf fmt "%s" "(";
       fprintf fmt "%s" "Just ";
       pp_pattern fmt pattern;
       fprintf fmt "%s" ")")
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
    ([ Ident "my"; Ident "first"; Ident "variable" ], PIdentificator (Ident "x"), [ TInteger ])
  = "((my@first@variable@x) :: Integer)"
;;

let rec pp_comprehension fmt = function
  | Condition expr -> pp_expr fmt expr
  | Generator (pattern, expr) ->
    pp_pattern fmt pattern;
    fprintf fmt "%s" " <- ";
    pp_expr fmt expr

and pp_ordinarylistbld fmt = function
  | ComprehensionList (expr, comprehension, list) ->
    fprintf fmt "%s" "[";
    pp_expr fmt expr;
    fprintf fmt "%s" " | ";
    pp_comprehension fmt comprehension;
    (match list with
     | [] -> ()
     | _ ->
       fprintf fmt "%s" ", ";
       fprintf fmt "%a" (pp_list ", " pp_comprehension) list);
    fprintf fmt "%s" "]"
  | IncomprehensionlList list -> fprintf fmt "[%a]" (pp_list ", " pp_expr) list

and pp_listbld fmt = function
  | LazyList (first, step, last) ->
    fprintf fmt "%s" "[";
    pp_expr fmt first;
    (match step with
     | None -> ()
     | Some step ->
       fprintf fmt ", ";
       pp_expr fmt step);
    fprintf fmt "%s" "..";
    (match last with
     | None -> ()
     | Some last -> pp_expr fmt last);
    fprintf fmt "%s" "]"
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
     | _ ->
       fprintf fmt "%s" " where ";
       fprintf fmt "%a" (pp_list "; " pp_binding) list)
  | FunDef (name, parameter, parameters_list, bindingbody, binding_list) ->
    pp_ident fmt name;
    fprintf fmt "%s" " ";
    pp_pattern fmt parameter;
    (match parameters_list with
     | [] -> ()
     | _ ->
       fprintf fmt "%s" " ";
       fprintf fmt "%a" (pp_list " " pp_pattern) parameters_list);
    (match bindingbody with
     | OrdBody _ -> fprintf fmt "%s" " = "
     | _ -> fprintf fmt "%s" " ");
    pp_bindingbody fmt bindingbody;
    (match binding_list with
     | [] -> ()
     | _ ->
       fprintf fmt "%s" " where ";
       fprintf fmt "%a" (pp_list "; " pp_binding) binding_list)
  | Decl (pattern, tp) ->
    pp_pattern fmt pattern;
    fprintf fmt "%s" " :: ";
    pp_tp fmt tp

and pp_condition_branch sep fmt (condition, branch) =
  pp_expr fmt condition;
  fprintf fmt "%s" sep;
  pp_expr fmt branch

and pp_bindingbody fmt = function
  | Guards ((condition, branch), list) ->
    fprintf fmt "%s" "|";
    pp_condition_branch " = " fmt (condition, branch);
    (match list with
     | [] -> ()
     | _ -> fprintf fmt "%a" (pp_list " | " (pp_condition_branch " = ")) list)
  | OrdBody expr -> pp_expr fmt expr

and pp_binary_tree_bld fmt = function
  | Nul -> fprintf fmt "%s" "$"
  | Node (node, left_son, right_son) ->
    fprintf fmt "%s" "(";
    pp_expr fmt node;
    fprintf fmt "%s" "; ";
    pp_expr fmt left_son;
    fprintf fmt "%s" "; ";
    pp_expr fmt right_son;
    fprintf fmt "%s" ")"

and pp_case_branch fmt (case, branch) =
  pp_pattern fmt case;
  match branch with
  | OrdBody _ ->
    fprintf fmt "%s" " -> ";
    pp_bindingbody fmt branch
  | Guards ((condition, branch), list) ->
    fprintf fmt "%s" "|";
    pp_condition_branch " -> " fmt (condition, branch);
    (match list with
     | [] -> ()
     | _ -> fprintf fmt "%a" (pp_list " | " (pp_condition_branch " -> ")) list)

and pp_expression fmt = function
  | Const const -> pp_const fmt const
  | Identificator ident -> pp_ident fmt ident
  | TupleBld (first, second, list) ->
    fprintf fmt "(";
    pp_expr fmt first;
    fprintf fmt "%s" ", ";
    pp_expr fmt second;
    (match list with
     | [] -> ()
     | _ ->
       fprintf fmt "%s" ", ";
       fprintf fmt "%a" (pp_list ", " pp_expr) list);
    fprintf fmt "%s" ")"
  | OptionBld expr ->
    (match expr with
     | Nothing -> fprintf fmt "%s" "Nothing"
     | Just expr ->
       fprintf fmt "%s" "Just ";
       fprintf fmt "%s" "(";
       pp_expr fmt expr;
       fprintf fmt "%s" ")")
  | ListBld listbld -> pp_listbld fmt listbld
  | Binop (first, binop, second) ->
    pp_expr fmt first;
    pp_binop fmt binop;
    pp_expr fmt second
  | Neg expr ->
    fprintf fmt "%s" "(-";
    pp_expr fmt expr;
    fprintf fmt "%s" ")"
  | IfThenEsle (condition, case, else_case) ->
    fprintf fmt "%s" "if ";
    pp_expr fmt condition;
    fprintf fmt "%s" " then ";
    pp_expr fmt case;
    fprintf fmt "%s" " else ";
    pp_expr fmt else_case
  | FunctionApply (name, argument, arguments) ->
    pp_expr fmt name;
    fprintf fmt "%s" " ";
    pp_expr fmt argument;
    fprintf fmt "%a" (pp_list " " pp_expr) arguments
  | Lambda (argument, arguments, body) ->
    fprintf fmt "%s" "\\";
    pp_pattern fmt argument;
    fprintf fmt "%a" (pp_list " " pp_pattern) arguments;
    fprintf fmt "%s" " -> ";
    pp_expr fmt body
  | BinTreeBld binary_tree_bld -> pp_binary_tree_bld fmt binary_tree_bld
  | Case (expr, (case, branch), list) ->
    fprintf fmt "%s" "case ";
    pp_expr fmt expr;
    fprintf fmt "%s" " of ";
    pp_case_branch fmt (case, branch);
    (match list with
     | [] -> ()
     | _ ->
       fprintf fmt "%s" "; ";
       fprintf fmt "%a" (pp_list "; " pp_case_branch) list)
  | InnerBindings (binding, binding_list, expr) ->
    fprintf fmt "%s" "let ";
    pp_binding fmt binding;
    (match binding_list with
     | [] -> ()
     | _ ->
       fprintf fmt "%s" "; ";
       fprintf fmt "%a" (pp_list "; " pp_binding) binding_list);
    fprintf fmt "%s" " in ";
    pp_expr fmt expr

and pp_expr fmt ((expression, tp_list) : expr) =
  pp_brackets fmt tp_list;
  fprintf fmt "%s" "(";
  pp_expression fmt expression;
  fprintf fmt "%s" ")";
  match tp_list with
  | [] -> ()
  | _ ->
    fprintf fmt "%s" " :: ";
    fprintf fmt "%a" (pp_list ") :: " pp_tp) tp_list;
    fprintf fmt "%s" ")"
;;
