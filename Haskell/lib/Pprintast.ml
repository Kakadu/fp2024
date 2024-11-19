(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

let pp_list sep pp_item =
  Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf sep) pp_item
;;

let pp_brackets fmt list =
  let rec helper = function
    | [] -> ()
    | _ :: t ->
      fprintf fmt "(";
      helper t
  in
  match list with
  | _ :: x :: xs -> helper (x :: xs)
  | _ -> ()
;;

let pp_const fmt const =
  fprintf
    fmt
    "%s"
    (match const with
     | Int n -> Int.to_string n
     | Bool b ->
       (match b with
        | true -> "True"
        | false -> "False")
     | Unit -> "()")
;;

let%test "pp Int" = asprintf "%a" pp_const (Int 18) = "18"
let%test "pp const Bool" = asprintf "%a" pp_const (Bool true) = "True"
let%test "pp const Unit" = asprintf "%a" pp_const Unit = "()"

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
  | TreeParam tp -> fprintf fmt "{%a}" pp_tp tp
  | ListParam tp -> fprintf fmt "[%a]" pp_tp tp
  | TupleParams (first, second, list) ->
    fprintf fmt "(%a)" (pp_list ", " pp_tp) (first :: second :: list)
  | FunctionType functype -> pp_functype fmt functype
;;

let%test "pp functype" =
  asprintf "%a" pp_functype (FuncT (TInt, TBool, [ TBool; TUnit ]))
  = "Int -> Bool -> Bool -> ()"
;;

let%test "pp tp TUnit" = asprintf "%a" pp_tp TUnit = "()"
let%test "pp tp TInt" = asprintf "%a" pp_tp TInt = "Int"
let%test "pp tp TBool" = asprintf "%a" pp_tp TBool = "Bool"
let%test "pp tp TreeParam" = asprintf "%a" pp_tp (TreeParam TInt) = "{Int}"
let%test "pp tp ListParam" = asprintf "%a" pp_tp (ListParam TBool) = "[Bool]"

let%test "pp tp TupleParams" =
  asprintf "%a" pp_tp (TupleParams (TInt, TBool, [ TBool; TInt ]))
  = "(Int, Bool, Bool, Int)"
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

and pp_listpat fmt = function
  | PCons (first, second) ->
    fprintf
      fmt
      (match first with
       | [], PList (PCons _), [] -> "(%a) : %a"
       | _ -> "%a : %a")
      (pp_pattern_sometimes_parensed Tp_only)
      first
      (pp_pattern_sometimes_parensed Tp_only)
      second
  | PEnum list -> fprintf fmt "[%a]" (pp_list ", " pp_pattern) list

and pp_treepat fmt = function
  | PNul -> fprintf fmt "$"
  | PNode (node, left_son, right_son) ->
    fprintf fmt "(%a; %a; %a)" pp_pattern node pp_pattern left_son pp_pattern right_son

and pp_pconst fmt = function
  | OrdinaryPConst const -> pp_const fmt const
  | NegativePInt n -> fprintf fmt "-%s" (Int.to_string n)

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
  | PConst pconst -> pp_pconst fmt pconst
  | PIdentificator ident -> pp_ident fmt ident
  | PList listpat -> pp_listpat fmt listpat
  | PTuple (first, second, list) ->
    fprintf fmt "(%a)" (pp_list ", " pp_pattern) (first :: second :: list)
  | PMaybe pattern ->
    (match pattern with
     | Nothing -> fprintf fmt "Nothing"
     | Just pattern ->
       fprintf fmt "Just %a" (pp_pattern_sometimes_parensed Tp_and_some_constrs) pattern)
  | PTree treepat -> pp_treepat fmt treepat
;;

let%test "pp listpat PCons" =
  asprintf
    "%a"
    pp_listpat
    (PCons (([], PIdentificator (Ident "x"), []), ([], PIdentificator (Ident "xs"), [])))
  = "x : xs"
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
  = "[x, y, z]"
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
  = "(x; y; z)"
;;

let%test "pp pconst OrdinaryPConst" =
  asprintf "%a" pp_pconst (OrdinaryPConst (Bool true)) = "True"
;;

let%test "pp pconst NegativePInt" = asprintf "%a" pp_pconst (NegativePInt 18) = "-18"
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
  = "x : xs"
;;

let%test "pp pat PTuple" =
  asprintf
    "%a"
    pp_pat
    (PTuple
       ( ([], PIdentificator (Ident "a"), [])
       , ([], PIdentificator (Ident "b"), [])
       , [ [], PIdentificator (Ident "c"), []; [], PIdentificator (Ident "d"), [] ] ))
  = "(a, b, c, d)"
;;

let%test "pp pat PMaybe Nothing" = asprintf "%a" pp_pat (PMaybe Nothing) = "Nothing"

let%test "pp pat PMaybe Just" =
  asprintf "%a" pp_pat (PMaybe (Just ([], PIdentificator (Ident "x"), []))) = "Just x"
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
  = "(x; y; z)"
;;

let%test "pp pattern without capture, without type" =
  asprintf "%a" pp_pattern ([], PIdentificator (Ident "x"), []) = "x"
;;

let%test "pp pattern with capture, without type" =
  asprintf
    "%a"
    pp_pattern
    ([ Ident "my"; Ident "first"; Ident "variable" ], PIdentificator (Ident "x"), [])
  = "my@first@variable@x"
;;

let%test "pp pattern with capture, with type" =
  asprintf
    "%a"
    pp_pattern
    ([ Ident "my"; Ident "first"; Ident "variable" ], PIdentificator (Ident "x"), [ TInt ])
  = "my@first@variable@x :: Int"
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
      (match tp2, get_prior expresion2 < get_prior expression with
       | [], true -> "(%a)"
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
     | ( ( Binop _
         | Neg _
         | IfThenEsle _
         | FunctionApply _
         | Lambda _
         | InnerBindings _
         | Case _ )
       , [] ) -> "(%a)"
     | _ -> "%a")
    pp_expr_parenced_tp
    expr

and pp_expr fmt ((expression, tp_list) : expr) =
  fprintf
    fmt
    (match expression, tp_list with
     | (IfThenEsle _ | Lambda _ | InnerBindings _ | Case _), _ :: _ -> "%a(%a)"
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

let%expect_test "expr_with_prio" =
  printf
    "%a"
    pp_expr
    ( Binop
        ( (Binop ((Binop (i_const 1, Plus, i_const 0), []), Multiply, i_const 2), [])
        , Greater
        , i_const 1 )
    , [] );
  [%expect {|
      (1 + 0) * 2 > 1 |}]
;;

let%expect_test "expr_with_prio_tp" =
  printf
    "%a"
    pp_expr
    ( Binop
        ( (Binop ((Binop (i_const 1, Plus, i_const 0), [ TInt ]), Multiply, i_const 2), [])
        , Greater
        , i_const 1 )
    , [ TBool ] );
  [%expect {|
      (1 + 0 :: Int) * 2 > 1 :: Bool |}]
;;

let%expect_test "expr_with_fun_app_tp" =
  printf
    "%a"
    pp_expr
    ( Binop
        ( ( FunctionApply
              ( (Identificator (Ident "f"), [ FunctionType (FuncT (TInt, TInt, [])) ])
              , (Identificator (Ident "x"), [ TInt ])
              , [ Identificator (Ident "g"), []; i_const 2 ] )
          , [] )
        , Plus
        , i_const 1 )
    , [] );
  [%expect {|
      (f :: Int -> Int) (x :: Int) g 2 + 1 |}]
;;

let%expect_test "expr_case_neg" =
  printf
    "%a"
    pp_expr
    ( Case
        ( (Neg (i_const 1), [])
        , (([], PConst (NegativePInt 1), []), OrdBody (i_const 1))
        , [] )
    , [] );
  [%expect {|
      case - 1 of -1 -> 1 |}]
;;

let%expect_test "expr_case_tp" =
  printf
    "%a"
    pp_expr
    ( Case
        ( (Neg (i_const 1), [ TInt ])
        , (([], PConst (NegativePInt 1), [ TInt ]), OrdBody (Const (Int 1), [ TInt ]))
        , [] )
    , [ TInt ] );
  [%expect {|
      (case - 1 :: Int of (-1 :: Int) -> 1 :: Int) :: Int |}]
;;

let%expect_test "expr_doble_cons_and_lam" =
  printf
    "%a"
    pp_expr
    ( Lambda
        ( ( []
          , PList
              (PCons
                 ( ([], PIdentificator (Ident "x"), [])
                 , ([], PIdentificator (Ident "xs"), []) ))
          , [] )
        , []
        , ( Binop
              ( ( Binop
                    ( (Identificator (Ident "x1"), [])
                    , Cons
                    , (Identificator (Ident "x2"), []) )
                , [] )
              , Cons
              , (Identificator (Ident "xs"), []) )
          , [] ) )
    , [] );
  [%expect {|
      \ (x : xs) -> (x1 : x2) : xs |}]
;;

let%expect_test "expr_cons_lin" =
  printf
    "%a"
    pp_expr
    ( Binop
        ( (Identificator (Ident "xs"), [])
        , Cons
        , ( Binop
              ((Identificator (Ident "x1"), []), Cons, (Identificator (Ident "x2"), []))
          , [] ) )
    , [] );
  [%expect {|
      xs : x1 : x2 |}]
;;

let%expect_test "expr_cons_lin_tp" =
  printf
    "%a"
    pp_expr
    ( Binop
        ( (Identificator (Ident "xs"), [ TUnit ])
        , Cons
        , ( Binop
              ((Identificator (Ident "x1"), []), Cons, (Identificator (Ident "x2"), []))
          , [] ) )
    , [ ListParam TUnit ] );
  [%expect {|
      (xs :: ()) : x1 : x2 :: [()] |}]
;;

let%expect_test "fac" =
  printf
    "%a"
    pp_binding
    (FunDef
       ( Ident "fac"
       , ([], PIdentificator (Ident "n"), [])
       , []
       , OrdBody
           ( IfThenEsle
               ( (Binop ((Identificator (Ident "n"), []), Less, i_const 0), [])
               , (ENothing, [])
               , ( FunctionApply
                     ( (EJust, [])
                     , ( FunctionApply
                           ( (Identificator (Ident "save_fac"), [])
                           , (Identificator (Ident "n"), [])
                           , [] )
                       , [] )
                     , [] )
                 , [] ) )
           , [] )
       , [ FunDef
             ( Ident "save_fac"
             , ([], PIdentificator (Ident "y"), [])
             , []
             , Guards
                 ( ( (Binop ((Identificator (Ident "y"), []), Equality, i_const 0), [])
                   , i_const 1 )
                 , [ ( (Identificator (Ident "otherwise"), [])
                     , ( Binop
                           ( (Identificator (Ident "y"), [])
                           , Multiply
                           , ( FunctionApply
                                 ( (Identificator (Ident "save_fac"), [])
                                 , ( Binop
                                       ((Identificator (Ident "y"), []), Minus, i_const 1)
                                   , [] )
                                 , [] )
                             , [] ) )
                       , [] ) )
                   ] )
             , [] )
         ] ));
  [%expect
    {|
      fac n = if n < 0 then Nothing else Just (save_fac n) where save_fac y | y == 0 = 1 | otherwise = y * save_fac (y - 1) |}]
;;
