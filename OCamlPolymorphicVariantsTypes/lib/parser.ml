(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility
open Utils

(** Data record which contains [Miniml.Ast]
    view of [binary_operator]
    and it's view in string *)
type binary_operator_parse_data =
  { oper_view : string
  ; oper_ast : binary_operator
  }

(**  *)
let is_keyword = function
  | "true" | "false" -> true
  | "if" | "then" | "else" -> true
  | "fun" -> true
  | "let" | "rec" | "and" | "in" -> true
  | _ -> false
;;

(** Parser of some elements sequence:
    - [start]: first element of sequence
    - [element_parser]: parser of one element in sequence
    - [list_converter]: coverter of elements sequence to one new element
    - [separator]: char sequence which splitted sequence elements *)
let element_sequence : 'a 'b. 'a -> 'a parser -> ('a list -> 'b) -> string -> 'b parser =
  fun start element_parser list_converter sep ->
  let next_element sep =
    skip_ws
    *> ssequence sep
    *> (element_parser
        <|> perror (Printf.sprintf "Not found elements after separator: '%s'" sep))
  in
  skip_ws
  *> (ssequence sep
      >>> many (next_element sep)
      >>= fun l -> preturn (list_converter (List.append [ start ] l)))
;;

(** Parse [Miniml.identifier] value. *)
let ident =
  let ident_symbol = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
    | _ -> false
  in
  let helper = many1 (dsatisfy ident_symbol (fun c -> c)) in
  skip_ws *> helper
  >>= fun l ->
  if is_digit (List.nth l 0)
  then pfail
  else
    preturn (string_of_char_list l)
    >>= fun id -> if is_keyword id then pfail else preturn id
;;

(** Parser of integer literals: [0 .. Int64.max_int].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let integer =
  let rec helper counter =
    digit
    >>= (fun v -> helper (v + (counter * 10)))
    <|> (preturn counter >>= fun v -> preturn (IntLiteral v))
  in
  skip_ws *> digit >>= fun d -> helper d
;;

(** Parser of boolean literals: [true], [false].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let boolean =
  skip_ws *> ssequence "true"
  <|> ssequence "false"
  >>= fun cl -> preturn (BoolLiteral (List.length cl = 4))
;;

(** Parser of patterns: [<pvar>] | [<ptuple>] *)
let rec pattern_parser state = (skip_ws *> (pvariable <|> ptuple)) state

(** Parser of variable pattern *)
and pvariable state = (skip_ws *> (ident >>= fun id -> preturn (PVar id))) state

(** Parser of tuple pattern and unit pattern *)
and ptuple state =
  let tuple_elements pfirst =
    skip_ws *> element_sequence pfirst pattern_parser (fun l -> PTuple l) ","
  in
  let brackets_subexpr =
    skip_ws *> pattern_parser
    >>= (fun pfirst ->
          tuple_elements pfirst
          <|> (skip_ws *> symbol ')' >>> preturn pfirst)
          <|> perror "Unsupported separator of tuple pattern")
    <|> preturn PUnit
  in
  (skip_ws
   *> (symbol '(' *> brackets_subexpr <* (symbol ')' <|> perror "Not found close bracket"))
  )
    state
;;

(** Parser of constants expression: [integer] and [boolean]

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let const_expr = skip_ws *> integer <|> boolean >>= fun id -> preturn (Const id)

(** Parser of variable expression *)
let variable = skip_ws *> ident >>= fun s -> preturn (Variable s)

(** Parser of all expression which defines on [Miniml.Ast] module *)
let rec expr state = apply_expr state

(** Parser of basic expressions: [<unary>] | [<const>] *)
and basic_expr state =
  (skip_ws *> unary_expr
   <|> bracket_expr
   <|> if_expr
   <|> lambda_expr
   <|> variable
   <|> const_expr)
    state

(** Parser of unary expression *)
and unary_expr state =
  let helper =
    symbol '+' *> basic_expr
    <|> (symbol '-' *> skip_ws *> basic_expr >>= fun e -> preturn (Unary (Negate, e)))
  in
  (skip_ws *> (helper <|> symbol '~' *> helper)) state

(** Parser of expression sequence:
    - [start]: first element of sequence
    - [converter]: coverter of elements sequence to one new element
    - [separator]: char sequence which splitted sequence elements *)
and expression_sequence start converter separator =
  element_sequence start expr converter separator

(** Parser of brackets expression:
    - unit: [()]
    - one expression: [(<expr>)]
    - expression block: [(<expr>; ...; <expr>)]
    - tuple: [(<expr>, ..., <expr>)] *)
and bracket_expr state =
  let expr_block ex =
    skip_ws *> expression_sequence ex (fun l -> ExpressionBlock l) ";"
  in
  let tuple_expr ex = skip_ws *> expression_sequence ex (fun l -> Tuple l) "," in
  let brackets_subexpr =
    skip_ws *> expr
    >>= (fun ex ->
          expr_block ex
          <|> tuple_expr ex
          <|> (skip_ws *> symbol ')' >>> preturn ex)
          <|> perror "Unsupported separator of bracket expression")
    <|> preturn (Const UnitLiteral)
  in
  (skip_ws
   *> (symbol '(' *> brackets_subexpr
       <* (skip_ws *> symbol ')' <|> perror "Not found close bracket")))
    state

(** Abstract parser of binary operations
    - [subparser]: parser of subexpression
    - [operations]: list of one priorety level binary operations *)
and binary_expression subparser operations state =
  let operation left oper =
    skip_ws
    *> ssequence oper.oper_view
    *> (subparser
        >>= (fun right -> preturn (Binary (left, oper.oper_ast, right)))
        <|> perror
              (Printf.sprintf
                 "Not found right operand of '%s' binary operator"
                 oper.oper_view))
  in
  let rec next ex =
    skip_ws *> (one_of (List.map (operation ex) operations) >>= fun e -> next e)
    <|> preturn ex
  in
  (skip_ws *> subparser >>= fun ex -> next ex) state

(** Parser of binary expressions such as [<expr> * <expr>] and [<expr> / <expr>] *)
and multiply_expr state =
  binary_expression
    basic_expr
    [ { oper_view = "*"; oper_ast = Multiply }; { oper_view = "/"; oper_ast = Division } ]
    state

(** Parser of binary expressions such as [<expr> + <expr>] and [<expr> - <expr>] *)
and summary_expr state =
  binary_expression
    multiply_expr
    [ { oper_view = "+"; oper_ast = Add }; { oper_view = "-"; oper_ast = Subtract } ]
    state

(** Parser of binary expressions such as
    - [<expr> = <expr>]
    - [<expr> <> <expr>]
    - [<expr> > <expr>]
    - [<expr> < <expr>]
    - [<expr> >= <expr>]
    - [<expr> <= <expr>] *)
and compare_expr state =
  binary_expression
    summary_expr
    [ { oper_view = "="; oper_ast = Equals }
    ; { oper_view = "<>"; oper_ast = Unequals }
    ; { oper_view = ">="; oper_ast = Gte }
    ; { oper_view = "<="; oper_ast = Lte }
    ; { oper_view = ">"; oper_ast = Gt }
    ; { oper_view = "<"; oper_ast = Lt }
    ]
    state

(** Parser of binary expressions such as [<expr> && <expr>] and [<expr> || <expr>] *)
and boolean_expr state =
  binary_expression
    compare_expr
    [ { oper_view = "&&"; oper_ast = And }; { oper_view = "||"; oper_ast = Or } ]
    state

and if_expr state =
  let else_block ex then_ex =
    skip_ws *> (expr >>= fun else_ex -> preturn (If (ex, then_ex, Some else_ex)))
    <|> perror "Expected expression of 'else' branch for if expression"
  in
  let then_block ex =
    skip_ws
    *> (expr
        >>= fun then_ex ->
        skip_ws *> (ssequence "else" *> else_block ex then_ex)
        <|> preturn (If (ex, then_ex, None)))
    <|> perror "Expected expression of 'then' branch for if expression"
  in
  (skip_ws
   *> ssequence "if"
   *> (expr
       >>= (fun ex ->
             skip_ws
             *> (ssequence "then" *> then_block ex
                 <|> perror "Not found 'then' branch for if-expression"))
       <|> perror "Not found if expression after keyword 'if'"))
    state

(** Parser of apply expressions such as [<applyable_expr>  <expr list>]*)
and apply_expr state =
  (skip_ws *> boolean_expr
   >>= fun ex ->
   many (skip_ws *> expr) >>= fun l -> preturn (if is_empty l then ex else Apply (ex, l))
  )
    state

(** Parser of lambdas definitions *)
and lambda_expr state =
  (skip_ws
   *> ssequence "fun"
   *> (many1 (skip_ws *> pattern_parser)
       >>= (fun pl ->
             skip_ws
             *> (ssequence "->" *> (expr >>= fun ex -> preturn (Lambda (pl, ex)))
                 <|> perror "Not found expression of lambda")
             <|> perror "Not found special sequence '->' of lambda definition")
       <|> perror "Not found patterns for lambda definition"))
    state

(** Parser of value-bindings: [<pattern> = <expr>]*)
and value_binding_parser state =
  (skip_ws
   *> (pattern_parser
       >>= fun p ->
       many (skip_ws *> pattern_parser)
       >>= fun pl ->
       skip_ws
       *> (ssequence "="
           *> (skip_ws *> expr
               >>= fun ex -> preturn (if is_empty pl then p, ex else p, Lambda (pl, ex)))
           <|> perror "Not found expression of let-definition")
       <|> perror "Not found special sequence '=' of let-definition")
   <|> perror "Not found name-pattern of let-definition")
    state

(** Parser of let-definitions *)
and define_expr state =
  let nonrecursive state =
    (value_binding_parser
     >>= fun vb ->
     skip_ws
     *> (ssequence "in" <|> perror "Not found  sequence 'in' of let-definition")
     *> (expr >>= fun ex -> preturn (Define ((Nonrecursive, [ vb ]), ex)))
     <|> perror "Not found in-expression of let-definition")
      state
  in
  let recursive state =
    (skip_ws *> value_binding_parser
     >>= fun vb ->
     many
       (skip_ws
        *> (ssequence "and"
            *> (value_binding_parser
                <|> perror "Not found value binding one of recusion difinition")))
     >>= fun vbl ->
     (skip_ws *> ssequence "in" <|> perror "Not found  sequence 'in' of let-definition")
     *> (expr
         >>= fun ex ->
         preturn (Define ((Recursive, vb :: vbl), ex))
         <|> perror "Not found in-expression of let-definition"))
      state
  in
  (skip_ws *> ssequence "let" *> skip_ws *> (ssequence "rec" *> recursive <|> nonrecursive)
  )
    state
;;
