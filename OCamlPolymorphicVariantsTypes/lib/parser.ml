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

(** Predicate to check string as keyword of Miniml *)
let is_keyword = function
  | "true" | "false" -> true
  | "if" | "then" | "else" -> true
  | "fun" -> true
  | "let" | "rec" | "and" | "in" -> true
  | _ -> false
;;

let ident_symbol = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '0' .. '9' -> true
  | _ -> false
;;

(** Parse [Miniml.identifier] value. *)
let ident =
  let helper = many (dsatisfy ident_symbol Fun.id) in
  skip_ws *> dsatisfy ident_symbol Fun.id
  >>= fun s ->
  if is_digit s || s = '\''
  then pfail
  else
    helper
    >>| (fun l -> string_of_char_list (s :: l))
    >>= fun id -> if is_keyword id then pfail else preturn id
;;

(** Parser of keyword *)
let keyword word =
  let helper c = not (ident_symbol c) in
  let check_end = asatisfy helper in
  skip_ws *> ssequence word *> check_end *> preturn ()
;;

(** Parser of some elements sequence:
    - [start]: first element of sequence
    - [element_parser]: parser of one element in sequence
    - [list_converter]: coverter of elements sequence to one new element
    - [separator]: char sequence which splitted sequence elements *)
let element_sequence
  : 'a 'b. 'a -> 'a parser -> ('a list -> 'b) -> string -> 'a parser -> 'b parser
  =
  fun start element_parser list_converter sep pnotfound ->
  let next_element sep =
    skip_ws
    *> (if is_keyword sep then keyword sep else ssequence sep >>| fun _ -> ())
    *> (element_parser <|> pnotfound)
  in
  skip_ws
  *> (many (next_element sep) >>| fun l -> list_converter (List.append [ start ] l))
;;

(** Parser of integer literals: [0 .. Int64.max_int].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let integer =
  let rec helper counter =
    digit
    >>= (fun v -> helper (v + (counter * 10)))
    <|> (preturn counter >>| fun v -> IntLiteral v)
  in
  skip_ws *> digit >>= helper
;;

(** Parser of boolean literals: [true], [false].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let boolean =
  skip_ws *> (keyword "true" >>| fun _ -> BoolLiteral true)
  <|> (keyword "false" >>| fun _ -> BoolLiteral false)
;;

(** Parser of patterns: [<pvar>] | [<ptuple>] *)
let rec pattern_parser state = (skip_ws *> (pvariable <|> ptuple)) state

(** Parser of variable pattern *)
and pvariable state = (skip_ws *> (ident >>| fun id -> PVar id)) state

(** Parser of tuple pattern and unit pattern *)
and ptuple state =
  let tuple_elements pfirst =
    skip_ws
    *> element_sequence
         pfirst
         pattern_parser
         (fun l -> PTuple l)
         ","
         (perror "Not found expression after tuple separator: ','")
  in
  let brackets_subexpr =
    skip_ws *> pattern_parser
    >>= (fun pfirst -> tuple_elements pfirst <|> preturn pfirst)
    <|> preturn PUnit
  in
  (skip_ws
   *> (symbol '(' *> brackets_subexpr
       <* (skip_ws *> symbol ')' <|> perror "Not found close bracket")))
    state
;;

(** Parser of constants expression: [integer] and [boolean]

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let const_expr = skip_ws *> (integer <|> boolean) >>| fun l -> Const l

(** Parser of variable expression *)
let variable = skip_ws *> ident >>| fun s -> Variable s

(** Parser of all expression which defines on [Miniml.Ast] module *)
let rec expr state = (skip_ws *> block_expr) state

(** Parser of expression block *)
and block_expr state =
  let helper ex =
    element_sequence
      ex
      tuple_expr
      (function
        | ex :: [] -> ex
        | _ as l -> ExpressionBlock l)
      ";"
      pfail
    >>= fun block ->
    assequence ";;" *> preturn block <|> ssequence ";" *> preturn block <|> preturn block
  in
  (skip_ws *> (tuple_expr >>= helper)) state

(** Parser of tuple expression *)
and tuple_expr state =
  let helper ex =
    element_sequence
      ex
      boolean_expr
      (function
        | ex :: [] -> ex
        | _ as l -> Tuple l)
      ","
      (perror "Not found expression after tuple separator: ','")
  in
  (skip_ws *> (boolean_expr >>= helper)) state

(** Parser of applyable expressions *)
and applyable_expr state = (skip_ws *> bracket_expr <|> lambda_expr <|> variable) state

(** Parser of basic expressions:
    [<unary>] | [<if-expr>] | [<define_expr>] | [<apply-expr>] | [<const>] *)
and basic_expr inapply state =
  (skip_ws *> unary_expr inapply
   <|> if_expr
   <|> define_expr
   <|> (if inapply then applyable_expr else apply_expr)
   <|> const_expr)
    state

(** Parser of unary expression *)
and unary_expr inapply state =
  let helper =
    symbol '+' *> basic_expr inapply
    <|> (symbol '-' *> basic_expr inapply >>| fun e -> Unary (Negate, e))
  in
  (skip_ws *> (helper <|> symbol '~' *> helper)) state

(** Parser of brackets expression:
    - unit: [()]
    - else: [(<expr>)] *)
and bracket_expr state =
  (skip_ws *> symbol '(' *> (expr <|> preturn (Const UnitLiteral))
   <* (skip_ws *> symbol ')' <|> perror "Not found close bracket"))
    state

(** Abstract parser of binary operations
    - [subparser]: parser of subexpression
    - [operations]: list of one priorety level binary operations *)
and binary_expression subparser operations state =
  let operation left oper =
    skip_ws
    *> ssequence oper.oper_view
    *> (subparser
        >>| (fun right -> Binary (left, oper.oper_ast, right))
        <|> perror
              (Printf.sprintf
                 "Not found right operand of '%s' binary operator"
                 oper.oper_view))
  in
  let rec next ex =
    skip_ws *> (one_of (List.map (operation ex) operations) >>= next) <|> preturn ex
  in
  (skip_ws *> subparser >>= next) state

(** Parser of binary expressions such as [<expr> * <expr>] and [<expr> / <expr>] *)
and multiply_expr state =
  binary_expression
    (basic_expr false)
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
    skip_ws *> (expr >>| fun else_ex -> If (ex, then_ex, Some else_ex))
    <|> perror "Expected expression of 'else' branch for if expression"
  in
  let then_block ex =
    skip_ws
    *> (expr
        >>= fun then_ex ->
        skip_ws *> (keyword "else" *> else_block ex then_ex)
        <|> preturn (If (ex, then_ex, None)))
    <|> perror "Expected expression of 'then' branch for if expression"
  in
  (skip_ws
   *> keyword "if"
   *> (expr
       >>= (fun ex ->
             skip_ws
             *> (keyword "then" *> then_block ex
                 <|> perror "Not found 'then' branch for if-expression"))
       <|> perror "Not found if expression after keyword 'if'"))
    state

(** Parser of apply expressions such as [<applyable_expr>  <expr list>]*)
and apply_expr state =
  let bin_op_checker =
    skip_ws
    *> one_of
         (List.map
            assequence
            [ "+"; "-"; "/"; "*"; "&&"; "||"; "<="; ">="; "<>"; "<"; ">"; "=" ])
  in
  let helper ex =
    many (skip_ws *> basic_expr true)
    >>= fun l -> preturn (if is_empty l then ex else Apply (ex, l))
  in
  (skip_ws *> applyable_expr >>= fun ex -> bin_op_checker *> preturn ex <|> helper ex)
    state

(** Parser of lambdas definitions *)
and lambda_expr state =
  (skip_ws
   *> keyword "fun"
   *> (many1 (skip_ws *> pattern_parser)
       >>= (fun pl ->
             skip_ws
             *> (ssequence "->" *> (expr >>| fun ex -> Lambda (pl, ex))
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
               >>| fun ex -> if is_empty pl then p, ex else p, Lambda (pl, ex))
           <|> perror "Not found expression of let-definition")
       <|> perror "Not found special sequence '=' of let-definition binding expresssion")
   <|> perror "Not found name-pattern of let-definition")
    state

and value_bindings_parser state =
  (skip_ws *> value_binding_parser
   >>= (fun vb ->
         element_sequence
           vb
           value_binding_parser
           Fun.id
           "and"
           (perror "Not found value binding"))
   <|> perror "Not found value binging")
    state

(** Parser of let-definitions *)
and define_expr state =
  let inexpr =
    skip_ws
    *> (keyword "in"
        *> (expr <|> perror "Not found expression after keyword 'in' of let-xpression"))
    <|> perror "Not found keyword for let-expression"
  in
  let recursive =
    skip_ws *> keyword "rec" *> value_bindings_parser
    >>= fun vbl -> inexpr >>| fun ex -> Define ((Recursive, vbl), ex)
  in
  let nonrecursive =
    skip_ws *> value_bindings_parser
    >>= fun vbl -> inexpr >>| fun ex -> Define ((Nonrecursive, vbl), ex)
  in
  (skip_ws *> keyword "let" *> (recursive <|> nonrecursive)) state
;;

(** Parser of definition item *)
let define_item =
  let inexpr =
    skip_ws
    *> keyword "in"
    *> (expr <|> perror "Not found expression after keyword 'in' of let-xpression")
  in
  let recursive =
    skip_ws *> keyword "rec" *> value_bindings_parser
    >>= fun vbl ->
    inexpr
    >>| (fun ex -> EvalItem (Define ((Recursive, vbl), ex)))
    <|> preturn (DefineItem (Recursive, vbl))
  in
  let nonrecursive =
    skip_ws *> value_bindings_parser
    >>= fun vbl ->
    inexpr
    >>| (fun ex -> EvalItem (Define ((Nonrecursive, vbl), ex)))
    <|> preturn (DefineItem (Nonrecursive, vbl))
  in
  skip_ws *> keyword "let" *> (recursive <|> nonrecursive)
;;

(** Parser of eval item *)
let eval_item = skip_ws *> expr >>| fun ex -> EvalItem ex

(** Parser of all stricture item *)
let struct_item_parser =
  skip_ws *> (define_item <|> eval_item)
  >>= fun item -> skip_ws *> ssequence ";;" *> preturn item
;;

(** Parser of program item *)
let program_parser = skip_ws *> many struct_item_parser
