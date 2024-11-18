[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open FSharpActivePatterns.Ast
open FSharpActivePatterns.AstPrinter
open FSharpActivePatterns.Parser
open FSharpActivePatterns.PrettyPrinter
(*open FSharpActivePatterns.KeywordChecker*)

(*
   let gen_const_manual =
   QCheck.Gen.(
   frequency
   [ 1, map int_e nat
      ; 1, map bool_e bool
      ; 0, return unit_e
      ; 0, map string_e (string ?gen:None)
      ])
   ;;

   let gen_varname_manual =
   let open QCheck.Gen in
   let loop =
   let gen_char_of_range l r = map Char.chr (int_range (Char.code l) (Char.code r)) in
   let gen_first_char =
   oneof [ gen_char_of_range 'a' 'z'; gen_char_of_range 'A' 'Z'; return '_' ]
   in
   let gen_next_char = oneof [ gen_first_char; gen_char_of_range '0' '9' ] in
   map2
   (fun first rest ->
   String.make 1 first ^ String.concat "" (List.map (String.make 1) rest))
   gen_first_char
   (list_size (1 -- 5) gen_next_char)
   in
   loop >>= fun name -> if is_keyword name then loop else return name
   ;; *)

(*
   let gen_ident_manual = QCheck.Gen.map (fun s -> Ident (s, None)) gen_varname_manual
let gen_variable_manual = QCheck.Gen.map variable_e gen_varname_manual
let gen_unop_manual = QCheck.Gen.(oneof @@ List.map return [ Unary_minus; Unary_not ]) 
let tuple_e e1 e2 rest = Tuple (e1, e2, rest)
let un_e unop e = Unary_expr (unop, e)*)
let bin_e op e1 e2 = Bin_expr (op, e1, e2)
(*
   let if_e i t e = If_then_else (i, t, e)
   let func_def pat pat_list body = Lambda (pat, pat_list, body)
   let apply f arg = Apply (f, arg)
   let let_bind name args body = Let_bind (name, args, body) *)

(*
   let letin rec_flag let_bind let_bind_list inner_e =
   LetIn (rec_flag, let_bind, let_bind_list, inner_e)
   ;; *)

(*
   let gen_binop_manual =
  QCheck.Gen.(
    oneof
    @@ List.map
         return
         [ Binary_equal
         ; Binary_unequal
         ; Binary_less
         ; Binary_less_or_equal
         ; Binary_greater
         ; Binary_greater_or_equal
         ; Binary_add
         ; Binary_subtract
         ; Binary_multiply
         ; Logical_or
         ; Logical_and
         ; Binary_divide
           (* ; Binary_or_bitwise
              ; Binary_xor_bitwise
              ; Binary_and_bitwise *)
         ])
;;

let gen_is_recursive_manual = QCheck.Gen.(oneof [ return Rec; return Nonrec ])

let gen_let_bind_manual gen =
  QCheck.Gen.(map3 let_bind gen_ident_manual (list_size (0 -- 15) gen_ident_manual) gen)
;; *)

(*
   let gen_expr_manual =
  QCheck.Gen.(
    sized
    @@ fix (fun self ->
         function
         | 0 -> frequency [ 1, gen_const_manual; 1, gen_variable_manual ]
         | n ->
           frequency
             [ ( 1
               , map3
                   tuple_e
                   (self (n / 4))
                   (self (n / 4))
                   (list_size (0 -- 15) (self (n / 4))) )
             ; 1, map2 un_e gen_unop_manual (self (n / 4))
             ; 1, map3 bin_e gen_binop_manual (self (n / 4)) (self (n / 4))
             ; ( 1
               , map3
                   if_e
                   (self (n / 4))
                   (self (n / 4))
                   (oneof [ return None; map (fun e -> Some e) (self (n / 4)) ]) )
             ; 0, map2 func_def (list_size (0 -- 15) gen_ident_manual) (self (n / 4))
             ; 1, map2 func_call gen_variable_manual (self (n / 4))
               (* TODO: make apply of arbitrary expr*)
             ; ( 0
               , map3
                   letin
                   gen_is_recursive_manual
                   (gen_let_bind_manual (self (n / 4)))
                   (list_size (0 -- 15) (gen_let_bind_manual (self (n / 4))))
                 <*> self (n / 4) )
             ]))
;; *)

let shrink_lt =
  let open QCheck.Iter in
  function
  | Int_lt x -> QCheck.Shrink.int x >|= fun a' -> Int_lt a'
  | Bool_lt _ -> empty
  | Unit_lt -> empty
  | String_lt x -> QCheck.Shrink.string x >|= fun a' -> String_lt a'
;;

let rec shrink_let_bind =
  let open QCheck.Iter in
  function
  | Let_bind (name, args, e) ->
    shrink_expr e
    >|= (fun a' -> Let_bind (name, args, a'))
    <+> (QCheck.Shrink.list args >|= fun a' -> Let_bind (name, a', e))

and shrink_expr =
  let open QCheck.Iter in
  function
  | Const lt -> shrink_lt lt >|= fun a' -> Const a'
  | Tuple (e1, e2, rest) ->
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun a' -> Tuple (a', e2, rest))
    <+> (shrink_expr e2 >|= fun a' -> Tuple (e1, a', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_expr rest >|= fun a' -> Tuple (e1, e2, a'))
  | List (Cons_list (hd, Cons_list (hd2, tl))) ->
    of_list
      [ List (Cons_list (hd, Empty_list))
      ; List (Cons_list (hd2, tl))
      ; List (Cons_list (hd, tl))
      ; List tl
      ]
    <+> (shrink_expr hd >|= fun hd' -> List (Cons_list (hd', Cons_list (hd2, tl))))
    <+> (shrink_expr hd2 >|= fun hd2' -> List (Cons_list (hd, Cons_list (hd2', tl))))
  | List (Cons_list (hd, Empty_list)) ->
    shrink_expr hd >|= fun hd' -> List (Cons_list (hd', Empty_list))
  | Bin_expr (op, e1, e2) ->
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun a' -> bin_e op a' e2)
    <+> (shrink_expr e2 >|= fun a' -> bin_e op e1 a')
  | Unary_expr (op, e) -> return e <+> (shrink_expr e >|= fun e' -> Unary_expr (op, e'))
  | If_then_else (i, t, Some e) ->
    of_list [ i; t; e; If_then_else (i, e, None) ]
    <+> (shrink_expr i >|= fun a' -> If_then_else (a', t, Some e))
    <+> (shrink_expr t >|= fun a' -> If_then_else (i, a', Some e))
  | If_then_else (i, t, None) ->
    of_list [ i; t ]
    <+> (shrink_expr i >|= fun a' -> If_then_else (a', t, None))
    <+> (shrink_expr t >|= fun a' -> If_then_else (i, a', None))
  | LetIn (rec_flag, let_bind, let_bind_list, inner_e) ->
    return inner_e
    <+> (shrink_let_bind let_bind
         >|= fun a' -> LetIn (rec_flag, a', let_bind_list, inner_e))
    <+> (QCheck.Shrink.list ~shrink:shrink_let_bind let_bind_list
         >|= fun a' -> LetIn (rec_flag, let_bind, a', inner_e))
    <+> (shrink_expr inner_e >|= fun a' -> LetIn (rec_flag, let_bind, let_bind_list, a'))
  | Apply (f, arg) ->
    of_list [ f; arg ]
    <+> (shrink_expr f >|= fun a' -> Apply (a', arg))
    <+> (shrink_expr arg >|= fun a' -> Apply (f, a'))
  | Lambda (pat, pat_list, body) ->
    shrink_expr body
    >|= (fun body' -> Lambda (pat, pat_list, body'))
    <+> (QCheck.Shrink.list ~shrink:shrink_pattern pat_list
         >|= fun pat_list' -> Lambda (pat, pat_list', body))
  | Match (value, pat1, expr1, cases) ->
    of_list [ value; expr1 ]
    <+> (shrink_expr value >|= fun a' -> Match (a', pat1, expr1, cases))
    <+> (shrink_pattern pat1 >|= fun a' -> Match (value, a', expr1, cases))
    <+> (shrink_expr expr1 >|= fun a' -> Match (value, pat1, a', cases))
    <+> (QCheck.Shrink.list
           ~shrink:(fun (p, e) ->
             let* p_shr = shrink_pattern p in
             let* e_shr = shrink_expr e in
             return (p_shr, e_shr))
           cases
         >|= fun a' -> Match (value, pat1, expr1, a'))
  | Option (Some e) ->
    of_list [ e; Option None ] <+> (shrink_expr e >|= fun a' -> Option (Some a'))
  | Option None -> empty
  | Variable _ -> empty
  | List Empty_list -> empty

and shrink_pattern =
  let open QCheck.Iter in
  function
  | PList (Cons_list (hd, Cons_list (hd2, tl))) ->
    of_list
      [ PList (Cons_list (hd, Empty_list))
      ; PList (Cons_list (hd2, tl))
      ; PList (Cons_list (hd, tl))
      ; PList tl
      ]
    <+> (shrink_pattern hd >|= fun hd' -> PList (Cons_list (hd', Cons_list (hd2, tl))))
    <+> (shrink_pattern hd2 >|= fun hd2' -> PList (Cons_list (hd, Cons_list (hd2', tl))))
  | PList (Cons_list (hd, Empty_list)) ->
    shrink_pattern hd >|= fun hd' -> PList (Cons_list (hd', Empty_list))
  | PTuple (p1, p2, rest) ->
    of_list [ p1; p2 ]
    <+> (shrink_pattern p1 >|= fun p1' -> PTuple (p1', p2, rest))
    <+> (shrink_pattern p2 >|= fun p2' -> PTuple (p1, p2', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_pattern rest
         >|= fun rest' -> PTuple (p1, p2, rest'))
  | PConst lt -> shrink_lt lt >|= fun lt' -> PConst lt'
  | PList Empty_list -> empty
  | Wild -> empty
  | PVar _ -> empty
;;

(*
   let let_st rec_flag let_bind let_bind_list = Let (rec_flag, let_bind, let_bind_list)*)

(* TODO: Active Pattern*)
(*
   let gen_statement_manual =
   QCheck.Gen.(
   map3
   let_st
   gen_is_recursive_manual
   (gen_let_bind_manual gen_expr_manual)
   (list_size (0 -- 15) (gen_let_bind_manual gen_expr_manual)))
   ;; *)

let shrink_statement =
  let open QCheck.Iter in
  function
  | Let (rec_flag, let_bind, let_bind_list) ->
    shrink_let_bind let_bind
    >|= (fun a' -> Let (rec_flag, a', let_bind_list))
    <+> (QCheck.Shrink.list ~shrink:shrink_let_bind let_bind_list
         >|= fun a' -> Let (rec_flag, let_bind, a'))
;;

(*| ActivePattern (cases, e) ->
  QCheck.Shrink.list cases
  >|= (fun a' -> ActivePattern (a', e))
  <+> (shrink_expr e >|= fun a' -> ActivePattern (cases, a')) *)

(*
   let gen_construction_manual =
   QCheck.Gen.(
   oneof
   [ (gen_expr_manual >|= fun a' -> Expr a')
      ; (gen_statement_manual >|= fun a' -> Statement a')
      ])
   ;; *)

let shrink_construction =
  let open QCheck.Iter in
  function
  | Expr e -> shrink_expr e >|= fun a' -> Expr a'
  | Statement s -> shrink_statement s >|= fun a' -> Statement a'
;;

let arbitrary_construction =
  QCheck.make
    gen_construction
    ~print:(Format.asprintf "%a" print_construction)
    ~shrink:shrink_construction
;;

let run n =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_construction ~count:n (fun c ->
          Some c = parse (Format.asprintf "%a\n" pp_construction c)))
    ]
;;
