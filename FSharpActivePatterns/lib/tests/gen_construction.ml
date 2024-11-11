[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open FSharpActivePatterns.Ast
open FSharpActivePatterns.AstPrinter
open FSharpActivePatterns.Parser
open FSharpActivePatterns.PrettyPrinter

let int_e x = Const (Int_lt x)
let bool_e x = Const (Bool_lt x)
let unit_e = Const Unit_lt
let string_e x = Const (String_lt x)
let variable_e x = Variable (Ident (x, None))

let gen_const =
  QCheck.Gen.(
    frequency
      [ 1, map int_e nat; 1, map bool_e bool; 0, return unit_e; 0, map string_e string ])
;;

let gen_varname =
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
;;

let gen_ident = QCheck.Gen.map (fun s -> Ident (s, None)) gen_varname
let gen_variable = QCheck.Gen.map variable_e gen_varname
let gen_unop = QCheck.Gen.(oneof @@ List.map return [ Unary_minus; Unary_not ])
let tuple_e e1 e2 rest = Tuple (e1, e2, rest)
let un_e unop e = Unary_expr (unop, e)
let bin_e op e1 e2 = Bin_expr (op, e1, e2)
let if_e i t e = If_then_else (i, t, e)
let func_def args body = Function_def (args, body)
let func_call f arg = Function_call (f, arg)
let let_bind name args body = Let_bind (name, args, body)

let letin rec_flag let_bind let_bind_list inner_e =
  LetIn (rec_flag, let_bind, let_bind_list, inner_e)
;;

let gen_binop =
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

let gen_rec_flag = QCheck.Gen.(oneof [ return Rec; return Nonrec ])

let gen_let_bind gen =
  QCheck.Gen.(map3 let_bind gen_ident (list_size (0 -- 15) gen_ident) gen)
;;

let gen_expr =
  QCheck.Gen.(
    sized
    @@ fix (fun self ->
         function
         | 0 -> frequency [ 1, gen_const; 1, gen_variable ]
         | n ->
           frequency
             [ ( 0
               , map3
                   tuple_e
                   (self (n / 2))
                   (self (n / 2))
                   (list_size (0 -- 15) (self (n / 2))) )
             ; 1, map2 un_e gen_unop (self (n / 2))
             ; 1, map3 bin_e gen_binop (self (n / 2)) (self (n / 2))
             ; ( 1
               , map3
                   if_e
                   (self (n / 2))
                   (self (n / 2))
                   (oneof [ return None; map (fun e -> Some e) (self (n / 2)) ]) )
             ; 0, map2 func_def (list_size (0 -- 15) gen_ident) (self (n / 2))
             ; 1, map2 func_call gen_variable (self (n / 2))
               (* TODO: make apply of arbitrary expr*)
             ; ( 0
               , map3
                   letin
                   gen_rec_flag
                   (gen_let_bind (self (n / 2)))
                   (list_size (0 -- 15) (gen_let_bind (self (n / 2))))
                 <*> self (n / 2) )
             ]))
;;

let shrink_lt =
  let open QCheck.Iter in
  function
  | Int_lt x -> QCheck.Shrink.int x >|= int_e
  | Bool_lt _ -> empty
  | Unit_lt -> empty
  | String_lt x -> QCheck.Shrink.string x >|= string_e
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
  | Const lt -> shrink_lt lt
  | Variable _ -> empty
  | Tuple (e1, e2, rest) ->
    shrink_expr e1
    >|= (fun a' -> Tuple (a', e2, rest))
    <+> shrink_expr e2
    >|= (fun a' -> Tuple (e1, a', rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_expr rest >|= fun a' -> Tuple (e1, e2, a'))
  | Unary_expr (op, e) -> return e <+> shrink_expr e >|= fun a' -> un_e op a'
  | Bin_expr (op, e1, e2) ->
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun a' -> bin_e op a' e2)
    <+> (shrink_expr e2 >|= fun a' -> bin_e op e1 a')
  | If_then_else (i, t, Some _) -> return (If_then_else (i, t, None))
  | If_then_else (i, t, None) ->
    shrink_expr i
    >|= (fun a' -> If_then_else (a', t, None))
    <+> (shrink_expr t >|= fun a' -> If_then_else (i, a', None))
  | LetIn (rec_flag, let_bind, let_bind_list, inner_e) ->
    shrink_let_bind let_bind
    >|= (fun a' -> LetIn (rec_flag, a', let_bind_list, inner_e))
    <+> (QCheck.Shrink.list ~shrink:shrink_let_bind let_bind_list
         >|= fun a' -> LetIn (rec_flag, let_bind, a', inner_e))
    <+> shrink_expr inner_e
    >|= fun a' -> LetIn (rec_flag, let_bind, let_bind_list, a')
  | Function_call (f, arg) ->
    shrink_expr f
    >|= (fun a' -> Function_call (a', arg))
    <+> shrink_expr arg
    >|= fun a' -> Function_call (f, a')
  | Function_def (args, body) ->
    QCheck.Shrink.list args
    >|= (fun a' -> Function_def (a', body))
    <+> (shrink_expr body >|= fun a' -> Function_def (args, a'))
  | _ -> empty
;;

let let_st rec_flag let_bind let_bind_list = Let (rec_flag, let_bind, let_bind_list)

(* TODO: Active Pattern*)
let gen_statement =
  QCheck.Gen.(
    map3
      let_st
      gen_rec_flag
      (gen_let_bind gen_expr)
      (list_size (0 -- 15) (gen_let_bind gen_expr)))
;;

(* TODO: Active Pattern *)
let shrink_statement =
  let open QCheck.Iter in
  function
  | Let (rec_flag, let_bind, let_bind_list) ->
    shrink_let_bind let_bind
    >|= (fun a' -> Let (rec_flag, a', let_bind_list))
    <+> (QCheck.Shrink.list ~shrink:shrink_let_bind let_bind_list
         >|= fun a' -> Let (rec_flag, let_bind, a'))
  | _ -> empty
;;

let gen_construction =
  QCheck.Gen.(
    oneof [ (gen_expr >|= fun a' -> Expr a'); (gen_statement >|= fun a' -> Statement a') ])
;;

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
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_construction ~count:n (fun c ->
          Some c = parse (Format.asprintf "%a\n" pp_construction c)))
    ]
;;
