(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Ast

(* open Ast
   open Pr_printer
   open Parser *)
(* open Ast *)
open Pr_printer
open Parser

(*------------------Generator-----------------*)
(* let gen_const =
  let open Gen in
  oneof
    [ (*  Gen.map (fun i -> Int i) Gen.int *)
      map (fun i -> Int i) (int_range (-1073741824) 1073741823)
      (* ; for now because string misbehave map (fun s -> String s) (string_size ~gen:printable (int_bound 20)) *)
      (* ; Gen.map (fun s -> String s) Gen.string *)
    ; map (fun b -> Bool b) bool
    ; return Unit
    ]
;;

let gen_bin_op = Gen.oneofl [ Add; Mult; Sub; Div; Gt; Lt; Eq; Neq; Gte; Lte; And; Or ]
let gen_un_op = Gen.oneofl [ Negative; Positive; Not ]
let gen_rec_flag = Gen.oneofl [ Recursive; Non_recursive ]

(* let gen_id = QCheck.Gen.string_size ~gen:QCheck.Gen.printable (QCheck.Gen.int_bound 20) *)
let gen_id =
  let open Gen in
  let first_char = oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; return '_' ] in
  let rest_char =
    frequency
      [ 26, char_range 'a' 'z'
      ; 26, char_range 'A' 'Z'
      ; 10, char_range '0' '9'
      ; 1, return '_'
      ; 1, return '\''
      ]
  in
  (* length limit of 20 characters (for the sake of being able to read it) *)
  let gen_rest = string_size ~gen:rest_char (int_range 0 19) in
  map2 (fun start rest -> String.make 1 start ^ rest) first_char gen_rest
;;

(* let gen_id = Gen.sized (fun n -> Gen.string_size ~gen:Gen.printable (Gen.return n)) *)
let gen_pattern =
  Gen.oneof
    [ Gen.map (fun id -> PVar id) gen_id
      (* ; Gen.map (fun c -> PConst c) gen_const *)
      (* ; Gen.return PAny *)
    ]
;;

let gen_expr =
  let open Gen in
  sized
  @@ fix (fun self ->
       function
       | 0 -> oneof [ map (fun c -> Econst c) gen_const; map (fun v -> Evar v) gen_id ]
       | n ->
         frequency
           [ 1, map (fun c -> Econst c) gen_const
           ; 1, map (fun v -> Evar v) gen_id
           ; ( 2
             , map3
                 (fun e1 e2 e3 -> Eif_then_else (e1, e2, e3))
                 (self (n / 2))
                 (self (n / 2))
                 (oneof [ map (fun e -> Some e) (self (n / 2)); return None ]) )
           ; 2, map (fun es -> Elist es) (list_size (int_bound 10) (self (n / 2)))
           ; ( 2
             , map3
                 (fun e1 e2 rest -> Etuple (e1, e2, rest))
                 (self (n / 2))
                 (self (n / 2))
                 (list_size (int_bound 5) (self (n / 2))) )
           ; ( 2
             , map
                 (fun e -> Eoption e)
                 (oneof [ map (fun e -> Some e) (self (n / 2)); return None ]) )
             (* ; 2, map (fun es -> Elist es) (list (self (n / 2))) *)
             (* ; 1, map (fun e -> Eoption (Some e)) (self (n / 2))
                ; 1, map (fun _ -> Eoption None) (self (n / 2)) *)
           ; ( 2
             , map3
                 (fun op e1 e2 -> Ebin_op (op, e1, e2))
                 gen_bin_op
                 (self (n / 2))
                 (self (n / 2)) )
           ; 2, map2 (fun op e -> Eun_op (op, e)) gen_un_op (self (n / 2))
           ; ( 1
             , map3
                 (fun vb vbl e2 -> Elet (Recursive, vb, vbl, e2))
                 (map2 (fun id e -> Evalue_binding (id, e)) gen_id (self (n / 2)))
                 (list_size
                    (int_bound 10)
                    (map2 (fun id e -> Evalue_binding (id, e)) gen_id (self (n / 2))))
                 (self (n / 2)) )
           ; ( 1
             , map3
                 (fun id e1 e2 -> Elet (Non_recursive, id, e1, e2))
                 (map2 (fun id e -> Evalue_binding (id, e)) gen_id (self (n / 2)))
                 (list_size
                    (int_bound 10)
                    (map2 (fun id e -> Evalue_binding (id, e)) gen_id (self (n / 2))))
                 (self (n / 2)) )
           ; ( 2
             , map2 (fun e1 e2 -> Efun_application (e1, e2)) (self (n / 2)) (self (n / 2))
             )
           ; ( 2
             , map2
                 (fun patterns body -> Efun (patterns, body))
                 (list_size (int_bound 10) gen_pattern)
                 (* (list gen_pattern) *)
                 (self (n / 2)) )
           ])
;;

let gen_value_binding = Gen.map2 (fun id e -> Evalue_binding (id, e)) gen_id gen_expr

let gen_structure_item =
  let open Gen in
  frequency
    [ 1, map (fun e -> SEval e) gen_expr
    ; ( 2
      , gen_rec_flag
        >>= fun r ->
        gen_value_binding
        >>= fun vb ->
        list_size (int_bound 2) gen_value_binding
        >>= fun vb_l -> gen_expr >>= fun e2 -> return (SValue (r, vb, vb_l, e2)) )
    ]
;;

(* let gen_structure : structure QCheck.Gen.t = Gen.list gen_structure_item *)
let gen_structure =
  let open Gen in
  list_size (int_bound 2) gen_structure_item
;;

(*------------------Shrinker-----------------*)

let rec shrink_expr = function
  | Econst (Int _) -> Iter.return (Econst (Int 1))
  | Econst (Bool b) -> Iter.return (Econst (Bool b))
  | Econst _ -> Iter.empty
  | Evar _ -> Iter.return (Evar "a")
  | Ebin_op (op, e1, e2) ->
    Iter.(
      return e1
      <+> return e2
      <+> map (fun e1' -> Ebin_op (op, e1', e2)) (shrink_expr e1)
      <+> map (fun e2' -> Ebin_op (op, e1, e2')) (shrink_expr e2)
      <+> return (Ebin_op (Add, e1, e2)))
  | Eun_op (_, e) ->
    Iter.(return e <+> map (fun e' -> Eun_op (Negative, e')) (shrink_expr e))
  | Eif_then_else (cond_e, then_e, Some else_e) ->
    Iter.(
      return then_e
      <+> return else_e
      <+> map
            (fun cond_e' -> Eif_then_else (cond_e', then_e, Some else_e))
            (shrink_expr cond_e)
      <+> map
            (fun then_e' -> Eif_then_else (cond_e, then_e', Some else_e))
            (shrink_expr then_e)
      <+> map
            (fun else_e' -> Eif_then_else (cond_e, then_e, Some else_e'))
            (shrink_expr else_e))
  | Eif_then_else (cond_e, then_e, None) ->
    Iter.(
      return then_e
      <+> map (fun cond_e' -> Eif_then_else (cond_e', then_e, None)) (shrink_expr cond_e)
      <+> map (fun then_e' -> Eif_then_else (cond_e, then_e', None)) (shrink_expr then_e))
  | Eoption (Some e) -> shrink_expr e
  | Eoption None -> Iter.empty
  | Elist es ->
    Iter.(
      (*removing elements from the list *)
      let shrink_list_length =
        map (fun es' -> Elist es') (QCheck.Shrink.list ~shrink:shrink_expr es)
      in
      (* shrink each element within the list *)
      let shrink_elements =
        List.fold_right
          (fun e acc ->
            map
              (fun e' -> Elist (List.map2 (fun x y -> if x = e then e' else y) es es))
              (shrink_expr e)
            <+> acc)
          es
          empty
      in
      shrink_list_length <+> shrink_elements)
  | Etuple (e1, e2, e3) ->
    Iter.(
      map (fun e1' -> Etuple (e1', e2, e3)) (shrink_expr e1)
      <+> map (fun e2' -> Etuple (e1, e2', e3)) (shrink_expr e2)
      <+> map
            (fun e3' -> Etuple (e1, e2, e3'))
            (QCheck.Shrink.list ~shrink:shrink_expr e3))
  | Elet (flag, vb, vb_l, e) ->
    Iter.(
      let shrink_value_binding_length =
        map (fun vb_l' -> Elet (flag, vb, vb_l', e)) (QCheck.Shrink.list vb_l)
      in
      return e
      <+> map (fun e' -> Elet (flag, vb, vb_l, e')) (shrink_expr e)
      <+> shrink_value_binding_length)
  | Efun (patterns, body) ->
    Iter.(
      let shrink_patterns_length =
        map (fun patterns' -> Efun (patterns', body)) (QCheck.Shrink.list patterns)
      in
      map (fun body' -> Efun (patterns, body')) (shrink_expr body)
      <+> shrink_patterns_length)
  | Efun_application (e1, e2) ->
    Iter.(
      return e1
      <+> return e2
      <+> map (fun e1' -> Efun_application (e1', e2)) (shrink_expr e1)
      <+> map (fun e2' -> Efun_application (e1, e2')) (shrink_expr e2))
;;

let shrink_structure_item = function
  | SEval e -> Iter.(map (fun e' -> SEval e') (shrink_expr e))
  | SValue (r, vb, vb_l, e) ->
    (* Iter.(return e1 <+> return e2 <+> shrink_expr e1 <+> shrink_expr e2) *)
    Iter.(
      let shrink_value_binding_length =
        map (fun vb_l' -> SValue (r, vb, vb_l', e)) (QCheck.Shrink.list vb_l)
      in
      map (fun e' -> SValue (r, vb, vb_l, e')) (shrink_expr e)
      <+> shrink_value_binding_length)
;;

let shrink_structure structure : structure Iter.t =
  match structure with
  | [] -> Iter.empty
  | _ ->
    (* Iter.(return structure <+> return rest <+> (shrink_structure_item str_item ):: rest) *)
    Iter.(
      let shrink_elements =
        List.fold_right
          (fun e acc ->
            map
              (fun e' ->
                List.map2 (fun x y -> if x == e then e' else y) structure structure)
              (shrink_structure_item e)
            <+> acc)
          structure
          empty
      in
      QCheck.Shrink.list structure <+> shrink_elements)
;;

let arbitrary_structure_manual =
  make gen_structure ~print:(Format.asprintf "%a" prpr_structure) ~shrink:shrink_structure
;;

let run_manual () =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_structure_manual (fun structure ->
          Result.ok structure = parse_expr (Format.asprintf "%a" prpr_structure structure)))
    ]
;; *)

(* Reuse types from the Ast module and add QCheck generators *)
(* include (
   struct *)
(* let gen_id =
   let open Gen in
   let first_char = oneof [ char_range 'a' 'z'; return '_' ] in
   let rest_char =
   frequency
   [ 26, char_range 'a' 'z'
      ; (* lowercase letters *)
        26, char_range 'A' 'Z'
      ; (* uppercase letters *)
        10, char_range '0' '9'
      ; (* digits *)
        1, return '_'
      ; (* underscore *)
        1, return '\'' (* apostrophe *)
      ]
   in
   (* limit the total length to 20 characters *)
   let gen_rest = string_size ~gen:rest_char (int_range 0 19) in
   (* combine the first character with the generated rest part *)
   map2 (fun start rest -> String.make 1 start ^ rest) first_char gen_rest
   ;; *)

(* let gen_limited_list gen_list max_size =
   let open Gen in
   let size = int_range 0 max_size in
   list_size size gen_list
   ;;

   type const = Ast.const =
   | Int of (int[@gen Gen.int])
   | String of (string[@gen Gen.string])
   | Bool of (bool[@gen Gen.bool])
   | Unit
   [@@deriving qcheck, show]

   type bin_op = Ast.bin_op =
   | Add
   | Mult
   | Sub
   | Div
   | Gt
   | Lt
   | Eq
   | Neq
   | Gte
   | Lte
   | And
   | Or
   [@@deriving qcheck, show]

   type un_op = Ast.un_op =
   | Negative
   | Positive
   | Not
   [@@deriving qcheck, show]

   type id = (string[@gen gen_id]) [@@deriving qcheck, show] *)

(* type id = (string[@gen gen_id]) [@@deriving qcheck, show] *)
(* type id = Ast.id = string[@gen gen_id] [@@deriving qcheck, show] *)

(* type rec_flag = Ast.rec_flag =
  | Recursive
  | Non_recursive
[@@deriving qcheck, show]

type pattern = Ast.pattern =
  | PVar of id
  | PConst of const
  | PAny
[@@deriving qcheck, show]

let gen_limited_pattern_list max_size =
  let open Gen in
  let size = int_range 0 max_size in
  (* The list will have between 0 and max_size elements *)
  list_size size gen_pattern (* Use gen_pattern to generate the elements of the list *)
;;

type expr = Ast.expr =
  | Econst of const
  | Evar of id
  | Eif_then_else of expr * expr * expr option
  | Eoption of expr option
  | Etuple of expr * expr * expr list
  | Elist of expr list
  (* [@gen gen_limited_list gen_expr 5] *)
  | Ebin_op of bin_op * expr * expr
  | Ematch of expr * case * case list
  | Eun_op of un_op * expr
  | Elet of rec_flag * value_binding * value_binding list * expr
  | Efun_application of expr * expr
  | Efun of pattern list * expr (* [@gen gen_limited_pattern_list 3] *)
[@@deriving qcheck, show]

and case =
  { left : pattern
  ; right : expr
  }
[@@deriving show { with_path = false }, qcheck]

and value_binding = Ast.value_binding = Evalue_binding of id * expr
[@@deriving qcheck, show]

type structure_item = Ast.structure_item =
  | SEval of expr
  | SValue of rec_flag * value_binding * value_binding list * expr
[@@deriving qcheck, show]

type structure = structure_item list [@@deriving qcheck, show] *)
(* end :
   sig
   val gen_const : Ast.const QCheck.Gen.t
   val gen_bin_op : Ast.bin_op QCheck.Gen.t
   val gen_un_op : Ast.un_op QCheck.Gen.t

   (* val gen_id : Ast.id QCheck.Gen.t *)
   val gen_rec_flag : Ast.rec_flag QCheck.Gen.t
   val gen_pattern : Ast.pattern QCheck.Gen.t
   val gen_expr : Ast.expr QCheck.Gen.t
   val gen_value_binding : Ast.value_binding QCheck.Gen.t
   val gen_structure_item : Ast.structure_item QCheck.Gen.t
   val gen_structure : Ast.structure QCheck.Gen.t
   end) *)

(* let run_auto () =
   QCheck_base_runner.run_tests
   [
      QCheck.Test.make
        (* arbitrary_structure *)
        (* (fun structure -> *)
          (* Result.ok structure = parse_expr (Format.asprintf "%a" prpr_structure structure) *)
          (* match parse_expr (Format.asprintf "%a" prpr_structure structure) with
          | Result.Ok structure-> 
          | _ *)
          (* Add a meaningful property here *)
          (* List.length structure >= 0 *)
          (* ); *)
          
    ] *)

(*------------------Shrinker-----------------*)
(*
let rec shrink_expr = function
  | Econst (Int _) -> Iter.return (Econst (Int 1))
  | Econst (Bool b) -> Iter.return (Econst (Bool b))
  | Econst _ -> Iter.empty
  | Evar _ -> Iter.return (Evar "a")
  | Ebin_op (op, e1, e2) ->
    Iter.(
      return e1
      <+> return e2
      <+> map (fun e1' -> Ebin_op (op, e1', e2)) (shrink_expr e1)
      <+> map (fun e2' -> Ebin_op (op, e1, e2')) (shrink_expr e2)
      <+> return (Ebin_op (Add, e1, e2)))
  | Eun_op (_, e) ->
    Iter.(return e <+> map (fun e' -> Eun_op (Negative, e')) (shrink_expr e))
  | Eif_then_else (cond_e, then_e, Some else_e) ->
    Iter.(
      return then_e
      <+> return else_e
      <+> map
            (fun cond_e' -> Eif_then_else (cond_e', then_e, Some else_e))
            (shrink_expr cond_e)
      <+> map
            (fun then_e' -> Eif_then_else (cond_e, then_e', Some else_e))
            (shrink_expr then_e)
      <+> map
            (fun else_e' -> Eif_then_else (cond_e, then_e, Some else_e'))
            (shrink_expr else_e))
  | Eif_then_else (cond_e, then_e, None) ->
    Iter.(
      return then_e
      <+> map (fun cond_e' -> Eif_then_else (cond_e', then_e, None)) (shrink_expr cond_e)
      <+> map (fun then_e' -> Eif_then_else (cond_e, then_e', None)) (shrink_expr then_e))
  | Eoption (Some e) -> shrink_expr e
  | Eoption None -> Iter.empty
  | Elist es ->
    Iter.(
      (*removing elements from the list *)
      let shrink_list_length =
        map (fun es' -> Elist es') (QCheck.Shrink.list ~shrink:shrink_expr es)
      in
      (* shrink each element within the list *)
      let shrink_elements =
        List.fold_right
          (fun e acc ->
            map
              (fun e' -> Elist (List.map2 (fun x y -> if x = e then e' else y) es es))
              (shrink_expr e)
            <+> acc)
          es
          empty
      in
      shrink_list_length <+> shrink_elements)
  | Etuple (e1, e2, e3) ->
    Iter.(
      map (fun e1' -> Etuple (e1', e2, e3)) (shrink_expr e1)
      <+> map (fun e2' -> Etuple (e1, e2', e3)) (shrink_expr e2)
      <+> map
            (fun e3' -> Etuple (e1, e2, e3'))
            (QCheck.Shrink.list ~shrink:shrink_expr e3))
  | Elet (flag, vb, vb_l, e) ->
    Iter.(
      let shrink_value_binding_length =
        map (fun vb_l' -> Elet (flag, vb, vb_l', e)) (QCheck.Shrink.list vb_l)
      in
      return e
      <+> map (fun e' -> Elet (flag, vb, vb_l, e')) (shrink_expr e)
      <+> shrink_value_binding_length)
  | Efun (pattern, patterns, body) ->
    Iter.(
      let shrink_patterns_length =
        map
          (fun patterns' -> Efun (pattern, patterns', body))
          (QCheck.Shrink.list patterns)
      in
      map (fun body' -> Efun (pattern, patterns, body')) (shrink_expr body)
      <+> shrink_patterns_length)
  | Efun_application (e1, e2) ->
    Iter.(
      return e1
      <+> return e2
      <+> map (fun e1' -> Efun_application (e1', e2)) (shrink_expr e1)
      <+> map (fun e2' -> Efun_application (e1, e2')) (shrink_expr e2))
  | Ematch (e, case, case_l) ->
    Iter.(
      let shrink_cases_length =
        map (fun cases' -> Ematch (e, case, cases')) (QCheck.Shrink.list case_l)
      in
      map (fun e' -> Ematch (e', case, case_l)) (shrink_expr e) <+> shrink_cases_length)
;;

let shrink_structure_item = function
  | SEval e -> Iter.(map (fun e' -> SEval e') (shrink_expr e))
  | SValue (r, vb, vb_l, e) ->
    (* Iter.(return e1 <+> return e2 <+> shrink_expr e1 <+> shrink_expr e2) *)
    Iter.(
      let shrink_value_binding_length =
        map (fun vb_l' -> SValue (r, vb, vb_l', e)) (QCheck.Shrink.list vb_l)
      in
      map (fun e' -> SValue (r, vb, vb_l, e')) (shrink_expr e)
      <+> shrink_value_binding_length)
;;

let shrink_structure structure : structure Iter.t =
  match structure with
  | [] -> Iter.empty
  | _ ->
    (* Iter.(return structure <+> return rest <+> (shrink_structure_item str_item ):: rest) *)
    Iter.(
      let shrink_elements =
        List.fold_right
          (fun e acc ->
            map
              (fun e' ->
                List.map2 (fun x y -> if x = e then e' else y) structure structure)
              (shrink_structure_item e)
            <+> acc)
          structure
          empty
      in
      QCheck.Shrink.list structure <+> shrink_elements)
;;

let arbitrary_structure =
  (* QCheck.make gen_structure ~print:(fun s -> Format.asprintf "%a" show_structure s) *)
  make gen_structure ~print:(Format.asprintf "%a" prpr_structure) ~shrink:shrink_structure
;;

let arbitrary_expr =
  make gen_expr ~print:(Format.asprintf "%a" pp_expr) ~shrink:shrink_expr
;;

(* let run_auto n =
   (* let open QCheck_base_runner in
   let ppb = *)
   Test.make arbitrary_structure ~count:n (fun structure ->
   (* Convert structure to a string *)
   let structure_str = Format.asprintf "%a" prpr_structure structure in
   (* Try parsing the string back to a structure *)
   match parse_expr structure_str with
   | Result.Ok parsed_structure ->
   (* Check if the parsed structure matches the original one *)
   parsed_structure = structure
   | Result.Error _ ->
   (* If parsing fails, the test fails *)
   false
   )
   ;; *)
let run_auto n =
  QCheck_base_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_structure ~count:n (fun structure ->
          Result.ok structure = parse_expr (Format.asprintf "%a" prpr_structure structure)))
    ]
;;

(* in
   ignore (QCheck_base_runner.run_tests ~verbose:true [ ppb ]) *)

let test_structure_generation =
  (* let open QCheck in *)
  Test.make
    ~name:"Generate structure list"
    ~count:1
    (QCheck.make gen_structure)
    (fun structure ->
       Printf.printf "Generated structure: %s\n" (Ast.show_structure structure);
       true)
;;

let test_expr_generation =
  Test.make ~name:"Generate expressions\n" ~count:1 arbitrary_expr (fun expr ->
    Printf.printf "Generated expression: %s\n" (Ast.show_expr expr);
    true)
;;

let iter_to_list iter =
  let result = ref [] in
  iter (fun x -> result := x :: !result);
  (* Apply the iterator and collect values *)
  List.rev !result
;;

let arbitrary_expr = make gen_expr ~print:(Format.asprintf "%a" pp_expr)

let test_shrinking_expr =
  Test.make ~name:"Test shrinking" ~count:1 arbitrary_expr (fun expr ->
    let shrunk_exprs = iter_to_list (shrink_expr expr) in
    match shrunk_exprs with
    | [] -> true (* No shrinking happened, which is valid *)
    | shrunk_exprs ->
      (* Check if all shrunk expressions are smaller or equal in size to the original *)
      List.for_all
        (fun shrunk_expr ->
          (* Printf.printf "shrunk_expr: %s\n" (shrunk_expr); *)
          (* let shrunk_size = complexity shrunk_expr in *)
          Printf.printf "expr: %s\n" (show_expr expr);
          Printf.printf "shrunk_expr: %s\n" (show_expr shrunk_expr);
          true
          (* Printf.printf "original_size: %d\n" (original_size); *)
          (* Printf.printf "shrunk_size: %d\n" (shrunk_size); *)
          (* shrunk_size <= original_size) *))
        shrunk_exprs)
;;


*)