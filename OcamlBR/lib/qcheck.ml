(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open QCheck
open Ast
open Pr_printer
open Parser

(*------------------Generator-----------------*)
let gen_const =
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
       | 0 -> oneof [ map (fun c-> Econst c) gen_const; map (fun v -> Evar v) gen_id ]
       | n ->
         frequency
           [ 1, map (fun c -> Econst c) gen_const
           ; 1, map (fun v -> Evar v) gen_id
           ; ( 2
             , map3
                 (fun e1 e2 e3 -> Eif_then_else (e1, e2, Some e3))
                 (self (n / 2))
                 (self (n / 2))
                 (self (n / 2)) )
           ; 2, map (fun es -> Elist es) (list_size (int_bound 10) (self (n / 2)))
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
                 (fun id e1 e2 -> Elet (Recursive, id, e1, e2))
                 gen_id
                 (self (n / 2))
                 (self (n / 2)) )
           ; ( 1
             , map3
                 (fun id e1 e2 -> Elet (Non_recursive, id, e1, e2))
                 gen_id
                 (self (n / 2))
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

let gen_structure_item =
  let open Gen in
  frequency
    [ 1, map (fun e -> SEval e) gen_expr
    ; ( 2
      , gen_rec_flag
        >>= fun r ->
        gen_id
        >>= fun id ->
        gen_expr >>= fun e1 -> gen_expr >>= fun e2 -> return (SValue (r, id, e1, e2)) )
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
  (* | Eoption (Some e) -> shrink_expr e
     | Eoption None -> Iter.empty *)
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
  | Elet (flag, id, e1, e2) ->
    Iter.(
      return e2
      <+> map (fun e1' -> Elet (flag, id, e1', e2)) (shrink_expr e1)
      <+> map (fun e2' -> Elet (flag, id, e1, e2')) (shrink_expr e2))
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
  | _ -> Iter.empty
;;

let shrink_structure_item = function
  | SEval e -> Iter.(map (fun e' -> SEval e') (shrink_expr e))
  | SValue (r, id, e1, e2) ->
    (* Iter.(return e1 <+> return e2 <+> shrink_expr e1 <+> shrink_expr e2) *)
    Iter.(
      map (fun e1' -> SValue (r, id, e1', e2)) (shrink_expr e1)
      <+> map (fun e2' -> SValue (r, id, e1, e2')) (shrink_expr e2))
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
  QCheck.Test.check_exn
    (Test.make arbitrary_structure_manual (fun structure ->
       Result.ok structure = parse_expr (Format.asprintf "%a" prpr_structure structure)))
;;
