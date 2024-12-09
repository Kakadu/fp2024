(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let gen_id_name =
  let open QCheck.Gen in
  let varname =
    let first_char = oneof [ char_range 'a' 'z'; return '_' ] in
    let rest_char =
      frequency
        [ 26, char_range 'a' 'z'
        ; 26, char_range 'A' 'Z'
        ; 10, char_range '0' '9'
        ; 1, return '_'
        ; 1, return '\''
        ]
    in
    (* limit the total length to 15 characters *)
    let gen_rest = string_size ~gen:rest_char (int_range 1 14) in
    (* combine the first character with the generated rest part *)
    map2 (fun start rest -> String.make 1 start ^ rest) first_char gen_rest
  in
  let is_keyword = function
    | "let"
    | "in"
    | "fun"
    | "rec"
    | "if"
    | "then"
    | "else"
    | "true"
    | "false"
    | "Some"
    | "None"
    | "and"
    | "match"
    | "with" -> true
    | _ -> false
  in
  (* unallow varname same as keyword *)
  varname >>= fun name -> if is_keyword name then varname else return name
;;

type id = Id of string * string option [@@deriving show { with_path = false }]

let gen_id = QCheck.Gen.map (fun name -> Id (name, None)) gen_id_name

type const =
  | Int of (int[@gen QCheck.Gen.int_range 0 1000])
  | String of
      (string
      [@gen
        QCheck.Gen.(
          let printable_without_quotes_and_backslash =
            oneof
              [ char_range '\032' '!' (* characters before '"' *)
              ; char_range '#' '[' (* characters after '"' but before '\' *)
              ; char_range ']' '\126' (* characters after '\' *)
              ]
          in
          string_size ~gen:printable_without_quotes_and_backslash (0 -- 20))])
    (* remove double quote character and backslash *)
  | Bool of (bool[@gen QCheck.Gen.bool])
  | Unit
(* language constants of type int, string, bool, and unit respectively *)
[@@deriving show { with_path = false }, qcheck]

type bin_op =
  | Add (* addition of two ints *)
  | Mult (* multiplication of two ints *)
  | Sub (* subtraction of two ints *)
  | Div (* division of two ints *)
  | Gt (* greater than *)
  | Lt (* less than *)
  | Eq (* equal *)
  | Neq (* not equal *)
  | Gte (* greater than or equal *)
  | Lte (* less than or equal *)
  | And (* logical AND *)
  | Or (* logical OR *)
[@@deriving show { with_path = false }, qcheck]

type un_op =
  | Negative
  | Positive
  | Not
(* unary minus, logical NOT *)
[@@deriving show { with_path = false }, qcheck]

type rec_flag =
  | Recursive
  | Non_recursive
(* flag for let expressions *)
[@@deriving show { with_path = false }, qcheck]

let divisor = 30

type pattern =
  | PVar of id
  | PConst of const
  | PTuple of
      (pattern[@gen gen_pattern_sized (n / divisor)])
      * (pattern[@gen gen_pattern_sized (n / divisor)])
      * (pattern list
        [@gen QCheck.Gen.(list_size (0 -- 4) (gen_pattern_sized (n / divisor)))])
  | PAny (* wildcard pattern '_' *)
  | PList of
      (pattern list
      [@gen QCheck.Gen.(list_size (0 -- 4) (gen_pattern_sized (n / divisor)))])
[@@deriving show { with_path = false }, qcheck]

type expr =
  | Econst of const (* constants, e.g. 10, "meow", true *)
  | Evar of id (* identifiers, e.g. "x", "f"*)
  (* maybe later switch to id, or even now *)
  | Eif_then_else of
      (expr[@gen gen_expr_sized (n / divisor)])
      * (expr[@gen gen_expr_sized (n / divisor)])
      * (expr[@gen gen_expr_sized (n / divisor)]) option
  (* if E0 then E1 else E2; else expression is optional *)
  | Eoption of
      (expr[@gen gen_expr_sized (n / divisor)]) option (* option type, Some e, None *)
  | Etuple of
      (expr[@gen gen_expr_sized (n / divisor)])
      * (expr[@gen gen_expr_sized (n / divisor)])
      * (expr list[@gen QCheck.Gen.(list_size (0 -- 4) (gen_expr_sized (n / divisor)))])
  (* expressions (E0, .., En), n >= 2 *)
  (* or expr * expr * expr list, cause invariant n >= 2 *)
  | Elist of
      (expr list[@gen QCheck.Gen.(list_size (0 -- 4) (gen_expr_sized (n / divisor)))])
    (* expressions [E0; ..; En], n >= 0 *)
  | Ebin_op of
      bin_op
      * (expr[@gen gen_expr_sized (n / divisor)])
      * (expr[@gen gen_expr_sized (n / divisor)])
  (** match E with P1 -> E1 ... Pn -> Pn *)
  (* E0 bin_op E1, e.g. 1 + 3 *)
  | Ematch of
      (expr[@gen gen_expr_sized (n / divisor)])
      * (case[@gen gen_case_sized (n / divisor)])
      * (case list[@gen QCheck.Gen.(list_size (0 -- 4) (gen_case_sized (n / divisor)))])
  | Eun_op of un_op * (expr[@gen gen_expr_sized (n / divisor)])
    (* E0 un_op E1, e.g. Negative 2, Not true *)
  | Elet of
      rec_flag
      * (value_binding[@gen gen_value_binding_sized (n / divisor)])
      * (value_binding list
        [@gen QCheck.Gen.(list_size (0 -- 4) (gen_value_binding_sized (n / divisor)))])
      * (expr[@gen gen_expr_sized (n / divisor)])
  (* let (rec) P1 = E1 and P2 = E2 and ... and Pn = En in E, e.g. let x = 5 in x - 10 *)
  | Efun_application of
      (expr[@gen gen_expr_sized (n / divisor)])
      * (expr[@gen gen_expr_sized (n / divisor)])
    (* E0 E1, e.g. f x *)
  | Efun of
      (pattern[@gen gen_pattern_sized (n / divisor)])
      * (pattern list
        [@gen QCheck.Gen.(list_size (0 -- 4) (gen_pattern_sized (n / divisor)))])
      * (expr[@gen gen_expr_sized (n / divisor)])
(* anonymous functions, e.g. fun x y -> x + 1 - y, arguments num >= 1 *)
[@@deriving show { with_path = false }, qcheck]

and case =
  | Ecase of
      (pattern[@gen gen_pattern_sized (n / divisor)])
      * (expr[@gen gen_expr_sized (n / divisor)])
[@@deriving show { with_path = false }, qcheck]

and value_binding = Evalue_binding of id * (expr[@gen gen_expr_sized (n / divisor)])
[@@deriving show { with_path = false }, qcheck]

let gen_expr =
  QCheck.Gen.(
    let* n = small_nat in
    gen_expr_sized n)
;;

let gen_value_binding =
  QCheck.Gen.(
    let* n = small_nat in
    gen_value_binding_sized n)
;;

let gen_case =
  QCheck.Gen.(
    let* n = small_nat in
    gen_case_sized n)
;;

type structure_item =
  | SEval of expr
  | SValue of
      rec_flag
      * (value_binding[@gen gen_value_binding])
      * (value_binding list[@gen QCheck.Gen.(list_size (0 -- 4) gen_value_binding)])
    (* let (rec) P1 = E1 and P2 = E2 and ... and Pn = En e.g. let x = 5 *)
[@@deriving show { with_path = false }, qcheck]

type structure =
  (structure_item list[@gen QCheck.Gen.(list_size (1 -- 2) gen_structure_item)])
[@@deriving show { with_path = false }, qcheck]
