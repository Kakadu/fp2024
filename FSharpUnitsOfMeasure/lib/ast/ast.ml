(** Copyright 2024, Vlasenco Daniel and Strelnikov Andrew *)

(** SPDX-License-Identifier: MIT *)

open QCheck.Gen
open Keywords
open Base

let gen_id_first_char =
  frequency [ 1, char_range 'a' 'z'; 1, char_range 'A' 'Z'; 1, return '_' ]
;;

let gen_digit = char_range '0' '9'
let gen_id_char = frequency [ 1, gen_id_first_char; 1, gen_digit; 1, return '\'' ]

let gen_ident =
  let gen_name =
    let* fst = gen_id_first_char >|= fun c -> Char.to_string c in
    let* rest = string_size ~gen:gen_id_char (0 -- 4) in
    return (fst ^ rest)
  in
  gen_name
  >>= fun name ->
  if is_keyword name || is_builtin_type name then gen_name else return name
;;

let gen_type_ident =
  let gen_builtin_type_ident =
    frequency
      [ 1, return "int"
      ; 1, return "bool"
      ; 1, return "float"
      ; 1, return "char"
      ; 1, return "string"
      ]
  in
  gen_builtin_type_ident
;;

(* Small integers (0 <= |a| < 100) *)
let gen_small_pint = small_nat
let gen_small_nint = small_nat >>= fun n -> return (-n)

type integer_exp =
  | Pos_int_exp of (int[@gen gen_small_pint])
  (** Positive (or zero) integer exponent: [2] *)
  | Neg_int_exp of (int[@gen gen_small_nint])
  (** Negative (or zero) integer exponent: [-1] *)
[@@deriving qcheck, show { with_path = false }]

type measure_num =
  | Mnum_int of int (** Integer number in unit of measure *)
  | Mnum_float of float (** Real number in unit of measure *)
[@@deriving qcheck, show { with_path = false }]

type measure =
  | Measure_ident of (string[@gen gen_ident]) (** Measure identificator: [m] *)
  | Measure_prod of measure * measure (** Measure product: [sec * h], [kg m] *)
  | Measure_div of measure * measure (** Measure division: [m / sec] *)
  | Measure_pow of measure * integer_exp (** Measure to the integer power: [cm^3] *)
  | Measure_paren of measure (** Parentheses around measure: [(kg / m^3)] *)
[@@deriving qcheck, show { with_path = false }]

(** Unit of measure: [1<m>], [9.8<kg m / s>], [0.3<kg^3>] etc. *)
type unit_of_measure = Unit_of_measure of measure_num * measure
[@@deriving qcheck, show { with_path = false }]

type constant =
  | Const_int of int (** Integer constants: [1] *)
  | Const_float of float (** Float constants: [3.14], [1e+5], [5.9E-3f] *)
  | Const_bool of bool (** Boolean constants [true] and [false] *)
  | Const_char of char (** Char constants: ['a'] *)
  | Const_string of string (** String constants: ["foo"] *)
  | Const_unit_of_measure of unit_of_measure
  (** Units of measure constants: [5.0<cm>], [3<kg>] *)
[@@deriving qcheck, show { with_path = false }]

type core_type =
  | Type_ident of (string[@gen gen_type_ident]) (** Type identificator, such as [int] *)
  | Type_func of core_type * core_type (** Function type: [T1 -> T2] *)
  | Type_tuple of
      core_type
      * core_type
      * (core_type list[@gen small_list (gen_core_type_sized (n / 2))])
  (** [Type_tuple(T1, T2, [T3, ..., Tn])] represents:
      - [(T1 * T2)] when core_type list is []
      - [(T1 * T2 * T3 * ... * Tn)] when core_type list is Cons (A, B) *)
[@@deriving qcheck, show { with_path = false }]

type pattern =
  | Pattern_ident_or_op of (string[@gen gen_ident])
  (** Identificator or operation patterns: [x], [(+)] *)
  | Pattern_const of constant
  (** Constant patterns: [1], ['a'], ["foo"], [3.14], [5.0<cm>] *)
  | Pattern_wild (** Wildcard patterns [ _ ] *)
  | Pattern_typed of pattern * core_type (** Typed pattern [x : int] *)
  | Pattern_tuple of pattern * pattern * pattern list
  (** [Pattern_tuple(P1, P2, [P3, ..., Pn])] represents:
      - [(P1, P2)] when pattern list is []
      - [(P1, P2, P3, ..., Pn)] when pattern list is Cons (A, B) *)
  | Pattern_list of (pattern list[@gen small_list (gen_pattern_sized (n / 2))])
  (** List patterns: [P1, ..., Pn] *)
  | Pattern_or of pattern * pattern
  (** OR patterns represent multiple satisfying patterns in pattern matching: [P1 | P2] *)
[@@deriving qcheck, show { with_path = false }]

type rec_flag =
  | Nonrecursive (** For nonrecursive function declarations *)
  | Recursive (** For recursive function declarations *)
[@@deriving qcheck, show { with_path = false }]

(* ['expr] just to avoid recursion, needs custom generator and no [deriving qcheck] *)

(** [Bind(P, E)] represents [let P = E] or [let rec P = E] *)
type 'expr val_binding = Bind of pattern * 'expr [@@deriving show { with_path = false }]

let gen_val_binding gen_expr_sized n =
  let* pat = gen_pattern_sized (n / 2) in
  let* expr = gen_expr_sized (n / 2) in
  return (Bind (pat, expr))
;;

(* ['expr] just to avoid recursion, needs custom generator and no [deriving qcheck] *)

(** [Rule(P, E)] represents [P -> E] in pattern matching *)
type 'expr rule = Rule of pattern * 'expr [@@deriving show { with_path = false }]

let gen_rule gen_expr_sized n =
  let* pat = gen_pattern_sized (n / 2) in
  let* expr = gen_expr_sized (n / 2) in
  return (Rule (pat, expr))
;;

type expression =
  | Expr_const of constant
  (** Constant expressions: [1], ['a'], ["foo"], [3.14], [true], [5.0<cm>] *)
  | Expr_ident_or_op of (string[@gen gen_ident])
  (** Identificator or operation expressions: [x], [+] *)
  | Expr_typed of expression * core_type (** Typed expression: [x: int] *)
  | Expr_tuple of
      expression
      * expression
      * (expression list[@gen small_list (gen_expression_sized (n / 2))])
  (** [Expr_tuple(E1, E2, [E3, ..., En])] represents:
      - [(E1, E2)] when pattern list is []
      - [(E1, E2, E3, ..., En)] when pattern list is Cons (A, B) *)
  | Expr_list of (expression list[@gen small_list (gen_expression_sized (n / 2))])
  (** List expressions: [E1; ...; En] *)
  | Expr_lam of (pattern[@gen gen_pattern_sized (n / 2)]) * expression
  (** Anonimous functions: [Expr_lam(P, E)] represents [fun P -> E] *)
  | Expr_let of
      rec_flag
      * (expression val_binding[@gen gen_val_binding gen_expression_sized (n / 2)])
      * (expression val_binding list
        [@gen small_list (gen_val_binding gen_expression_sized (n / 2))])
      * (expression[@gen gen_expression_sized (n / 2)])
  (** [Expr_let(rec_flag, Bind(P1, E1), [Bind(P2, E2); ...; Bind(Pn, En)], E)] represents:
      - [let P1 = E1 in E] when val_binding list is [] and rec_flag is Nonrecursive
      - [let rec P1 = E1 in E] when val_binding list is [] and rec_flag is Recursive
      - [let P1 = E1 and ... and Pn = En in E] when val_binding list is Cons (A, B) and rec_flag is Nonrecursive
      - [let rec P1 = E1 and ... and Pn = En in E] when val_binding list is Cons (A, B) and rec_flag is Recursive *)
  | Expr_ifthenelse of expression * expression * expression option
  (** [if E1 then E2 else E3] *)
  | Expr_apply of expression * expression (** Application [E1 E2] *)
  | Expr_match of
      expression
      * (expression rule[@gen gen_rule gen_expression_sized (n / 2)])
      * (expression rule list[@gen small_list (gen_rule gen_expression_sized (n / 2))])
  (** [Expr_match(E, Rule(P1, E1), [Rule(P2, E2); ...; Rule(Pn, En)])] represents:
      - [match E with P1 -> E1] if rule list is []
      - [match E with P1 -> E1 | P2 -> E2 | ... | Pn -> En] if rule list is Cons (A, B) *)
  | Expr_function of
      (expression rule[@gen gen_rule gen_expression_sized (n / 2)])
      * (expression rule list[@gen small_list (gen_rule gen_expression_sized (n / 2))])
  (** [Expr_function(Rule(P1, E1), [Rule(P2, E2); ...; Rule(Pn, En)])] represents:
      - [function P1 -> E1] if rule list is []
      - [function P1 -> E1 | P2 -> E2 | ... | Pn -> En] if rule list is Cons (A, B) *)
[@@deriving qcheck, show { with_path = false }]

type type_def =
  | Measure_type_def of (string[@gen gen_ident]) * measure option
  (** Measure type definition:
      - [[<Measure>] type I] when measure is None
      - [[<Measure>] type I = M] when measure is Some M *)
[@@deriving qcheck, show { with_path = false }]

type structure_item =
  | Str_item_eval of expression
  (** Structure item which is single expression: [E] or [do E] *)
  | Str_item_def of rec_flag * expression val_binding * expression val_binding list
  (** [Str_item_def(rec_flag, (P1, E1), [(P2, E2); ...; (Pn, En)])] represents:
      - [let P1 = E1] when val_binding list is [] and rec_flag is Nonrecursive
      - [let rec P1 = E1] when val_binding list is [] and rec_flag is Recursive
      - [let P1 = E1 and ... and Pn = En] when val_binding list is Cons (A, B) and rec_flag is Nonrecursive
      - [let rec P1 = E1 and ... and Pn = En] when val_binding list is Cons (A, B) and rec_flag is Recursive *)
  | Str_item_type_def of type_def (** Structure item which is type definition *)
[@@deriving show { with_path = false }]

let gen_structure_item n =
  let gen_str_item_def_sized n =
    let* flag = frequency [ 5, return Nonrecursive; 1, return Recursive ] in
    let* bind_fst = gen_val_binding gen_expression_sized (n / 2) in
    let* bind_rest = small_list (gen_val_binding gen_expression_sized (n / 2)) in
    return (Str_item_def (flag, bind_fst, bind_rest))
  in
  frequency
    [ (1, gen_expression_sized (n / 2) >|= fun e -> Str_item_eval e)
    ; 1, gen_str_item_def_sized (n / 2)
    ]
;;

type program = structure_item list [@@deriving show { with_path = false }]

let gen_program n = list_size (1 -- 5) (gen_structure_item (n / 2))
