(** Copyright 2024, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Values types *)
type val_type =
  | ValInt of int (** Int value *)
  | ValChar of char (** Char value *)
  | ValNull (** Null *)
  | ValBool of bool (** Bool value *)
  | ValArray of val_type list (** TODO: array value *)
  | ValString of string (** TODO: string value *)
[@@deriving eq, show { with_path = false }]

(** Identidicator *)
type ident = Id of string [@@deriving eq, show { with_path = false }]

(** Basic types declarations *)
type base_type =
  | TypeInt (** Declaration of int *)
  | TypeChar (** Declaration of char *)
  | TypeBool (** Declaration of bool *)
  | TypeString (** Declaration of string TODO*)
[@@deriving eq, show { with_path = false }]
(* TODO: declaration of strings?? *)

(** Type delcaration *)
type _type =
  | TypeBase of base_type (** Declaration of basic type *)
  | TypeArray of base_type
  | TypeVoid (** Declaration of void TODO: remove by specification?? *)
  (** Declaration of array of basic type TODO: ranks & think about base_type *)
[@@deriving eq, show { with_path = false }]
(* TODO: records for arrays?? *)
(* TODO: strings "" *)

(** Variable *)
type var_type = TypeVar of _type [@@deriving eq, show { with_path = false }]
(* TODO: remove if needed - not implementing custom classes *)

(** Modifiers *)
type modifier =
  | MPublic (** Public modifier, used for main() method only  *)
  | MStatic (** Static modifier, used for main() method only *)
  | MConst (** Const modifier *) (* TODO *)
  | MAsync (** Async modifier *)
[@@deriving eq, show { with_path = false }]

type var_decl = Var of var_type * ident [@@deriving eq, show { with_path = false }]
type params = Params of var_decl list [@@deriving eq, show { with_path = false }]

(** Binary operations *)
type bin_op =
  | OpAdd (** Sum: a [+] b *)
  | OpSub (** a [-] b *)
  | OpMul (** a [*] b *)
  | OpDiv (** a [/] b in integers *)
  | OpMod (** a [%] b *)
  | OpEqual (** a [==] b *)
  | OpNonEqual (** a [!=] b *)
  | OpLess (** a [<] b *)
  | OpMore (** a [>] b *)
  | OpLessEqual (** a [<=] b *)
  | OpMoreEqual (** a [>=] b *)
  | OpAnd (** a [&&] b *)
  | OpOr (** a [||] b *)
  | OpAssign (** a [=] b *)
[@@deriving eq, show { with_path = false }]

(** Unary operations *)
type un_op =
  | OpInc (** [++] a or [++] a *) (* TODO remove?? *)
  | OpDec (** [--] a or [--] a *) (* TODO remove?? *)
  | OpNot (** [!] a *)
  | OpNew (** [new] a *)
[@@deriving eq, show { with_path = false }]

(** From clauses *)
type from_clause = FromClause of string * ident
[@@deriving eq, show { with_path = false }]

(** Language expressions *)
type expr =
  | EValue of val_type (** Some value *)
  | EBinOp of bin_op * expr * expr (** Binary operation *)
  | EUnOp of un_op * expr (** Unary operation *)
  | EConst of val_type (** Const expression TODO change for modifiers?? *)
  | EId of ident (** Identificator expression *)
  | EArrayAccess of expr * expr (** Array access: a = arr[i] *)
  | EFuncCall of expr * expr list
  (** Call of function: name(arguments) TODO: Program.x() *)
  | ELambda of expr * stmt (** Lambda expressions *)
  | EAwait of expr (** Await expression *)
  | ELinqQuery of linq_query (** from identifier in expr select expr *)
[@@deriving eq, show { with_path = false }]

(** Language statements *)
and stmt =
  | SFor of stmt option * expr option * expr option * stmt
  (** For cycle: [for] (int i = 0, j = 3; i < 4; i++, j--) \{\} *)
  | SIf of expr * stmt * stmt option
  (** If condition: [if] (a) [then] \{ b \} ([else] \{ c \} ) *)
  | SWhile of expr * stmt (** While cycle: [while] (a) \{ \} *)
  | SReturn of expr option (** Return: return (a) *)
  | SBlock of stmt list (** Block of statements: \{ a \}; could be empty: \{\} *)
  | SBreak (** Cycle break *)
  | SContinue (** Cycle continue *)
  | SExpr of expr (** Another expression *)
  | SDecl of var_decl * expr option (** Var declaration *)
[@@deriving eq, show { with_path = false }]

(** Select clause *)
and select_clause = SelectClause of expr [@@deriving eq, show { with_path = false }]

(** LINQ query *)
and linq_query =
  | SQuery of from_clause * select_clause (** from identifier in identifier select expr *)
[@@deriving eq, show { with_path = false }]

(** C Sharp class fields *)
type field =
  | VarField of modifier list * var_type * ident * expr option
  (** Class field - always initialized *)
  | Method of modifier list * _type * ident * params * stmt (** Class method *)
[@@deriving eq, show { with_path = false }]

(** C Sharp class *)
type c_sharp_class =
  | Class of modifier list * ident * field list (** Basic class (Program) name *)
[@@deriving eq, show { with_path = false }]

(** Program AST *)
type program = Program of c_sharp_class [@@deriving eq, show { with_path = false }]

(* TODO: read specification!! + write factorial parser from scratch *)
