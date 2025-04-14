(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Ast

let pp_list : 'a. (formatter -> 'a -> unit) -> string -> formatter -> 'a list -> unit =
  fun pp sep fmt lst ->
  let rec aux fmt = function
    | [] -> ()
    | [ x ] -> pp fmt x
    | x :: xs -> fprintf fmt "%a%s%a" pp x sep aux xs
  in
  aux fmt lst
;;

let pp_option : 'a. (formatter -> 'a -> unit) -> formatter -> 'a option -> unit =
  fun pp fmt -> function
  | None -> fprintf fmt ""
  | Some x -> pp fmt x
;;

let pp_ident fmt (Id s) = fprintf fmt "%s" s

let pp_base_type fmt = function
  | TypeInt -> fprintf fmt "int"
  | TypeChar -> fprintf fmt "char"
  | TypeBool -> fprintf fmt "bool"
  | TypeString -> fprintf fmt "string"
;;

let pp_type fmt = function
  | TypeBase bt -> pp_base_type fmt bt
  | TypeVoid -> fprintf fmt "void"
;;

let pp_var_type fmt (TypeVar t) = pp_type fmt t

let pp_modifier fmt = function
  | MPublic -> fprintf fmt "public"
  | MStatic -> fprintf fmt "static"
  | MAsync -> fprintf fmt "async"
;;

let pp_var_decl fmt (Var (vt, id)) = fprintf fmt "%a %a" pp_var_type vt pp_ident id

let pp_bin_op fmt = function
  | OpAdd -> fprintf fmt "+"
  | OpSub -> fprintf fmt "-"
  | OpMul -> fprintf fmt "*"
  | OpDiv -> fprintf fmt "/"
  | OpMod -> fprintf fmt "%%"
  | OpEqual -> fprintf fmt "=="
  | OpNonEqual -> fprintf fmt "!="
  | OpLess -> fprintf fmt "<"
  | OpMore -> fprintf fmt ">"
  | OpLessEqual -> fprintf fmt "<="
  | OpMoreEqual -> fprintf fmt ">="
  | OpAnd -> fprintf fmt "&&"
  | OpOr -> fprintf fmt "||"
  | OpAssign -> fprintf fmt "="
;;

let pp_un_op fmt = function
  | OpNot -> fprintf fmt "!"
;;

let pp_val_type fmt = function
  | ValInt n -> fprintf fmt "%d" n
  | ValChar c -> fprintf fmt "'%c'" c
  | ValNull -> fprintf fmt "null"
  | ValBool b -> fprintf fmt "%b" b
  | ValString s -> fprintf fmt {|%S|} s
;;

let rec pp_expr fmt = function
  | EValue v -> pp_val_type fmt v
  | EBinOp (op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_expr e1 pp_bin_op op pp_expr e2
  | EUnOp (op, e) -> fprintf fmt "(%a%a)" pp_un_op op pp_expr e
  | EId id -> pp_ident fmt id
  | EArrayAccess (e1, e2) -> fprintf fmt "%a[%a]" pp_expr e1 pp_expr e2
  | EFuncCall (e, args) -> fprintf fmt "%a(%a)" pp_expr e (pp_list pp_expr ", ") args
  | EAwait e -> fprintf fmt "await %a" pp_expr e
;;

let rec pp_stmt fmt = function
  | SFor (init, cond, incr, body) ->
    fprintf
      fmt
      "@[<v 4>for (%a; %a; %a) {@ %a@]@ }"
      (pp_option pp_stmt)
      init
      (pp_option pp_expr)
      cond
      (pp_option pp_expr)
      incr
      pp_stmt
      body
  | SIf (cond, then_branch, else_branch) ->
    fprintf
      fmt
      "@[<v 4>if (%a) {@ %a@]@ }%a"
      pp_expr
      cond
      pp_stmt
      then_branch
      (pp_option (fun fmt -> fprintf fmt "@ @[<v 4>else {@ %a@]@ }" pp_stmt))
      else_branch
  | SWhile (cond, body) ->
    fprintf fmt "@[<v 4>while (%a) {@ %a@]@ }" pp_expr cond pp_stmt body
  | SReturn e -> fprintf fmt "return %a;" (pp_option pp_expr) e
  | SBlock stmts -> pp_sblock fmt stmts
  | SBreak -> fprintf fmt "break;"
  | SContinue -> fprintf fmt "continue;"
  | SExpr e -> fprintf fmt "%a;" pp_expr e
  | SDecl (vd, e) -> fprintf fmt "%a = %a;" pp_var_decl vd (pp_option pp_expr) e

and pp_sblock fmt = function
  | [] -> fprintf fmt ""
  | stmts -> fprintf fmt "@[<v>%a@]" (pp_list pp_stmt "@ ") stmts
;;

let pp_field fmt = function
  | VarField (mods, t, id, e) ->
    fprintf
      fmt
      "@[<v 4>%a %a %a = %a;@]"
      (pp_list pp_modifier " ")
      mods
      pp_var_type
      t
      pp_ident
      id
      (pp_option pp_expr)
      e
  | Method (mods, t, id, Params params, body) ->
    fprintf
      fmt
      "@[<v 2>%a %a %a(%a)@ @[<v 4>{@ %a@]@ @[<v 2>}@]@ "
      (pp_list pp_modifier " ")
      mods
      pp_type
      t
      pp_ident
      id
      (pp_list pp_var_decl ", ")
      params
      pp_stmt
      body
;;

let pp_c_sharp_class fmt (Class (mods, id, fields)) =
  fprintf
    fmt
    "@[<v>%a class %a@ @[<v 4>{@ %a@]@ @[<v>}@]"
    (pp_list pp_modifier " ")
    mods
    pp_ident
    id
    (pp_list pp_field " ")
    fields
;;

let pp_prog fmt (Program cls) = pp_c_sharp_class fmt cls
