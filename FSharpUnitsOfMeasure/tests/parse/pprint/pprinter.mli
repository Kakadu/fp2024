(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

val pprint_ident : tag -> tag
val pprint_measure : measure -> tag
val pprint_uom : unit_of_measure -> tag
val pprint_const : constant -> tag
val pprint_type : core_type -> tag
val pprint_pat : pattern -> tag
val pprint_binds : val_binding list -> tag
val pprint_rules : rule list -> tag
val pprint_expr : expression -> tag
val pprint_struct_item : structure_item -> tag
val pprint_program : program -> tag
