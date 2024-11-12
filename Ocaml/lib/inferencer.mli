type error =
  [ `No_variable of string
  | `Occurs_check
  | `Unapplicable_type of Ast.type_expr option * Typedtree.ty
  | `Unification_failed of Typedtree.ty * Typedtree.ty
  ]

val pp_error : Format.formatter -> error -> unit
val w : Ast.expr -> (Typedtree.ty, error) result
