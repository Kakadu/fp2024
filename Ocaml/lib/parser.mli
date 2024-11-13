type error =
  [ `Parsing_Error of string
  | `Some_Error
  ]

val pp_error : Format.formatter -> error -> unit
val parse : string -> (Ast.expr, error) result
