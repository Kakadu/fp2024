(** Pretty printer for base type *)
val pp_base_type_my : Format.formatter -> Types.base_type -> unit
(** Pretty printer for type *)
val pp_typ_my : Format.formatter -> Types.typ -> unit
(** Pretty printf for inference errors *)
val pp_error_my : Format.formatter -> Types.error -> unit
(** Print type *)
val print_typ : ?name:string -> Types.typ -> unit
