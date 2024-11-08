module Nonnegative_integer : sig
  type t

  val of_string_opt : string -> t option
  val of_int : int -> t
  val to_string : t -> string
  val to_zarith_type : t -> Z.t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Nonnegative of Z.t

  let of_string_opt s =
    match String.get s 0 with
    | '0' .. '9' -> Some (Nonnegative (Z.of_string s))
    | _ -> None
  ;;

  let of_int x =
    let x = Z.of_int x in
    Nonnegative x
  ;;

  let to_string (Nonnegative x) = Z.to_string x
  let to_zarith_type (Nonnegative x) = x
  let pp f (Nonnegative x) = Z.pp_print f x
end

(* type integer = Z.t *)

type nonnegative_integer = Nonnegative_integer.t [@@deriving show { with_path = false }]
