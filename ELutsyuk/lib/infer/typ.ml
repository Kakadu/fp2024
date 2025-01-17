(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Format

type binder = int [@@deriving show { with_path = false }]

type typ =
  (* интересно, что в одной из версий используется base_type в конструкторе, а здесь
     теперь юзается string, так легко расширять типы, не нужны никакие конструкторы типов *)
  | TypConst of string
  (* эт, кстати, кажется, те самые переменные типа из статьи *)
  | TypVar of binder
  | TypArrow of typ * typ
  | TypTuple of typ * typ * typ list
  | TypList of typ
  | TypOption of typ
[@@deriving show { with_path = false }]

type error = OccursCheckFailed of int * typ
