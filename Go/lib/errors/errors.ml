(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

type type_check_error =
  | Check_failed (*Inner error to match errors inside typechecker*)
  | Incorrect_main of string (*No main/main with returns or args*)
  | Multiple_declaration of string (*Multiple declaration of ident*)
  | Undefined_ident of string (*No declaration of ident in current sapce*)
  | Mismatched_types of string (*Mismatched types in binoper/assign/return...*)
  (*Я НЕ СДЕЛАЛ ВЫВОД ТИПОВ КАК ПОПРОСИЛИ В РЕВЬЮ ПОТОМУ ЧТО Я УМИРАЮ ПЕРЕД ЗАЧЕТАМИ ПО МАТАНУ,
    ОСЯМ И ЗАЩИТОЙ ПОЩАДИТЕ МЕНЯ У МЕНЯ УЖЕ НЕТ НА ЭТО СИЛ, СДЕАЮ КАК-НИБУДЬ ПОСЛЕ ЗАЩИТЫ*)
  | Cannot_assign of string (*Error with assigning a multiple-return value*)
[@@deriving show { with_path = false }]

type error = Type_check_error of type_check_error
(* | Eval_error *)
[@@deriving show { with_path = false }]
