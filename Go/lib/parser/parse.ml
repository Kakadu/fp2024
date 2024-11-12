(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

let parse parser str = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str
