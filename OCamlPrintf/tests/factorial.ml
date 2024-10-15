(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
