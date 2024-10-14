(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let fac =
  "fac n = if n < 0 then Nothing else Just (save_fac n) where save_fac y | y == 0  = 1 | \
   otherwise = y * save_fac (y - 1)"
;;

let out_term file s =
  Out_channel.with_open_text file (fun ch ->
    Format.fprintf (Format.formatter_of_out_channel ch) "%s" s;
    flush ch)
;;

let () = out_term "fac.txt" fac
