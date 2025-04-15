(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Prettyprinter
open C_sharp_strange_lib.Parser
open Format

let fact_str =
  {|
public class Program
{
    public int Factorial(int n)
    {
        if (n == 0)
        {
            return 1;
        }
        else
        {
            return n * Factorial(n - 1);
        }
    }

    public static void Main()
    {

    }
}

|}
;;

let fact_prog = parse_option parse_prog fact_str

let pretty_fact_str = function
  | Some x -> asprintf "%a" pp_prog x
  | None -> ""
;;

let parse_after_pp prog = parse_option parse_prog (pretty_fact_str prog)
let%test "Factorial pp" = parse_after_pp fact_prog = fact_prog

let cycles_str =
  {|
public class Program
{
    public int Cycles(int n, bool e, string x)
    {
        int x = 0;
        while (x < n)
        {
            if (x == -1)
            {
                break;
            }

            if (x == -2)
            {
                continue;
            }

            x = x + 1;
        }

        for (int i = 1; i < n; i++)
        {
            break;
        }

        for (;;)
        {
            break;
        }

        for (int i = 1;; i++)
        {
            break;
        }
    }

    public static void Main()
    {
        Cycles(5, true, "sample");
    }
}
|}
;;

let cycles_prog = parse_option parse_prog cycles_str
let%test "Cycles pp" = parse_after_pp cycles_prog = cycles_prog

let binops_prog =
  parse_option
    parse_prog
    {|

public class Program
{
    public int Binops(int n, bool e, string x)
    {
        int x_ = n;
        bool sample = !e || ((1 + 2 < 3 + 4) && (5 == 8));
        string e = x;
        char eeAe065ef = 'a';
        e = null;
        const int a = 1;
    }


    public static void Main()
    {
        Binops(5, true, "");
    }
}
|}
;;

let%test "Binops pp" = parse_after_pp binops_prog = binops_prog
