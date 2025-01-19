fac_cps n k = if n==1 then k 1 else fac_cps (n-1) (\p -> k (p*n))

main = seq (print_int (fac_cps 4 (\ print_int -> print_int))) 0