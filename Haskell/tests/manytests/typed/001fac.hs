fac n = if n<=1 then 1 else n * fac (n-1)

main = let () = print_int (fac 4) in 0