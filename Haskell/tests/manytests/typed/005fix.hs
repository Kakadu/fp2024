fix f x = f (fix f) x

fac self n = if n<=1 then 1 else n * self (n-1)

main = let () = print_int (fix fac 6) in 0
