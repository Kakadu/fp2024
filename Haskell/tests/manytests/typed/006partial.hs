foo b = if b then (\foo -> foo+2) else (\foo -> foo*10)

foo2 x = foo True (foo False (foo True (foo False x)))

main = let () = print_int (foo2 11) in 0
