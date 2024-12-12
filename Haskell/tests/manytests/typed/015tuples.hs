fix f x = f (fix f) x
map f p = let (a,b) = p in (f a, f b)
fixpoly l = fix (\self l -> map (\li x -> li (self l) x) l) l
feven p n = let (e, o) = p in if n == 0 then 1 else o (n - 1)
fodd p n = let (e, o) = p in if n == 0 then 0 else e (n - 1)
tie = fixpoly (feven, fodd)
meven n = if n == 0 then 1 else modd (n - 1); modd n = if n == 0 then 1 else meven (n - 1)
main = let () = print_int (modd 1) in let () = print_int (meven 2) in let (even,odd) = tie in let () = print_int (odd 3) in let () = print_int (even 4) in 0
