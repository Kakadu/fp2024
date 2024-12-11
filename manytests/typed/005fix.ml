let rec fix f x = f (fix f) x

let fac self n = if n<=1 then 1 else n * self (n-1)

let main =
  let () = print_int (fix fac 6) in
  0

