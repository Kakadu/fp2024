let _1 = fun x y (a, _) -> (x + y - a) = 1
let _2 =
    let x, Some f = 1, Some ( ( + ) 4 )
    in f x

let _3 =  Some (1, "hi")

let _4 = let rec f x = f 5 in f

let int_of_option = function Some x -> x | None -> 0

let _42 = function 42 -> true | _ -> false