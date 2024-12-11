let _1 = fun x y (a, _) -> (x + y - a) = 1
let _2 =
    let x, Some f = 1, Some ( ( + ) 4 )
    in f x

let _3 =  Some (1, "hi")

let _4 =
    let a, _ = 1, 2, 3 in a

let int_of_option = function Some x -> x | None -> 0

let _5 = let rec f x = f 5 in f

let _42 = function 42 -> true | _ -> false