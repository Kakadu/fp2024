let fmap f = function
  | None -> None
  | Some x -> Some (f x)

let a = fmap (fun x -> x + 2) None

let b = fmap (fun x -> x * 2) (Some 12)
