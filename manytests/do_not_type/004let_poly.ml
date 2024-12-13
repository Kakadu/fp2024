let _1 =
  (fun f -> (f 1, f true)) (fun x -> x)

let _2 = function
  | Some f -> let _ = f "42" in f 42
  | None -> 1