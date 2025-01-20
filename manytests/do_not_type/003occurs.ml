let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f))
