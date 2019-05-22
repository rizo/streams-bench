
let length = 100_000
let limit  = length / 2
let input  = Array.to_list (Array.init length (fun x -> x))

let () =
  Printf.printf "Input length = %d, limit = %d\n" length limit
