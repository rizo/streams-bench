let ( = ) : int -> int -> bool = ( = )


let (--) i j =
  Strymonas.unfold (fun x ->
    [%code if [%e x] = [%e j] then None else Some ([%e x], [%e x] + 1)]) i

let strymonas_test () =
  let open Strymonas in
  [%code 0] -- [%code 1024]
  |> map (fun x -> [%code [%e x] + 1])
  |> filter (fun x -> [%code [%e x] mod 3 = 0])
  |> take [%code 512]
  |> flat_map (fun x -> x -- [%code [%e x] + 30])
  |> fold (fun x y -> [%code [%e x] +  [%e y]]) [%code 0]


let () = 
  Format.printf "@[%a@]@." Ppx_stage.print (strymonas_test ())

