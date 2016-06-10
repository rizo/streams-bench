
module Fast = struct

  type 'a reduced = Continue of 'a | Done of 'a

  type ('a, 's) iterator = 's * ('s -> ('a * 's) option)

  type ('a, 'r, 's) reducer =
    {  init : 's; step : 's -> 'r -> 'a -> 's * 'r reduced }

  let compose g f =
    fun x -> f (g x)

  let (>>) g f = compose f g
  let (<<) f g = compose f g

  let stateless f = { init = (); step = fun () x y -> (), Continue (f x y) }

  let transduce xf f r0 (input, next) =
    let {init = s0; step} = xf (stateless f) in
    let rec loop s r input =
      match next input with
      | None -> (s, r)
      | Some (x, xs) ->
        begin match step s r x with
          | s, Done r     -> (s, r)
          | s, Continue r -> loop s r xs
        end in
    let (s, r) = loop s0 r0 input in
    r

  let map f reducer =
    let step s r x = reducer.step s r (f x) in
    {reducer with step}

  let filter p reducer =
    let step s r x =
          if p x then reducer.step s r x
          else (s, Continue r) in
    {reducer with step}

  let take n {init = s0; step = next} =
    let step (s, i) r a =
      if i >= n then
        ((s, i), Done r)
      else
        let s', r' = next s r a in
        ((s', i + 1), r') in
    { init = (s0, 0); step }

  (* Producers *)

  let iter_list input =
    let next l =
      match l with
      | []      -> None
      | x :: xs -> Some (x, xs) in
    (input, next)

  let iter_chan input =
    let next c =
      try Some (input_line c, c)
      with End_of_file -> None in
    (input, next)

  let count () =
    (0, fun i -> Some (i, i + 1))

  (* Consumers *)

  let sum xf iterator =
    transduce xf (+) 0 iterator

  let len xf iterator =
    transduce xf (fun r _ -> r + 1) 0 iterator

  let into_list l0 xf iterator =
    List.rev (transduce xf (fun r x -> x :: r) l0 iterator)

  let into_chan c0 xf iterator =
    let _ = (transduce xf (fun r x -> output_string r (x ^ "\n"); r) c0 iterator)
    in ()

end

