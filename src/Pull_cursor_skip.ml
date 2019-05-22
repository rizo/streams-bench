
type 'a t =
    Iter : { init : 's;
              step : 'r. stop:'r -> skip:('s -> 'r) -> yield:('a -> 's -> 'r) -> 's -> 'r } -> 'a t

let map f (Iter i) =
  let step ~stop ~skip ~yield s =
    i.step ~stop ~skip ~yield:(fun a s' -> yield (f a) s') s in
  Iter { i with step }

let fold f acc (Iter i) =
  let rec loop acc' s =
    i.step s
      ~stop:acc'
      ~skip:(fun s' -> loop acc' s')
      ~yield:(fun a s' -> loop (f a acc') s') in
  loop acc i.init

let of_list l =
  let step ~stop ~skip:_ ~yield s =
    match s with
    | [] -> stop
    | a :: rest -> yield a rest in
  Iter { init = l; step }

let filter f (Iter i) =
  let step ~stop ~skip ~yield s =
    i.step s
      ~stop ~skip
      ~yield:(fun a s' ->
          if f a then yield a s'
          else skip s') in
  Iter { i with step }

let take n (Iter i) =
  let step ~stop ~skip ~yield (s, count) =
    i.step s
      ~stop ~skip:(fun s -> (skip (s, count)))
      ~yield:(fun a s' ->
          if count = n then stop
          else yield a (s', count + 1)) in
  Iter { init = (i.init, 0); step }

