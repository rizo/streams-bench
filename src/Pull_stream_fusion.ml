type ('a, 's) step = Done | Skip of 's | Yield of 'a * 's
type +_ t = Stream : 's * ('s -> ('a, 's) step) -> 'a t

let singleton x =
  Stream
    ( false,
      function
      | false -> Yield (x, true)
      | true -> Done )

let of_list l =
  let next s =
    match s with
    | [] -> Done
    | x :: xs -> Yield (x, xs)
  in
  Stream (l, next)

let fold f init (Stream (seed, next)) =
  let rec loop seed v next f =
    match next seed with
    | Done -> v
    | Skip s -> loop s v next f
    | Yield (a, s) -> loop s (f v a) next f
  in
  loop seed init next f

let map f (Stream (seed, next)) =
  let next s =
    match next s with
    | Done -> Done
    | Skip s -> Skip s
    | Yield (a, s) -> Yield (f a, s)
  in
  Stream (seed, next)

let filter p (Stream (seed, next)) =
  let next s =
    match next s with
    | Done -> Done
    | Skip s -> Skip s
    | Yield (a, s) when p a -> Yield (a, s)
    | Yield (_, s) -> Skip s
  in
  Stream (seed, next)

let take n (Stream (s, next)) =
  let next (i, s) =
    if i >= n then Done
    else
      match next s with
      | Done -> Done
      | Skip s -> Skip (i, s)
      | Yield (a, s) -> Yield (a, (i + 1, s))
  in
  Stream ((0, s), next)

let empty = Stream ((), fun () -> Done)

let flat_map f self =
  Stream
    ( (empty, self),
      fun (Stream (seed, next), rest) ->
        match next seed with
        | Done -> (
          match rest with
          | Stream (seed, next) -> (
            match next seed with
            | Done -> Done
            | Skip s -> Skip (empty, Stream (s, next))
            | Yield (a, s) -> Skip (f a, Stream (s, next))))
        | Skip s -> Skip (Stream (s, next), rest)
        | Yield (a, s) -> Yield (a, (Stream (s, next), rest)) )

let unfold seed next =
  Stream
    ( seed,
      fun seed ->
        match next seed with
        | None -> Done
        | Some (a, s) -> Yield (a, s) )

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
