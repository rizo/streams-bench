
type ('a, 's) step =
  | Done
  | Skip  of 's
  | Yield of 'a * 's

type ('a, 's) t = 's * ('s -> ('a, 's) step)

let fold f init ((seed, next)) =
  let rec loop seed v next f =
    match next seed with
    | Done         -> v
    | Skip  s      -> loop s v next f
    | Yield (a, s) -> loop s (f v a) next f in
  loop seed init next f

let unfold f init =
  let next s =
    match f s with
    | None -> Done
    | Some (a, s) -> Yield (a, s) in
  (init, next)

let of_list l =
  let next s =
    match s with
    | [] -> Done
    | x :: xs -> Yield (x, xs) in
  (l, next)

let map f ((seed, next)) =
  let next s =
    match next s with
    | Done         -> Done
    | Skip s       -> Skip s
    | Yield (a, s) -> Yield (f a, s) in
  (seed, next)

let filter p ((seed, next)) =
  let next s =
    match next s with
    | Done         -> Done
    | Skip s       -> Skip s
    | Yield (a, s) when p a -> Yield (a, s)
    | Yield (_, s) -> Skip s in
  (seed, next)

let take n ((s, next)) =
  if n < 0 then invalid_arg "take";
  let next (i, s) =
    if i >= n then Done
    else
      match next s with
      | Done         -> Done
      | Skip s       -> Skip (i, s)
      | Yield (a, s) -> Yield (a, (i + 1, s)) in
  ((0, s), next)

let count n =
  (n, fun n -> Yield (n, n + 1))

