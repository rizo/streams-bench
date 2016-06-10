
module Base = struct
  type ('a, 's) iter = ('s * ('s -> ('a * 's) option))

  let take n ((init, next)) =
    let next' (i, s) =
      if i <= 0 then None
      else match next s with
        | Some (a, s') -> Some (a, (i - 1, s'))
        | None -> None in
    ((n, init), next')

  let count () =
    let next i = Some (i, i + 1) in
    (0, next)

  let range ?from:(start = 0) ?by:(step = 1) stop =
    let next i = Some (i, i + 1) in
    (start, next)

  let map f ((init, next)) =
    let next' s =
      match next s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    (init, next')

  let filter p ((init, next)) =
    let rec next' s =
      match next s with
      | Some (a, s') ->
        if p a then Some (a, s')
        else next s'
      | None -> None in
    (init, next')

  let fold f acc ((init, next)) =
    let rec loop acc s =
      match next s with
      | None -> acc
      | Some (a, s') -> loop (f acc a) s' in
    loop acc init

  let to_list ((s0, next)) =
    let rec loop acc s =
      match next s with
      | None -> List.rev acc
      | Some (x, s') -> loop (x :: acc) s' in
    loop [] s0

  let of_list l =
    let next = function
      | []    -> None
      | x::xs -> Some (x, xs) in
    (l, next)
end


module Fast = struct
  type ('a, 's) iter =
    ('s * ('s -> ('a * 's) option))

  let take n ((init, next)) =
    let next' (i, s) =
      match next s with
      | Some (a, s') -> Some (a, (i - 1, s'))
      | None -> None in
    ((n, init), next')

  let count () =
    let next i = Some (i, i + 1) in
    (0, next)

  let map f ((init, next)) =
    let next' s =
      match next s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    (init, next')

  let filter p ((init, next)) =
    let rec next' s =
      match next s with
      | Some (a, s') ->
        if p a then Some (a, s')
        else next s'
      | None -> None in
    (init, next')

  let fold f acc ((init, next)) =
    let rec loop acc s =
      match next s with
      | None -> acc
      | Some (a, s') -> loop (f acc a) s' in
    loop acc init
end

module K1 = struct

  let count k =
    let next i = Some (i, i + 1) in
    k (0, next)

  let map f (s0, next) k =
    let next' s =
      match next s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    k (s0, next')

  let filter p (init, next) k =
    let rec next' s =
      match next s with
      | Some (a, s') ->
        if p a then Some (a, s')
        else next s'
      | None -> None in
    k (init, next')

  let take n (init, next) k =
    let next' (i, s) =
      if i <= 0 then None
      else match next s with
        | Some (a, s') -> Some (a, (i - 1, s'))
        | None -> None in
    k ((n, init), next')

  let fold f acc (s0, next) =
    let rec loop acc s =
      match next s with
      | Some (a, s') -> loop (f acc a) s'
      | None -> acc in
    loop acc s0

  let (>>>) f x = f x

end


module K2 = struct

  let count k =
    let next i = Some (i, i + 1) in
    k 0 next

  let map f s0 next k =
    let next' s =
      match next s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    k s0 next'

  let filter p init next k =
    let rec next' s =
      match next s with
      | Some (a, s') ->
        if p a then Some (a, s')
        else next s'
      | None -> None in
    k init next'

  let take n init next k =
    let next' (i, s) =
      if i <= 0 then None
      else match next s with
        | Some (a, s') -> Some (a, (i - 1, s'))
        | None -> None in
    k (n, init) next'

  let fold f acc s0 next =
    let rec loop acc s =
      match next s with
      | Some (a, s') -> loop (f acc a) s'
      | None -> acc in
    loop acc s0

  let (>>>) f x = f x

end


module K3 = struct

  let count k =
    let next i = Some (i, i + 1) in
    k 0 next

  let map f s0 next k =
    let next' s =
      match next s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    k s0 next'

  let filter p init next k =
    let rec next' s =
      match next s with
      | Some (a, s') ->
        if p a then Some (a, s')
        else next s'
      | None -> None in
    k init next'

  let take n init next k =
    let next' (i, s) =
      if i <= 0 then None
      else match next s with
        | Some (a, s') -> Some (a, (i - 1, s'))
        | None -> None in
    k (n, init) next'

  let fold f acc s0 next =
    let rec loop acc s =
      let handle_next = function
        | Some (a, s') -> loop (f acc a) s'
        | None -> acc in
      next s handle_next in
    loop acc s0

  let (>>>) f x = f x

end

