
module List_view = struct
  let init l = l

  let view b f s =
    match s with
    | [] -> b
    | x :: s' -> f x s'

  let fold f b input =
    let rec go b s =
      view b (fun a s' -> go (f a b) s') s in
    go b (init input)
end

module Array_view = struct
  let init a = (a, 0)

  let view b f (a, i) =
    if i = Array.length a then
      b
    else
      f (Array.get a i) (a, i + 1)

  let fold f b input =
    let rec go b s =
      view b (fun a s' -> go (f a b) s') s in
    go b (init input)
end

