(* Streaming.Source *)

type +'a t = Iter : 's * ('s -> ('a * 's) option) -> 'a t

let singleton a =
  Iter
    ( false,
      function
      | false -> Some (a, true)
      | true -> None )

let take n (Iter (s0, next)) =
  let next' (s, i) =
    if i <= 0 then None
    else
      match next s with
      | Some (a, s') -> Some (a, (s', i - 1))
      | None -> None
  in
  Iter ((s0, n), next')

let filter p (Iter (s0, next)) =
  let next' s =
    let rec loop s =
      match next s with
      | Some (a, s') when p a -> Some (a, s')
      | Some (_, s') -> loop s'
      | None -> None
    in
    loop s
  in
  Iter (s0, next')

let map f (Iter (s0, next)) =
  let next' s =
    match next s with
    | Some (a, s') -> Some (f a, s')
    | None -> None
  in
  Iter (s0, next')

let unfold seed next = Iter (seed, next)

let of_list l =
  let next = function
    | [] -> None
    | x :: xs -> Some (x, xs)
  in
  Iter (l, next)

let fold f r0 (Iter (s0, next)) =
  let rec loop r s =
    match next s with
    | None -> r
    | Some (a, s') -> loop (f r a) s'
  in
  loop r0 s0

type ('a, 'b, 'top_st) flat_map_cursor =
  | Flat_map_cursor : {
      top : 'top_st;
      sub : 'sub_st;
      sub_next : 'sub_st -> ('b * 'sub_st) option;
    }
      -> ('a, 'b, 'top_st) flat_map_cursor

let flat_map f (Iter (state, next)) =
  let rec next' (Flat_map_cursor { top; sub; sub_next }) =
    match sub_next sub with
    | None -> begin
      match next top with
      | None -> None
      | Some (x, top') ->
        let (Iter (sub, sub_next)) = f x in
        next' (Flat_map_cursor { top = top'; sub; sub_next })
    end
    | Some (x, sub') -> Some (x, Flat_map_cursor { top; sub = sub'; sub_next })
  in
  Iter
    ( Flat_map_cursor { top = state; sub = (); sub_next = (fun _ -> None) },
      next' )

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
