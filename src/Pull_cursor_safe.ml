open Local

type 'a t =
    Iter : {
      init : 'cursor;
      next : 'r . ('a -> 'cursor -> 'r) -> (unit -> 'r) -> 'cursor -> 'r;
      free : 'cursor -> unit
    } -> 'a t

let of_list list =
  let[@inline] next yield acc cursor =
    match cursor with
    | [] -> acc ()
    | a :: cursor' -> yield a cursor' in
  Iter {
    init = list;
    next;
    free = ignore
  }

let map f (Iter iter) =
  let[@inline] next yield acc cursor =
    iter.next (fun a cursor' -> yield (f a) cursor') acc cursor in
  Iter { iter with next }

let filter predicate (Iter iter) =
  let[@inline] rec next yield acc cursor =
    iter.next
      ((fun a cursor' ->
          if predicate a then yield a cursor'
          else next yield acc cursor'))
      acc cursor in
  Iter { iter with next }

let take n (Iter iter) =
  let[@inline] next yield acc (cursor, i) =
    if i <= 0 then acc ()
    else iter.next (fun a cursor' -> yield a (cursor', i - 1)) (acc) cursor in
  Iter { init = iter.init, n;
          free = (fun (s, _) -> iter.free s);
          next }

let fold f z (Iter iter) =
  let rec loop acc cursor =
    iter.next (fun a -> loop (f a acc)) (fun () -> acc) cursor
  in
  bracket (fun () -> iter.init) iter.free
    (loop z)
