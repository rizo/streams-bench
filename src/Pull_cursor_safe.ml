let pass () = ()

let bracket init (stop : 'a -> unit) f =
  let x = init () in
  try
    let r = f x in
    stop x;
    r
  with exn ->
    stop x;
    raise exn

type 'a t =
  | Iter : {
      init : unit -> 's;
      next : 'r. ('a -> 's -> 'r) -> (unit -> 'r) -> 's -> 'r;
      stop : 's -> unit;
    }
      -> 'a t

let singleton x =
  let next yield r pushed = if pushed then r () else yield x true in
  Iter { init = (fun () -> false); next; stop = ignore }

let of_list l =
  let[@inline] next yield r s =
    match s with
    | [] -> r ()
    | a :: s' -> yield a s'
  in
  Iter { init = (fun () -> l); next; stop = ignore }

let unfold seed next_orig =
  let[@inline] next yield r s =
    match next_orig s with
    | None -> r ()
    | Some (a, s') -> yield a s'
  in
  Iter { init = (fun () -> seed); next; stop = ignore }

let filter predicate (Iter iter) =
  let[@inline] rec next yield r s =
    iter.next
      (fun a s' -> if predicate a then yield a s' else next yield r s')
      r s
  in
  Iter { iter with next }

let map f (Iter iter) =
  let[@inline] next yield r s = iter.next (fun a s' -> yield (f a) s') r s in
  Iter { iter with next }

let take n (Iter iter) =
  let[@inline] next yield r (s, i) =
    if i <= 0 then r () else iter.next (fun a s' -> yield a (s', i - 1)) r s
  in
  Iter
    {
      init = (fun () -> (iter.init (), n));
      stop = (fun (s, _) -> iter.stop s);
      next;
    }

let[@inline] fold f r0 (Iter iter) =
  let[@inline] rec loop r s =
    iter.next (fun a s' -> loop (f r a) s') (fun () -> r) s
  in
  bracket iter.init iter.stop (loop r0)

let empty =
  let next _yield r _cursor = r () in
  Iter { init = pass; next; stop = pass }

type ('a, 'cursor) flat_map_cursor =
  | Flat_map_cursor : {
      top_cursor : 'cursor;
      sub_cursor : 'sub_cursor;
      sub_next :
        'r. ('a -> 'sub_cursor -> 'r) -> (unit -> 'r) -> 'sub_cursor -> 'r;
      sub_stop : 'sub_cursor -> unit;
    }
      -> ('a, 'cursor) flat_map_cursor

let flat_map f (Iter iter) =
  let rec next yield r (Flat_map_cursor cursor) =
    cursor.sub_next
      (fun a sub_cursor' ->
        yield a (Flat_map_cursor { cursor with sub_cursor = sub_cursor' }))
      (fun () ->
        cursor.sub_stop cursor.sub_cursor;
        iter.next
          (fun a top_cursor' ->
            let (Iter sub_iter) = f a in
            let cursor' =
              Flat_map_cursor
                {
                  top_cursor = top_cursor';
                  sub_cursor = sub_iter.init ();
                  sub_next = sub_iter.next;
                  sub_stop = sub_iter.stop;
                }
            in
            next yield r cursor')
          r cursor.top_cursor)
      cursor.sub_cursor
  in
  let init () =
    let (Iter sub_iter) = empty in
    Flat_map_cursor
      {
        top_cursor = iter.init ();
        sub_cursor = sub_iter.init ();
        sub_next = sub_iter.next;
        sub_stop = sub_iter.stop;
      }
  in
  let stop (Flat_map_cursor { top_cursor; sub_cursor; sub_stop; _ }) =
    iter.stop top_cursor;
    sub_stop sub_cursor
  in
  Iter { init; stop; next }

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
