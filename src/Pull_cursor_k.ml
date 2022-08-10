type ('a, 's) iter = {
  init : 's;
  next : 'r. ('a -> 's -> 'r) -> 's -> (unit -> 'r) -> 'r;
}

type 'a t = Iter : ('a, 's) iter -> 'a t [@@unboxed]

let of_list input =
  let next k s r =
    match s with
    | [] -> r ()
    | x :: s' -> k x s'
  in
  Iter { init = input; next }

let fold step r0 (Iter i) =
  let[@inline] rec go s r =
    i.next (fun x s' -> go s' (step r x)) s (fun () -> r)
  in
  go i.init r0

let singleton x =
  let next k s r = if s then r () else k x true in
  Iter { init = false; next }

let flat_map_mut f (Iter top) =
  let sub_state = ref (Iter { init = (); next = (fun _k _s r -> r ()) }) in
  let rec next k s r =
    let (Iter sub) = !sub_state in
    sub.next
      (fun x sub_s' ->
        sub_state := Iter { sub with init = sub_s' };
        k x s)
      sub.init
      (fun () ->
        top.next
          (fun x s' ->
            sub_state := f x;
            next k s' r)
          s r)
  in
  Iter { top with next }

let flat_map f (Iter top) =
  let rec next k (s, Iter sub) r =
    sub.next
      (fun x sub_s' -> k x (s, Iter { sub with init = sub_s' }))
      sub.init
      (fun () -> top.next (fun x s' -> next k (s', f x) r) s r)
  in
  let init = Iter { init = (); next = (fun _k _s r -> r ()) } in
  Iter { init = (top.init, init); next }

let count n =
  let next k s _ = k s (s + 1) in
  Iter { init = n; next }

let map f (Iter i) =
  let next k s r = i.next (fun x s' -> k (f x) s') s r in
  Iter { i with next }

let filter pred (Iter i) =
  let rec next k s r =
    i.next (fun x s' -> if pred x then k x s' else next k s' r) s r
  in
  Iter { i with next }

let take n (Iter i) =
  let next k (s, count) r =
    if count >= n then r () else i.next (fun a s' -> k a (s', count + 1)) s r
  in
  Iter { init = (i.init, 0); next }

let unfold seed next =
  let next k s r =
    match next s with
    | None -> r ()
    | Some (x, s') -> k x s'
  in
  Iter { init = seed; next }

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
