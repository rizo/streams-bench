let ( = ) : int -> int -> bool = ( = )

type 'a thunk = unit -> 'a
type 'a t = { run : 'r. ('r thunk -> 'a -> 'r) -> 'r thunk -> 'r } [@@unboxed]

let[@inline] filter p self =
  let run k r = self.run (fun r' x -> if p x then k r' x else r' ()) r in
  { run }

let[@inline] map f self =
  let run k r = self.run (fun r' x -> k r' (f x)) r in
  { run }

let[@inline] take_mut n { run } =
  let run k r =
    let i = ref 0 in
    let k' r' x =
      if !i = n then r ()
      else (
        incr i;
        k r' x)
    in
    run k' r
  in
  { run = (fun k r -> run k r) }

let[@inline] take_k n { run } =
  let run k r =
    let k' r' x i = if i = 1 then k r x else k (fun () -> (r' ()) (i - 1)) x in
    run k' (fun () _ -> r ()) n
  in
  { run = (fun k r -> run k r) }

let take = take_mut

let ints : int t =
  let rec loop n k r = k (fun () -> loop (n + 1) k r) n in
  { run = (fun k r -> loop 0 k r) }

let singleton x = { run = (fun k r -> k r x) }
let[@inline] fold f r seq = seq.run (fun r' x -> f (r' ()) x) (fun () -> r)

let[@inline] unfold seed next =
  let rec loop seed k r =
    match next seed with
    | None -> r ()
    | Some (x, seed') -> k (fun () -> loop seed' k r) x
  in
  { run = (fun k r -> loop seed k r) }

let[@inline] of_list list =
  let rec loop xs k r =
    match xs with
    | [] -> r ()
    | x :: xs' -> k (fun () -> loop xs' k r) x
  in
  { run = (fun k r -> loop list k r) }

let[@inline] flat_map f self =
  let run k r = self.run (fun r' x -> (f x).run k r') r in
  { run }
