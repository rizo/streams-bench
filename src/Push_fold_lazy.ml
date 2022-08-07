let ( = ) : int -> int -> bool = ( = )

type 'a t = { run : 'r. ('r lazy_t -> 'a -> 'r) -> 'r lazy_t -> 'r } [@@unboxed]

let[@inline] filter p self =
  let run k r =
    self.run (fun r' x -> if p x then k r' x else Lazy.force r') r
  in
  { run }

let[@inline] map f self =
  let run k r = self.run (fun r' x -> k r' (f x)) r in
  { run }

let[@inline] take_mut n { run } =
  let run k r =
    let i = ref 0 in
    let k' r' x =
      if !i = n then Lazy.force r
      else (
        incr i;
        k r' x)
    in
    run k' r
  in
  { run = (fun k r -> run k r) }

let[@inline] take_k n { run } =
  let run k r =
    let k' r' x i =
      if i = 1 then k r x else k (lazy ((Lazy.force r') (i - 1))) x
    in
    run k' (lazy (fun _ -> Lazy.force r)) n
  in
  { run = (fun k r -> run k r) }

let take = take_mut

let ints : int t =
  let rec loop n k r = k (lazy (loop (n + 1) k r)) n in
  { run = (fun k r -> loop 0 k r) }

let singleton x = { run = (fun k r -> k r x) }
let[@inline] fold f r seq = seq.run (fun r' x -> f (Lazy.force r') x) (lazy r)

let[@inline] unfold seed next =
  let rec loop seed k r =
    match next seed with
    | None -> Lazy.force r
    | Some (x, seed') -> k (lazy (loop seed' k r)) x
  in
  { run = (fun k r -> loop seed k r) }

let[@inline] of_list list =
  let rec loop xs k r =
    match xs with
    | [] -> Lazy.force r
    | x :: xs' -> k (lazy (loop xs' k r)) x
  in
  { run = (fun k r -> loop list k r) }

let[@inline] flat_map f self =
  let run k r = self.run (fun r' x -> (f x).run k r') r in
  { run }
