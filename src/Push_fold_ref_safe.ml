let[@inline] bracket ~(init : unit -> 'r) ~(stop : 'r -> 'b) (f : 'r -> 'r) : 'b
    =
  let acc = init () in
  try
    let acc' = f acc in
    stop acc'
  with exn ->
    let _acc' = stop acc in
    raise exn

type +'a t = {
  fold : 'r. push:('r -> 'a -> 'r) -> init:(unit -> 'r) -> full:bool ref -> 'r;
}
[@@unboxed]

let[@inline] unfold s0 next =
  let[@inline] fold ~push ~init ~full =
    let r = ref (init ()) in
    let s = ref (next s0) in
    while Option.is_some !s && not !full do
      let x, s' = Option.get !s in
      s := next s';
      r := push !r x
    done;
    !r
  in
  { fold }

let[@inline] ( -- ) i j =
  unfold i (fun [@inline] x -> if x = j then None else Some (x, x + 1))

let[@inline] flat_map f self =
  let fold ~push ~init ~full =
    let push' r x =
      let inner = f x in
      inner.fold ~push ~init:(fun () -> r) ~full
    in
    self.fold ~push:push' ~init ~full
  in
  { fold }

let[@inline] map f self =
  let fold ~push ~init ~full =
    let push' acc x = push acc (f x) in
    self.fold ~push:push' ~init ~full
  in
  { fold }

let[@inline] filter pred self =
  let fold ~push ~init ~full =
    let push' r x = if pred x then push r x else r in
    self.fold ~push:push' ~init ~full
  in
  { fold }

let[@inline] take n self =
  let fold ~push ~init ~full =
    let i = ref 0 in
    let push' r x =
      incr i;
      push r x
    in
    let full' = ref (!i = n || !full) in
    self.fold ~push:push' ~init ~full:full'
  in
  { fold }

let[@inline] fold push init self =
  self.fold ~push ~init:(fun () -> init) ~full:(ref false)
