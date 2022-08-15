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
  fold :
    'acc 'r.
    push:('acc -> 'a -> 'acc) ->
    init:(unit -> 'acc) ->
    full:bool ref ->
    stop:('acc -> 'r) ->
    'r;
}
[@@unboxed]

let[@inline] unfold s0 next =
  let[@inline] fold ~push ~init ~full ~stop =
    let acc = ref (init ()) in
    try
      let s = ref (next s0) in
      while Option.is_some !s && not !full do
        let x, s' = Option.get !s in
        s := next s';
        acc := push !acc x
      done;
      stop !acc
    with exn ->
      let _r = stop !acc in
      raise exn
  in
  { fold }

let[@inline] ( -- ) i j =
  unfold i (fun [@inline] x -> if x = j then None else Some (x, x + 1))

let[@inline] flat_map f self =
  let fold ~push ~init ~full ~stop =
    let push' acc x =
      let inner = f x in
      inner.fold ~push ~init:(fun () -> acc) ~full ~stop:(fun acc -> acc)
    in
    self.fold ~push:push' ~init ~full ~stop
  in
  { fold }

let[@inline] map f self =
  let fold ~push ~init ~full =
    let push' acc x = push acc (f x) in
    self.fold ~push:push' ~init ~full
  in
  { fold }

let[@inline] filter pred self =
  let fold ~push ~init ~full ~stop =
    let push' acc x = if pred x then push acc x else acc in
    self.fold ~push:push' ~init ~full ~stop
  in
  { fold }

let[@inline] take n self =
  let fold ~push ~init ~full ~stop =
    let i = ref 0 in
    let push' acc x =
      incr i;
      push acc x
    in
    let full' = ref (!i = n || !full) in
    self.fold ~push:push' ~init ~full:full' ~stop
  in
  { fold }

let[@inline] fold push init self =
  self.fold ~push
    ~init:(fun () -> init)
    ~full:(ref false)
    ~stop:(fun acc -> acc)
