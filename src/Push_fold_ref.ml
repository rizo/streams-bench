type +'a t = { fold : 'r. 'r -> ('r -> 'a -> 'r) -> bool ref -> 'r } [@@unboxed]

let[@inline] unfold s0 next =
  let[@inline] fold init step full =
    let s = ref (next s0) in
    let r = ref init in
    while Option.is_some !s && not !full do
      let x, s' = Option.get !s in
      s := next s';
      r := step !r x
    done;
    !r
  in
  { fold }

let[@inline] ( -- ) i j =
  unfold i (fun [@inline] x -> if x = j then None else Some (x, x + 1))

let[@inline] flat_map f self =
  let fold init step full =
    let step' r x =
      let inner = f x in
      inner.fold r step full
    in
    self.fold init step' full
  in
  { fold }

let[@inline] map f self =
  let fold init step full =
    let step' acc x = step acc (f x) in
    self.fold init step' full
  in
  { fold }

let[@inline] filter pred self =
  let fold init step full =
    let step' r x = if pred x then step r x else r in
    self.fold init step' full
  in
  { fold }

let[@inline] take n self =
  let fold init step full =
    let i = ref 0 in
    let step' r x =
      incr i;
      step r x
    in
    let full' = ref (!i = n || !full) in
    self.fold init step' full'
  in
  { fold }

let[@inline] fold step init self = self.fold init step (ref false)
