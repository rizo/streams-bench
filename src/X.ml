
  type ('a, 'b) reducer =
    Reducer : {
      init : unit -> 'acc;
      step : 'acc -> 'a -> 'acc;
      full : 'acc -> bool;
      stop : 'acc -> 'b
    } -> ('a, 'b) reducer

  type 'a t =
    { reduce : 'r . ('a, 'r) reducer -> 'r }
    [@@unboxed]

  let[@inline] reduce r self =
    self.reduce r

  let[@inline] fold step init =
    let[@inline] init () = init in
    let[@inline] stop x = x in
    let[@inline] full _ = false in
    reduce (Reducer {
        init;
        step;
        full;
        stop;
      })

  let[@inline] (--) n m =
    let[@inline] reduce (Reducer k) =
      let[@inline] rec loop i r =
        if k.full r then r
        else
          if i > m then r
          else loop (i + 1) (k.step r i) in
      k.stop (loop n (k.init ())) in
    { reduce }

  let map f self =
    let reduce (Reducer k) =
      let step acc x = k.step acc (f x) in
      self.reduce (Reducer { k with step }) in
    { reduce }

  let filter pred self =
    let reduce (Reducer k) =
      let step r x = if pred x then k.step r x else r in
      self.reduce (Reducer { k with step }) in
    { reduce }

  let flat_map f this =
    let reduce (Reducer k) =
      let step r x =
        (f x).reduce (Reducer { k with
            init = (fun () -> r);
            stop = (fun r -> r)
          }) in
      this.reduce (Reducer { k with step }) in
    { reduce }

  let take n self =
    let reduce (Reducer k) =
      self.reduce (Reducer {
          init = (fun () -> k.init (), 0);
          step = (fun (acc, i) x -> (k.step acc x, i + 1));
          full = (fun (_, i) -> i = n);
          stop = (fun (acc, _) -> k.stop acc);
        }) in
    { reduce }
