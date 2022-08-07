let ( = ) : int -> int -> bool = ( = )

module Config = struct
  let length = 10240
  let limit = 5120
end

module Streaming_core = struct
  type +'a t = { fold : 'r. 'r -> ('r -> 'a -> 'r) -> bool ref -> 'r }
  [@@unboxed]

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
end

module Streaming_safe = struct
  let bracket ~(init : unit -> 'r) ~(stop : 'r -> 'b) (f : 'r -> 'r) : 'b =
    let acc = init () in
    try
      let acc' = f acc in
      stop acc'
    with exn ->
      let _acc' = stop acc in
      raise exn

  type ('a, 'b) reducer =
    | Reducer : {
        init : unit -> 'acc;
        step : 'acc -> 'a -> 'acc;
        full : 'acc -> bool;
        stop : 'acc -> 'b;
      }
        -> ('a, 'b) reducer

  type 'a t = { reduce : 'r. ('a, 'r) reducer -> 'r } [@@unboxed]

  let unfold s0 next =
    let reduce (Reducer k) =
      let rec loop s r =
        if k.full r then r
        else
          match next s with
          | None -> r
          | Some (x, s') -> loop s' (k.step r x)
      in
      bracket (loop s0) ~init:k.init ~stop:k.stop
    in
    { reduce }

  let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))

  let flat_map f self =
    let reduce (Reducer k) =
      let step r x =
        (f x).reduce
          (Reducer { k with init = (fun () -> r); stop = (fun r -> r) })
      in
      self.reduce (Reducer { k with step })
    in
    { reduce }

  let map f self =
    let reduce (Reducer k) =
      let step acc x = k.step acc (f x) in
      self.reduce (Reducer { k with step })
    in
    { reduce }

  let filter pred self =
    let reduce (Reducer k) =
      let step r x = if pred x then k.step r x else r in
      self.reduce (Reducer { k with step })
    in
    { reduce }

  let take n self =
    let reduce (Reducer k) =
      self.reduce
        (Reducer
           {
             init = (fun () -> (k.init (), 0));
             step = (fun (acc, i) x -> (k.step acc x, i + 1));
             full = (fun (_, i) -> i = n);
             stop = (fun (acc, _) -> k.stop acc);
           })
    in
    { reduce }

  let fold step init self =
    self.reduce
      (Reducer
         {
           init = (fun () -> init);
           step;
           full = (fun _ -> false);
           stop = (fun x -> x);
         })
end

module Iter = struct
  type 'a t = ('a -> unit) -> unit

  let map f seq k = seq (fun x -> k (f x))
  let filter p seq k = seq (fun x -> if p x then k x)

  exception ExitTake

  let take n seq k =
    let count = ref 0 in
    try
      seq (fun x ->
          if !count = n then raise_notrace ExitTake;
          incr count;
          k x)
    with ExitTake -> ()

  let fold f acc seq =
    let r = ref acc in
    seq (fun x -> r := f !r x);
    !r

  let flat_map f self k = self (fun a -> (f a) k)

  let unfold seed next k =
    let rec loop seed =
      match next seed with
      | None -> ()
      | Some (x, seed') ->
        k x;
        loop seed'
    in
    loop seed

  let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
end

let streaming_core_test () =
  let open Streaming_core in
  0 -- Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let streaming_safe_test () =
  let open Streaming_safe in
  0 -- Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take Config.limit
  |> flat_map (fun x -> x -- (x + 30))
  |> fold ( + ) 0

let iter_test () =
  let open Iter in
  0 -- Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take Config.limit
  |> flat_map (fun x -> x -- (x + 30))
  |> fold ( + ) 0

let strymons_test () =
  let s = ref 0 in
  (let s''1 = s in
   let s = ref (if 0 = Config.length then None else Some (0, 0 + 1)) in
   let nr = ref Config.limit in
   while !nr > 0 && !s <> None do
     match !s with
     | None -> assert false
     | Some (el, s') ->
       s := if s' = Config.length then None else Some (s', s' + 1);
       let t = el + 1 in
       if t mod 3 = 0 then (
         decr nr;
         let s = ref (if t = t + 30 then None else Some (t, t + 1)) in
         while !s <> None do
           match !s with
           | None -> assert false
           | Some (el, s') ->
             s := if s' = t + 30 then None else Some (s', s' + 1);
             s''1 := !s''1 + el
         done)
   done);
  !s

let () =
  let expected = iter_test () in
  assert (expected = streaming_core_test ());
  assert (expected = streaming_safe_test ());
  assert (expected = strymons_test ());
  let tests =
    [
      ("Streaming_core", Sys.opaque_identity streaming_core_test, ());
      ("Strymonas", Sys.opaque_identity strymons_test, ());
      ("Streaming_safe", Sys.opaque_identity streaming_safe_test, ());
      ("Iter", Sys.opaque_identity iter_test, ());
    ]
  in
  let results = (Sys.opaque_identity Benchmark.throughputN) ~repeat:2 3 tests in
  Benchmark.tabulate results
