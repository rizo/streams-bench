module Config = struct
  let length = int_of_float (2.0 ** 10.0)
  let limit = length / 2
end

module Streaming_stream = struct
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

  let flat_map f self =
    let reduce (Reducer k) =
      let step r x =
        (f x).reduce
          (Reducer { k with init = (fun () -> r); stop = (fun r -> r) })
      in
      self.reduce (Reducer { k with step })
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

  let take_mut n self =
    let reduce (Reducer k) =
      let i = ref 0 in
      self.reduce
        (Reducer
           {
             init = k.init;
             step =
               (fun r x ->
                 incr i;
                 k.step r x);
             full = (fun _ -> !i = n);
             stop = k.stop;
           })
    in
    { reduce }

  let take_pure n self =
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

  let take = take_mut

  let unfold_unsafe s0 next =
    let reduce (Reducer k) =
      let rec loop s r =
        if k.full r then r
        else
          match next s with
          | None -> r
          | Some (x, s') -> loop s' (k.step r x)
      in
      k.stop (loop s0 (k.init ()))
    in
    { reduce }

  let unfold_safe s0 next =
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

  let unfold = unfold_safe
  let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
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

let streaming_stream_test () =
  let open Streaming_stream in
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

let () =
  assert (iter_test () = streaming_stream_test ());
  let tests =
    [
      ("Streaming.Stream", Sys.opaque_identity streaming_stream_test, ());
      ("Iter", Sys.opaque_identity iter_test, ());
    ]
  in
  let results = (Sys.opaque_identity Benchmark.throughputN) ~repeat:1 3 tests in
  Benchmark.tabulate results
