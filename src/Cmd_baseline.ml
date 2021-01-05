let n = 1_000_000

module X_hlocal = struct
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

  let reduce r self =
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
end

let bench_iter () =
  let module S = Iter in
  let (--) = S.(--) in
  0 -- n
  |> S.map (fun x -> x + 1)
  |> S.filter (fun x -> x mod 3 = 0)
  |> S.take (n / 2)
  |> S.flat_map (fun x -> x -- (x + 30))
  |> S.fold (+) 0

let iter_local () =
  let module S = Streams_bench.Push_unit in
  let (--) = S.(--) in
  0 -- n
  |> S.map (fun x -> x + 1)
  |> S.filter (fun x -> x mod 3 = 0)
  |> S.take (n / 2)
  |> S.flat_map (fun x -> x -- (x + 30))
  |> S.fold (+) 0

let bench_stream () =
  let module S = Streaming.Stream in
  let (--) = S.(--) in
  0 -- n
  |> S.map (fun x -> x + 1)
  |> S.filter (fun x -> x mod 3 = 0)
  |> S.take (n / 2)
  |> S.flat_map (fun x -> x -- (x + 32))
  |> S.fold (+) 0

let bench_stream_local () =
  let module S = Streams_bench.Push_reducer_bool in
  let (--) = S.(--) in
  0 -- n
  |> S.map (fun x -> x + 1)
  |> S.filter (fun x -> x mod 3 = 0)
  |> S.take (n / 2)
  |> S.flat_map (fun x -> x -- (x + 30))
  |> S.fold (+) 0


let bench_stream_x_hlocal () =
  let open X_hlocal in
  0 -- n
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take (n / 2)
  |> flat_map (fun x -> x -- (x + 30))
  |> fold (+) 0


let bench_stream_x () =
  let open X in
  0 -- n
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take (n / 2)
  |> flat_map (fun x -> x -- (x + 30))
  |> fold (+) 0


let () =
  let x =  bench_iter () in
  assert (
    x = bench_stream_x_hlocal () &&
    x = bench_stream_x () &&
    x = bench_stream_local ()
  )

open Core_bench

let () =
  let tests = [
    Bench.Test.create ~name:"iter" (Sys.opaque_identity bench_iter);
    Bench.Test.create ~name:"stream_local" (Sys.opaque_identity bench_stream_local);
    Bench.Test.create ~name:"stream_x_hlocal" (Sys.opaque_identity bench_stream_x_hlocal);
    Bench.Test.create ~name:"stream_x" (Sys.opaque_identity bench_stream_x);
  ] in
  Core.Command.run (Bench.make_command tests)
