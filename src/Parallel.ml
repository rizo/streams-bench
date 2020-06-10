open Streams_bench

module Slice_par = struct
  module Reducer = Push_reducer_bool.Reducer
  module Slice = Pull_index

  let a1M = Array.init 100_000_000 (fun x -> sqrt (float_of_int x) ** 2.0)

  let seq () =
    let input = Pull_index.of_array a1M in
    Slice.reduce ~parallel:false (Reducer.take 100_000 Reducer.sum) input

  let par () =
    let input = Pull_index.of_array a1M in
    Slice.reduce ~parallel:true (Reducer.take 100_000 Reducer.sum) input
end

let slice_par = [
  "seq", Slice_par.seq;
  "par", Slice_par.par;
]

let current = slice_par


let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let t' = Unix.gettimeofday () -. t in
  (t', res)

