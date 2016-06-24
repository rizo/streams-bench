
open Streams

let id x = x

let init_list n f =
  let rec loop acc i =
    if i = n then acc
    else loop (f i :: acc) (i + 1) in
  loop [] 0

let input_len       = 10_000
let input_array     = Array.init input_len id
let input_list      = init_list input_len id

let array () =
  let open Array in
  fold_left (+) 0 input_array

let list () =
  let open List in
  fold_left (+) 0 input_list

let coroutine () =
  let open Coroutine in
  fold (+) 0 (of_list input_list)

let fusion_stream () =
  let open Fusion_stream in
  fold (+) 0 (of_list input_list)

let generator () =
  let open Generator.B in
  fold (+) 0 (of_list input_list)

let iterators () =
  let open Iterator in
  fold (+) 0 (of_list input_list)

let partial_finite_klist () =
  let open Partial_finite_klist in
  fold (+) 0 (of_list input_list)

let sequence () =
  let open Sequence in
  fold (+) 0 (of_list input_list)

let total_finite_klist () =
  let open Total_finite_klist in
  fold (+) 0 (init input_len id)

let () =
  let open Core_bench.Std in
  Printf.printf "streams-bench: fold [%d]\n" input_len;
  let bench n = Bench.Test.create ~name:n in
  Core.Command.run (Bench.make_command [
      bench "array"                   array                  ;
      bench "list"                    list                   ;
      bench "coroutine"               coroutine              ;
      bench "fusion_stream"           fusion_stream          ;
      bench "generator"               generator              ;
      bench "iterators"               iterators              ;
      bench "partial_finite_klist"    partial_finite_klist   ;
      bench "sequence"                sequence               ;
      bench "total_finite_klist"      total_finite_klist     ;
    ])

