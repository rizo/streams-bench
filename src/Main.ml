let ( = ) : int -> int -> bool = ( = )
let opaque = Sys.opaque_identity

module Config = struct
  let length = ref 10
  let limit = ref 5
end

(* Used as a validation bseline. Implemented as Pull_thunk_list. *)
let bench_all_stdlib_seq =
  let ( -- ) i j =
    Seq.unfold (fun x -> if x = j then None else Some (x, x + 1)) i
  in
  fun () ->
    let open Seq in
    0 -- !Config.length
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take !Config.limit
    |> flat_map (fun [@inline] x -> x -- (x + 30))
    |> fold_left ( + ) 0

(* See ppx_stage_strymonas/src/Strymonas_bench.ml *)
let bench_all_strymonas () =
  let length = !Config.length in
  let limit = !Config.limit in
  let s = ref 0 in
  (let s''1 = s in
   let s = ref (if 0 = length then None else Some (0, 0 + 1)) in
   let nr = ref limit in
   while !nr > 0 && !s <> None do
     match !s with
     | None -> assert false
     | Some (el, s') ->
       s := if s' = !Config.length then None else Some (s', s' + 1);
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

let bench_all_pull_cursor () =
  let open Streams_bench.Pull_cursor in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_pull_cursor_k () =
  let open Streams_bench.Pull_cursor_k in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_pull_cursor_safe () =
  let open Streams_bench.Pull_cursor_safe in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_pull_exn () =
  let open Streams_bench.Pull_exn in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_pull_option () =
  let open Streams_bench.Pull_option in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_pull_stream_fusion () =
  let open Streams_bench.Pull_stream_fusion in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_pull_thunk_list () =
  let open Streams_bench.Pull_thunk_list in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_bool () =
  let open Streams_bench.Push_bool in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_fold_k () =
  let open Streams_bench.Push_fold_k in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_fold_lazy () =
  let open Streams_bench.Push_fold_lazy in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_fold_ref () =
  let open Streams_bench.Push_fold_ref in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_fold_stop () =
  let open Streams_bench.Push_fold_stop in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_fold_thunk () =
  let open Streams_bench.Push_fold_thunk in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_reducer_bool () =
  let open Streams_bench.Push_reducer_bool in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_reducer_obj () =
  let open Streams_bench.Push_reducer_obj in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_reducer_stop () =
  let open Streams_bench.Push_reducer_stop in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let bench_all_push_unit () =
  let open Streams_bench.Push_unit in
  0 -- !Config.length
  |> map (fun x -> x + 1)
  |> filter (fun x -> x mod 3 = 0)
  |> take !Config.limit
  |> flat_map (fun [@inline] x -> x -- (x + 30))
  |> fold ( + ) 0

let benchmarks =
  [
    ( "all",
      [
        ("Strymonas", opaque bench_all_strymonas, ());
        ("Pull_cursor", opaque bench_all_pull_cursor, ());
        ("Pull_cursor_k", opaque bench_all_pull_cursor_k, ());
        ("Pull_cursor_safe", opaque bench_all_pull_cursor_safe, ());
        ("Pull_exn", opaque bench_all_pull_exn, ());
        ("Pull_option", opaque bench_all_pull_option, ());
        ("Pull_stream_fusion", opaque bench_all_pull_stream_fusion, ());
        ("Pull_thunk_list", opaque bench_all_pull_thunk_list, ());
        ("Pull_bool", opaque bench_all_push_bool, ());
        (* ("Pull_fold_k", opaque bench_all_push_fold_k, ()); *)
        (* ("Push_fold_lazy", opaque bench_all_push_fold_lazy, ()); *)
        ("Push_fold_ref", opaque bench_all_push_fold_ref, ());
        ("Push_fold_stop", opaque bench_all_push_fold_stop, ());
        (* ("Push_fold_thunk", opaque bench_all_push_fold_thunk, ()); *)
        ("Push_reducer_bool", opaque bench_all_push_reducer_bool, ());
        ("Push_reducer_stop", opaque bench_all_push_reducer_stop, ());
        ("Push_unit", opaque bench_all_push_unit, ());
      ] );
  ]

let () =
  begin
    let expected = bench_all_stdlib_seq () in
    assert (expected = bench_all_strymonas ());
    assert (expected = bench_all_pull_cursor ());
    assert (expected = bench_all_pull_cursor_k ());
    assert (expected = bench_all_pull_cursor_safe ());
    assert (expected = bench_all_pull_exn ());
    assert (expected = bench_all_pull_option ());
    assert (expected = bench_all_pull_stream_fusion ());
    assert (expected = bench_all_pull_thunk_list ());
    assert (expected = bench_all_push_bool ());
    (* assert (expected = bench_all_push_fold_k ()); *)
    (* assert (expected = bench_all_push_fold_lazy ()); *)
    assert (expected = bench_all_push_fold_ref ());
    assert (expected = bench_all_push_fold_stop ());
    (* assert (expected = bench_all_push_fold_thunk ()); *)
    assert (expected = bench_all_push_reducer_bool ());
    assert (expected = bench_all_push_reducer_obj ());
    assert (expected = bench_all_push_reducer_stop ());
    assert (expected = bench_all_push_unit ())
  end

let () =
  let cpu_process (b : Benchmark.t) = b.utime +. b.stime in
  let out_path =
    String.concat ""
      [
        "./results/result-";
        string_of_int (int_of_float (Unix.gettimeofday ()));
        ".csv";
      ]
  in
  let out = open_out out_path in
  List.iter
    (fun (benchmark_name, cases) ->
      Printf.fprintf out "benchmark,input_size,case,ops_per_sec\n%!";
      for pow = 1 to 6 do
        Config.length := int_of_float (float 10 ** float pow);
        Config.limit := !Config.length / 2;
        Printf.eprintf "-- Running benchmark %S with input %d\n%!"
          benchmark_name !Config.length;
        let results = (opaque Benchmark.throughputN) ~repeat:1 3 cases in
        List.iter
          (fun (case_name, vals) ->
            List.iter
              (fun (b : Benchmark.t) ->
                let ops_per_sec = Int64.to_float b.iters /. cpu_process b in
                Printf.fprintf out "%s,%d,%s,%f\n%!" benchmark_name
                  !Config.length case_name ops_per_sec)
              vals)
          results
      done)
    benchmarks;
  close_out out
