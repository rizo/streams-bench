
include Streams_bench.Cases

let () =
  let tests = List.map (fun (name, f) -> name, Sys.opaque_identity f, ()) current in
  let results = (Sys.opaque_identity Benchmark.throughputN) ~repeat:1 3 tests in
  Benchmark.tabulate results

