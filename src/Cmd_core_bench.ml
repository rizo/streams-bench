open Core_bench

open Streams_bench.Cases

let () =
  Streams_bench.Config.configure ();
  let tests = List.map (fun (name, f) -> Bench.Test.create ~name (Sys.opaque_identity f)) current in
  Core.Command.run (Bench.make_command tests)
