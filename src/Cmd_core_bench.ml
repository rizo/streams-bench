open Core_bench
open Streams_bench.Cases

let () =
  main_tests ();
  Streams_bench.Config.configure ();
  let tests =
    List.map
      (fun (name, f) -> Bench.Test.create ~name (Sys.opaque_identity f))
      current
  in
  Command_unix.run (Bench.make_command tests)
