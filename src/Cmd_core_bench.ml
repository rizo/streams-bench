open Core_bench.Std

include Cases

let () =
	let tests = List.map (fun (name, f) -> Bench.Test.create ~name (Sys.opaque_identity f)) current in
  Core.Command.run (Bench.make_command tests)

