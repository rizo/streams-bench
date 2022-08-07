let length = ref (int_of_float (2.0 ** 10.0))
let limit = ref (!length / 2)
let main_ops = ref "all"

let configure () =
  begin
    try
      main_ops := Sys.getenv "STREAMS_BENCHMARK";
      length := int_of_string (Sys.getenv "STREAMS_LENGTH");
      limit := int_of_string (Sys.getenv "STREAMS_LIMIT")
    with Not_found ->
      print_endline
        "Missing environment variables for benchmark:\n\n\
        \  - \
         STREAMS_BENCHMARK=all|all_no_flat_map|all_no_take|fold|map|filter|flat_map|take\n\
        \  - STREAMS_LENGTH=int (input length)\n\
        \  - STREAMS_LIMIT=int (used for take)\n\n\
         Using defaults."
  end;
  Printf.printf "benchmark=%s length=%d limit=%d\n" !main_ops !length !limit
