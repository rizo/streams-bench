module Mk_main (Stream : Types.Stream) = struct
  let ( -- ) i j =
    Stream.unfold i (fun x -> if x = j then None else Some (x, x + 1))

  let all () =
    let open Stream in
    0 -- !Config.length
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take !Config.limit
    |> flat_map (fun x -> x -- (x + 30))
    |> fold ( + ) 0

  let all_no_flat_map () =
    let open Stream in
    0 -- !Config.length
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take !Config.limit
    |> fold ( + ) 0

  let all_no_take () =
    let open Stream in
    0 -- !Config.length
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 2 = 0)
    |> flat_map (fun x -> x -- (x + 30))
    |> fold ( + ) 0

  let fold () =
    let open Stream in
    0 -- !Config.length |> fold ( + ) 0

  let map () =
    let open Stream in
    0 -- !Config.length |> map (fun x -> x + 1) |> fold ( + ) 0

  let filter () =
    let open Stream in
    0 -- !Config.length |> filter (fun x -> x mod 2 = 0) |> fold ( + ) 0

  let flat_map () =
    let open Stream in
    0 -- !Config.length |> flat_map (fun x -> singleton x) |> fold ( + ) 0

  let take () =
    let open Stream in
    0 -- !Config.length |> take !Config.limit |> fold ( + ) 0

  let current =
    match !Config.main_ops with
    | "all" -> all
    | "all_no_flat_map" -> all_no_flat_map
    | "all_no_take" -> all_no_take
    | "fold" -> fold
    | "map" -> map
    | "filter" -> map
    | "flat_map" -> flat_map
    | "take" -> take
    | unknown -> failwith ("invalid main_ops value: " ^ unknown)
end

module Main = struct
  module Pull_cursor = Mk_main (Pull_cursor)
  module Pull_cursor_k = Mk_main (Pull_cursor_k)
  module Pull_cursor_safe = Mk_main (Pull_cursor_safe)
  module Pull_option = Mk_main (Pull_option)
  module Pull_stream_fusion = Mk_main (Pull_stream_fusion)
  module Pull_thunk_list = Mk_main (Pull_thunk_list)
  module Push_reducer_bool = Mk_main (Push_reducer_bool)
  module Push_reducer_stop = Mk_main (Push_reducer_stop)
  module Push_fold_stop = Mk_main (Push_fold_stop)
  module Push_fold_thunk = Mk_main (Push_fold_thunk)
  module Push_fold_lazy = Mk_main (Push_fold_lazy)
  module Push_unit = Mk_main (Push_unit)
end

let main_tests () =
  let x = Main.Push_unit.current () in
  begin
    assert (x = Main.Pull_cursor.current ());
    assert (x = Main.Pull_cursor_k.current ());
    assert (x = Main.Pull_cursor_safe.current ());
    assert (x = Main.Pull_option.current ());
    assert (x = Main.Pull_stream_fusion.current ());
    assert (x = Main.Pull_thunk_list.current ());
    assert (x = Main.Push_reducer_bool.current ());
    assert (x = Main.Push_reducer_stop.current ());
    assert (x = Main.Push_fold_thunk.current ());
    assert (x = Main.Push_fold_stop.current ());
    assert (x = Main.Push_fold_lazy.current ());
    assert (x = Main.Push_unit.current ())
  end

let main =
  [
    ("Streaming.Source", Main.Pull_cursor.current);
    ("Gen", Main.Pull_option.current);
    ("Base.Sequence", Main.Pull_stream_fusion.current);
    ("Stdlib.Seq", Main.Pull_thunk_list.current);
    ("Streaming.Stream", Main.Push_reducer_bool.current);
    ("Iter", Main.Push_unit.current);
    ("Fold_thunk", Main.Push_fold_thunk.current);
    ("Fold_lazy", Main.Push_fold_lazy.current);
  ]

let current = main
