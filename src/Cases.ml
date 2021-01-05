
module Mk_main(Stream : Types.Stream) = struct
  let (--) i j =
    Stream.unfold i (fun x -> if x=j then None else Some (x, x + 1))

  let all () =
    let open Stream in
    0 -- !Config.length
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take !Config.limit
    |> flat_map (fun x -> x -- (x + 30))
    |> fold (+) 0

  let all_no_flat_map () =
    let open Stream in
    0 -- !Config.length
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take !Config.limit
    |> fold (+) 0

  let all_no_take () =
    let open Stream in
    0 -- !Config.length
    |> map (fun x -> x+1)
    |> filter (fun x -> x mod 2 = 0)
    |> flat_map (fun x -> x -- (x+30))
    |> fold (+) 0

  let fold () =
    let open Stream in
    0 -- !Config.length
    |> fold (+) 0

  let map () =
    let open Stream in
    0 -- !Config.length
    |> map (fun x -> x + 1)
    |> fold (+) 0

  let filter () =
    let open Stream in
    0 -- !Config.length
    |> filter (fun x -> x mod 2 = 0)
    |> fold (+) 0

  let flat_map () =
    let open Stream in
    0 -- !Config.length
    |> flat_map (fun x -> singleton x)
    |> fold (+) 0

  let take () =
    let open Stream in
    0 -- !Config.length
    |> take !Config.limit
    |> fold (+) 0

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

module Main_baseline = struct
  module S = Iter

  let (--) i j =
    S.unfoldr (fun x -> if x=j then None else Some (x, x + 1)) i

  let all () =
    0 -- !Config.length
    |> S.map (fun x -> x + 1)
    |> S.filter (fun x -> x mod 3 = 0)
    |> S.take !Config.limit
    |> S.flat_map (fun x -> x -- (x + 30))
    |> S.fold (+) 0

  let all_no_flat_map () =
    0 -- !Config.length
    |> S.map (fun x -> x + 1)
    |> S.filter (fun x -> x mod 3 = 0)
    |> S.take !Config.limit
    |> S.fold (+) 0

  let all_no_take () =
    0 -- !Config.length
    |> S.map (fun x -> x+1)
    |> S.filter (fun x -> x mod 2 = 0)
    |> S.flat_map (fun x -> x -- (x+30))
    |> S.fold (+) 0

  let fold () =
    0 -- !Config.length
    |> S.fold (+) 0

  let map () =
    0 -- !Config.length
    |> S.map (fun x -> x + 1)
    |> S.fold (+) 0

  let filter () =
    0 -- !Config.length
    |> S.filter (fun x -> x mod 2 = 0)
    |> S.fold (+) 0

  let flat_map () =
    0 -- !Config.length
    |> S.flat_map (fun x -> S.singleton x)
    |> S.fold (+) 0

  let take () =
    0 -- !Config.length
    |> S.take !Config.limit
    |> S.fold (+) 0

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
  module Baseline = Main_baseline
  module Pull_cursor = Mk_main(Pull_cursor)
  module Pull_cursor_k = Mk_main(Pull_cursor_k)
  module Pull_cursor_safe = Mk_main(Pull_cursor_safe)
  module Pull_option = Mk_main(Pull_option)
  module Pull_stream_fusion = Mk_main(Pull_stream_fusion)
  module Pull_thunk_list = Mk_main(Pull_thunk_list)
  module Push_reducer_bool = Mk_main(Push_reducer_bool)
  module Push_reducer_stop = Mk_main(Push_reducer_stop)
  module Push_fold_stop = Mk_main(Push_fold_stop)
  module Push_unit = Mk_main(Push_unit)
end


let main_tests =
  let x = Main.Push_unit.current () in begin
    assert (x = Main.Baseline.current ());
    assert (x = Main.Pull_cursor.current ());
    assert (x = Main.Pull_cursor_k.current ());
    assert (x = Main.Pull_cursor_safe.current ());
    assert (x = Main.Pull_option.current ());
    assert (x = Main.Pull_stream_fusion.current ());
    assert (x = Main.Pull_thunk_list.current ());
    assert (x = Main.Push_reducer_bool.current ());
    assert (x = Main.Push_reducer_stop.current ());
    assert (x = Main.Push_fold_stop.current ());
    assert (x = Main.Push_unit.current ());
  end

let main = [
  "Baseline", Main.Baseline.current;
  "Streaming.Source", Main.Pull_cursor.current;
  "Gen", Main.Pull_option.current;
  "Base.Sequence", Main.Pull_stream_fusion.current;
  "Stdlib.Seq", Main.Pull_thunk_list.current;
  "Streaming.Stream", Main.Push_reducer_bool.current;
  "Iter", Main.Push_unit.current;
]


module Mk_concat(Stream : sig
  type 'a t

  val unfold : 's -> ('s -> ('a * 's) option) -> 'a t
  val concat : 'a t -> 'a t -> 'a t
  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
end) = struct
  let (--) i j =
    Stream.unfold i (fun x -> if x=j then None else Some (x, x + 1))

  let no_concat () =
    let open Stream in
    0 -- !Config.length
    |> fold (+) 0

  let concat_2 () =
    let open Stream in
    let size = !Config.length / 2 in
    concat (0 -- size) (size -- size * 2)
    |> fold (+) 0

  let concat_4 () =
    let open Stream in
    let size = !Config.length / 4 in
    concat
      (concat (0 -- size) (size -- size * 2))
      (concat (size * 2 -- size * 3) (size * 3 -- size * 4))
    |> fold (+) 0

  let concat_8 () =
    let open Stream in
    let size = !Config.length / 8 in
    concat
      (concat
        (concat (0 -- size) (size -- size * 2))
        (concat (size * 2 -- size * 3) (size * 3 -- size * 4)))
      (concat
        (concat (size * 4 -- size * 5) (size * 5 -- size * 6))
        (concat (size * 6 -- size * 7) (size * 7 -- size * 8)))
    |> fold (+) 0

  let concat_16 () =
    let open Stream in
    let size = !Config.length / 16 in
    concat
      (concat
        (concat
          (concat (0 -- size) (size -- size * 2))
          (concat (size * 2 -- size * 3) (size * 3 -- size * 4)))
        (concat
          (concat (size * 4 -- size * 5) (size * 5 -- size * 6))
          (concat (size * 6 -- size * 7) (size * 7 -- size * 8))))
      (concat
        (concat
          (concat (size * 8  -- size * 9)  (size * 9  -- size * 10))
          (concat (size * 10 -- size * 11) (size * 11 -- size * 12)))
        (concat
          (concat (size * 12 -- size * 13) (size * 13 -- size * 14))
          (concat (size * 14 -- size * 15) (size * 15 -- size * 16))))
    |> fold (+) 0

  let concat_tests () =
    assert (!Config.length mod 2 = 0);
    let x = no_concat () in begin
      assert (x = concat_2 ());
      assert (x = concat_4 ());
      assert (x = concat_8 ());
      assert (x = concat_16 ());
    end
end

module Concat = struct
  module Push_reducer_bool = Mk_concat(Push_reducer_bool)
  module Push_unit         = Mk_concat(Push_unit)
end

let concat_push_reducer_bool = [
  "push_reducer_bool/no_concat", Concat.Push_reducer_bool.no_concat;
  "push_reducer_bool/concat_2",  Concat.Push_reducer_bool.concat_2;
  "push_reducer_bool/concat_4",  Concat.Push_reducer_bool.concat_4;
  "push_reducer_bool/concat_8",  Concat.Push_reducer_bool.concat_8;
  "push_reducer_bool/concat_16", Concat.Push_reducer_bool.concat_16;
]

let concat_push_unit = [
  "push_unit/no_concat", Concat.Push_unit.no_concat;
  "push_unit/concat_2",  Concat.Push_unit.concat_2;
  "push_unit/concat_4",  Concat.Push_unit.concat_4;
  "push_unit/concat_8",  Concat.Push_unit.concat_8;
  "push_unit/concat_16", Concat.Push_unit.concat_16;
]

let concat_all =
  concat_push_reducer_bool @
  concat_push_unit


let current = main

