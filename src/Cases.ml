
module Mk_main(Stream : Types.Stream) = struct
  let (--) i j =
    Stream.unfold i (fun x -> if x=j then None else Some (x, x + 1))

  let all_ops_list () =
    let open Stream in
    Config.input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take Config.limit
    |> flat_map (fun x -> x -- (x + 30))
    |> fold (+) 0

  let all_ops_range () =
    let open Stream in
    0 -- Config.length
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take Config.limit
    |> flat_map (fun x -> x -- (x + 30))
    |> fold (+) 0

  let basic_ops_range () =
    let open Stream in
    0 -- Config.length
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take Config.limit
    |> fold (+) 0

  let ccube () =
    let open Stream in
    0 -- Config.length
    |> map (fun x -> x+1)
    |> filter (fun x -> x mod 2 = 0)
    |> flat_map (fun x -> x -- (x+30))
    |> fold (+) 0

  let minimal () =
    let open Stream in
    0 -- Config.length
    |> fold (+) 0

  let map () =
    let open Stream in
    0 -- Config.length
    |> map (fun x -> x + 1)
    |> fold (+) 0

  let flat_map () =
    let open Stream in
    0 -- Config.length
    |> flat_map (fun x -> singleton x)
    |> fold (+) 0

  let take () =
    let open Stream in
    0 -- Config.length
    |> take Config.limit
    |> fold (+) 0

  let current = all_ops_range
end

module Main = struct
    module Push_reducer_bool = Mk_main(Push_reducer_bool)
    module Push_unit = Mk_main(Push_unit)
    module Pull_cursor = Mk_main(Pull_cursor)
    module Pull_cursor_k = Mk_main(Pull_cursor_k)
    module Pull_cursor_ultimate = Mk_main(Pull_cursor_ultimate)
    module Push_reducer_stop = Mk_main(Push_reducer_stop)
    module Pull_stream_fusion = Mk_main(Pull_stream_fusion)
end

let main = [
  "push_reducer_bool", Main.Push_reducer_bool.current;
  "push_unit", Main.Push_unit.current;
  "pull_cursor", Main.Pull_cursor.current;
  "pull_cursor_k", Main.Pull_cursor_k.current;
  "pull_cursor_ultimate", Main.Pull_cursor_ultimate.current;
  "push_reducer_stop", Main.Push_reducer_stop.current;
  "pull_stream_fusion", Main.Pull_stream_fusion.current;
]


module Mk_concat(Stream : sig
  type 'a t

  val unfold : 's -> ('s -> ('a * 's) option) -> 'a t
  val concat : 'a t -> 'a t -> 'a t
  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
end) = struct
  let (--) i j =
    Stream.unfold i (fun x -> if x=j then None else Some (x, x + 1))

  let baseline () =
    let open Stream in
    0 -- Config.length
    |> fold (+) 0

  let concat_2 () =
    let open Stream in
    let size = Config.length / 2 in
    concat (0 -- size) (size -- size * 2)
    |> fold (+) 0

  let concat_4 () =
    let open Stream in
    let size = Config.length / 4 in
    concat
      (concat (0 -- size) (size -- size * 2))
      (concat (size * 2 -- size * 3) (size * 3 -- size * 4))
    |> fold (+) 0

  let concat_8 () =
    let open Stream in
    let size = Config.length / 8 in
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
    let size = Config.length / 16 in
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

  let () =
    assert (
      baseline () = concat_2  () && 
      baseline () = concat_4  () &&
      baseline () = concat_8  () &&
      baseline () = concat_16 ()
    )
end

module Concat = struct
  module Push_reducer_bool = Mk_concat(Push_reducer_bool)
end

let concat = [
  "push_reducer_bool/baseline", Concat.Push_reducer_bool.baseline;
  "push_reducer_bool/concat_2", Concat.Push_reducer_bool.concat_2;
  "push_reducer_bool/concat_4", Concat.Push_reducer_bool.concat_4;
  "push_reducer_bool/concat_8", Concat.Push_reducer_bool.concat_8;
  "push_reducer_bool/concat_16", Concat.Push_reducer_bool.concat_16;
]


let current = main

