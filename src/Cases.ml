
module Make(Stream : Types.Stream) = struct
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

  let ccube () =
    let open Stream in
    1 -- Config.length
    |> map (fun x -> x+1)
    |> filter (fun x -> x mod 2 = 0)
    |> flat_map (fun x -> x -- (x+30))
    |> fold (+) 0

  let current = all_ops_range
end


module All = struct
    module Push_reducer_bool = Make(Push_reducer_bool)
    module Push_unit = Make(Push_unit)
    module Pull_cursor = Make(Pull_cursor)
    module Pull_cursor_k = Make(Pull_cursor_k)
    module Pull_cursor_ultimate = Make(Pull_cursor_ultimate)
    module Push_reducer_stop = Make(Push_reducer_stop)
    module Pull_stream_fusion = Make(Pull_stream_fusion)
end


let current = [
    "push_reducer_bool", All.Push_reducer_bool.current;
    "push_unit", All.Push_unit.current;
    (*
    "pull_cursor", All.Pull_cursor.current;
    "pull_cursor_k", All.Pull_cursor_k.current;
    "pull_cursor_ultimate", All.Pull_cursor_ultimate.current;
    "push_reducer_stop", All.Push_reducer_stop.current;
    "pull_stream_fusion", All.Pull_stream_fusion.current;
    *)
  ]

