
open Streams_bench.Types

module Assert = struct
  let int expected actual =
    if actual = expected then ()
    else (
      Fmt.epr "actual=%d != expected=%d@." actual expected;
      assert false
    )

  let list expected actual =
    if actual = expected then ()
    else (
      Fmt.epr "actual=%a != expected=%a@." Fmt.(list int) actual Fmt.(list int) expected;
      assert false
    )

  let unit () () = ()
end

module Basic (M : Stream) = struct
  let test_basic_ () =
    M.unfold 0 (fun n -> Some (n, n + 1))
    |> M.map (fun x -> x + 1)
    |> M.filter (fun x -> x mod 5 = 0)
    |> M.flat_map (fun x -> M.of_list [x + 1; x + 2; x + 3])
    |> M.filter (fun x -> x mod 3 = 0)
    |> M.take 0
    |> M.fold (+) 0
    |> Assert.int 0

  let test_operation_order () =
    assert false

  let test_flat_map_order () =
    M.unfold 0 (fun n -> Some (n, n + 1))
    (* FIXME: ORDER SHOULD NOT MATTER *)
    |> M.flat_map (fun x -> M.of_list [x])
    |> M.take 3
    |> M.fold (+) 0
    |> Assert.int 3

  let test_unfold_empty () =
    M.unfold () (fun () -> None)
    |> M.fold (+) 0
    |> Assert.int 0

  let test_unfold_seq () =
    M.unfold 0 (fun n -> if n = 3 then None else Some (n, n + 1))
    |> M.fold (+) 0
    |> Assert.int 3

  let test_unfold_infinite () =
    M.unfold 1 (fun n -> Some (n, n))
    |> M.take 3
    |> M.fold (+) 0
    |> Assert.int 3

  let test_unfold_infinite_2 () =
    M.unfold 1 (fun n -> Some (n, n))
    |> M.map (fun x -> x + 1)
    |> M.take 3
    |> M.fold (+) 0
    |> Assert.int 6

  let test_map_1 input expected =
    input
    |> M.of_list
    |> M.map (fun x -> x + 1)
    |> M.fold (fun xs x -> x :: xs) []
    |> Assert.list expected

  let test_filter_1 input expected =
    input
    |> M.of_list
    |> M.filter (fun x -> x mod 2 <> 0)
    |> M.fold (fun xs x -> x :: xs) []
    |> Assert.list expected

  let test_take_1 n input expected =
    input
    |> M.of_list
    |> M.take n
    |> M.fold (fun xs x -> x :: xs) []
    |> Assert.list expected

  let test_flat_map_1 input expected =
    input
    |> M.of_list
    |> M.flat_map (fun x -> M.of_list [x + 1; x + 2])
    |> M.fold (fun xs x -> x :: xs) []
    |> Assert.list expected

  let test_flat_map_2 input expected =
    input
    |> M.of_list
    |> M.flat_map (fun _ -> M.of_list [])
    |> M.fold (fun xs x -> x :: xs) []
    |> Assert.list expected

  let test_fold_1 input expected =
    input
    |> M.of_list
    |> M.fold (fun xs x -> x :: xs) []
    |> Assert.list expected
      
  let run () = begin
    test_flat_map_order ();

    test_map_1 [] [];
    test_map_1 [1] [2];
    test_map_1 [1; 2; 3] [4; 3; 2];

    test_filter_1 [] [];
    test_filter_1 [1] [1];
    test_filter_1 [2] [];
    test_filter_1 [1; 2; 3; 4; 5] [5; 3; 1];

    test_take_1 0 [] [];
    test_take_1 1 [1] [1];
    test_take_1 0 [1] [];
    test_take_1 5 [1; 2; 3; 4; 5] [5; 4; 3; 2; 1];
    test_take_1 0 [1; 2; 3; 4; 5] [];
    test_take_1 1 [1; 2; 3; 4; 5] [1];
    test_take_1 3 [1; 2; 3; 4; 5] [3; 2; 1];

    test_flat_map_1 [] [];
    test_flat_map_1 [1] [3; 2];
    test_flat_map_1 [2; 3; 4] [6; 5; 5; 4; 4; 3];

    test_flat_map_2 [] [];
    test_flat_map_2 [1] [];
    test_flat_map_2 [2; 3; 4] [];

    test_fold_1 [] [];
    test_fold_1 [1] [1];
    test_fold_1 [1; 2; 3] [3; 2; 1];

    test_unfold_empty ();
    test_unfold_seq ();
    test_unfold_infinite ();
    test_unfold_infinite_2 ();
  end
end

module Indexed (M : Indexed) = struct
  open Streams_bench.Pull_index

  let test_conv () = begin
    assert (to_array (of_array [||]) = [||]);
    assert (to_array (of_array [|1|]) = [|1|]);
    assert (to_array (of_array [|1; 2; 3|]) = [|1; 2; 3|]);
  end

  let test_get () = begin
    assert (get 0 (of_array [||]) = None);
    assert (get 7 (of_array [||]) = None);
    assert (get 0 (of_array [|'a'; 'b'; 'c'|]) = Some 'a');
    assert (get 1 (of_array [|'a'; 'b'; 'c'|]) = Some 'b');
    assert (get 2 (of_array [|'a'; 'b'; 'c'|]) = Some 'c');
    assert (get 3 (of_array [|'a'; 'b'; 'c'|]) = None);
  end


  let test_map () = begin
    assert (to_array (map string_of_int (of_array [||])) = [||]);
    assert (to_array (map string_of_int (of_array [|0|])) = [|"0"|]);
    assert (to_array (map string_of_int (of_array [|0; 1; 2|])) = [|"0"; "1"; "2"|]);
  end


  let test_append () = begin
    let s0 = of_array [||] in
    let s1 = of_array [|'a'; 'b'; 'c'; 'd'|] in
    let s2 = of_array [|'x'; 'y'; 'z'|] in
    assert (to_array (append s0 s0) = [||]);
    assert (to_array (append s0 s1) = [|'a'; 'b'; 'c'; 'd'|]);
    assert (to_array (append s1 s0) = [|'a'; 'b'; 'c'; 'd'|]);
    assert (to_array (append s1 s2) = [|'a'; 'b'; 'c'; 'd'; 'x'; 'y'; 'z'|]);
    assert (to_array (append s2 s1) = [|'x'; 'y'; 'z'; 'a'; 'b'; 'c'; 'd'|]);
  end


  let run () = begin
    test_conv ();
    test_get ();
    test_map ();
    test_append ();
  end
end


let streams : (string * (module Stream)) list = [
  "Push_reducer_bool", (module Streams_bench.Push_reducer_bool); 
  "Push_unit", (module Streams_bench.Push_unit);
  "Pull_cursor", (module Streams_bench.Pull_cursor);
  "Pull_cursor_k", (module Streams_bench.Pull_cursor_k);
  "Pull_cursor_ultimate", (module Streams_bench.Pull_cursor_ultimate);
  "Push_reducer_stop", (module Streams_bench.Push_reducer_stop);
  "Pull_stream_fusion", (module Streams_bench.Pull_stream_fusion);
]

let slices : (string * (module Indexed)) list = [
  "Pull_index", (module Streams_bench.Pull_index)
]

let () =
  Printexc.record_backtrace true;
  List.iter (fun (name, (case : (module Stream))) ->
      Printf.eprintf "Running tests for `%s`...\n%!" name;
      let module Test = Basic(val case) in
      Test.run ())
    streams;
  List.iter (fun (name, (case : (module Indexed))) ->
      Printf.eprintf "Running tests for `%s`...\n%!" name;
      let module Test = Indexed(val case) in
      Test.run ())
    slices

