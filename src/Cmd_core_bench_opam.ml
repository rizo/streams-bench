open Core
open Core_bench


(* let args = [ 10; 100; 1000; 10000; 100000; 1000000 ] *)
let args = [ 1_000_000]

(*----- the "Gen" library - c_cube -----*)
let f_gen n =
  let open Gen in
  0 -- n
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 3 = 0)
  |> take (n/2)
  |> flat_map (fun x -> x -- (x + 30))
  |> fold (+) 0 ;;

let f_gen_s n =
  Staged.stage (fun () -> f_gen n) ;;

(*----- the "Stdlib_Seq" library -----*)
let f_std_seq n =
  let open Seq in

  let rec (--) i j () =
    if i = j then Cons (i, empty)
    else if i < j then Cons (i, i + 1 -- j)
    else Cons (i, i - 1 -- j) in

  let rec take n (l:'a t) () =
    if n=0 then Nil
    else match l () with
      | Nil -> Nil
      | Cons (x,l') -> Cons (x, take (n-1) l')

  in
  0 -- n
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 3 = 0)
  |> take (n/2)
  |> flat_map (fun x -> x -- (x + 30))
  |> fold_left (+) 0 ;;

let f_std_seq_s n =
  Staged.stage (fun () -> f_std_seq n) ;;


(*----- the "Iter" library - c_cube -----*)
let f_iter n =
  let open Iter in
  0 -- n
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 3 = 0)
  |> take (n/2)
  |> flat_map (fun x -> x -- (x + 30))
  |> fold (+) 0 ;;

let f_iter_s n =
  Staged.stage (fun () -> f_iter n) ;;


(*----- the "Base" library - JST -----*)
let f_base n =
  let open Base.Sequence in
  let takes len s = take s len in
  range ~start:`inclusive ~stop:`inclusive 0 n
  |> map ~f:(fun x -> x+1)
  |> filter ~f:(fun x -> x mod 3 = 0)
  |> takes (n/2)
  |> concat_map ~f:(fun x -> range ~start:`inclusive ~stop:`inclusive x (x + 30))
  |> fold ~f:(+) ~init:0 ;;

let f_base_s n =
  Staged.stage (fun () -> f_base n) ;;

(*----- the "Streaming" library - rizo -----*)
let f_stream n =
  let open Streaming.Stream in
  0 -- n
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 3 = 0)
  |> take (n/2)
  |> flat_map (fun x -> x -- (x + 32)) (* 32 ??? *)
  |> fold (+) 0  ;;

let f_stream_s n =
  Staged.stage (fun () -> f_stream n) ;;


(*----- the "Streaming" library local - rizo -----*)
let (--) i j =
  Streams_bench.Push_reducer_bool.unfold i (fun x -> if x=j then None else Some (x, x + 1))
module S = Streams_bench.Push_reducer_bool
let f_stream_local n =
  0 -- n
  |> S.map (fun x -> x+1)
  |> S.filter (fun x -> x mod 3 = 0)
  |> S.take (n/2)
  |> S.flat_map (fun x -> x -- (x + 31)) (* 32 ??? *)
  |> S.fold (+) 0  ;;

let f_stream_local_s n =
  Staged.stage (fun () -> f_stream_local n) ;;


(*----- the "Iter" library local - c_cube -----*)
let (--) i j =
  Streams_bench.Push_unit.unfold i (fun x -> if x=j then None else Some (x, x + 1))
let f_iter_local n =
  let open Streams_bench.Push_unit in
  0 -- n
  |> map (fun x -> x+1)
  |> filter (fun x -> x mod 3 = 0)
  |> take (n/2)
  |> flat_map (fun x -> x -- (x + 31)) (* 32 ??? *)
  |> fold (+) 0  ;;

let f_iter_local_s n =
  Staged.stage (fun () -> f_iter_local n) ;;



(*----- Tests results -----*)
let () =
  let n = 1_000_000 in
  let fgen = f_gen n in
  assert (f_iter n = fgen);
  assert (f_base n = fgen);
  assert (f_std_seq n = fgen);
  assert (f_stream n = fgen);
  assert (f_stream_local n = fgen);
  assert (f_iter_local n = fgen);
  ()  ;;


let base_sequence =
  Bench.Test.create_indexed
    ~name:"base.sequence"
    ~args:args
    f_base_s ;;

let iter =
  Bench.Test.create_indexed
    ~name:"iter"
    ~args:args
    f_iter_s ;;

let std_seq =
  Bench.Test.create_indexed
    ~name:"std_seq"
    ~args:args
    f_std_seq_s ;;

let gen =
  Bench.Test.create_indexed
    ~name:"gen"
    ~args:args
    f_gen_s ;;

let stream =
  Bench.Test.create_indexed
    ~name:"stream"
    ~args:args
    f_stream_s ;;

let stream_local =
  Bench.Test.create_indexed
    ~name:"stream_local"
    ~args:args
    f_stream_local_s ;;

let iter_local =
  Bench.Test.create_indexed
    ~name:"iter_local"
    ~args:args
    f_iter_local_s ;;


let command =
  Bench.make_command [ stream; stream_local; iter_local; iter ]
  (* Bench.make_command [ gen; std_seq; base_sequence; stream; iter ] *)

let () = Command.run command ;;
