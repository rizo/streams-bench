
module Containers_sequence = Sequence
module Core_sequence       = Core.Std.Sequence
(* module Elements_stream     = Elements.Data.Stream *)

open Core_bench.Std
module B = Bench.Test



let list_for_loop village =
  let v_ref = ref village in
  let count = ref 0 in
  while !v_ref <> [] do
    let p = List.hd !v_ref in
    v_ref := List.tl !v_ref;
    if Village.age p < 18 && Village.sex p = `F
    then begin
      incr count;
    end
  done;
  !count

let list_simple_fold village =
  List.fold_left
    (fun r p -> if Village.age p < 18 && Village.sex p = `F then r + 1 else r)
    0 village

let list_sequential village =
  village
  |> List.filter (fun p -> Village.age p < 18)
  |> List.filter (fun p -> Village.sex p = `F)
  |> List.map (fun _ -> 1)
  |> List.fold_left (+) 0

(* let elements_stream village = *)
(*   village *)
(*   |> Elements_stream.of_list *)
(*   |> Elements_stream.filter (fun p -> Village.age p < 18) *)
(*   |> Elements_stream.filter (fun p -> Village.sex p = `F) *)
(*   |> Elements_stream.map (fun _ -> 1) *)
(*   |> Elements_stream.fold (+) 0 *)

let core_sequence village =
  let module Seq = Core_sequence in
  village
  |> Seq.of_list
  |> Seq.filter ~f:(fun p -> Village.age p < 18)
  |> Seq.filter ~f:(fun p -> Village.sex p = `F)
  |> Seq.map ~f:(fun _ -> 1)
  |> Seq.fold ~f:(+) ~init:0

let containers_sequence village =
  let module Seq = Containers_sequence in
  village
  |> Seq.of_list
  |> Seq.filter (fun p -> Village.age p < 18)
  |> Seq.filter (fun p -> Village.sex p = `F)
  |> Seq.map (fun _ -> 1)
  |> Seq.fold (+) 0

let containers_gen village =
  village
  |> Gen.of_list
  |> Gen.filter (fun p -> Village.age p < 18)
  |> Gen.filter (fun p -> Village.sex p = `F)
  |> Gen.map (fun _ -> 1)
  |> Gen.fold (+) 0

let transducers village =
  let process =
    Trans.(filter (fun p -> Village.age p < 18) >>
           filter (fun p -> Village.sex p = `F) >>
           map (fun _ -> 1)) in
  List.fold_left (process (+)) 0 village

let containers_klist village =
  village
  |> CCKList.of_list
  |> CCKList.filter (fun p -> Village.age p < 18)
  |> CCKList.filter (fun p -> Village.sex p = `F)
  |> CCKList.map (fun _ -> 1)
  |> CCKList.fold (+) 0

(* let batteries_enum village = *)
(* village *)
(* |> BatList.enum *)
(* |> BatEnum.filter (fun p -> Village.age p < 18) *)
(* |> BatEnum.filter (fun p -> Village.sex p = `F) *)
(* |> BatEnum.map (fun _ -> 1) *)
(* |> BatEnum.fold (+) 0 *)

let () =
  let s = 10000 in
  let v = Village.random_village s in
  print_endline "Benchmark for data processing libraries.";
  Printf.printf "Will perform a simple processing on a village of %d habitants.\n" s;
  Core.Command.run (Bench.make_command [
      B.create ~name:"containers_gen"      (fun () -> ignore (containers_gen      v));
      B.create ~name:"containers_sequence" (fun () -> ignore (containers_sequence v));
      B.create ~name:"containers_klist"    (fun () -> ignore (containers_klist    v));
      B.create ~name:"core_sequence"       (fun () -> ignore (core_sequence       v));
      (* B.create ~name:"elements_stream"     (fun () -> ignore (elements_stream     v)); *)
      B.create ~name:"list_for_loop"       (fun () -> ignore (list_for_loop       v));
      B.create ~name:"list_sequential"     (fun () -> ignore (list_sequential     v));
      B.create ~name:"list_simple_fold"    (fun () -> ignore (list_simple_fold    v));
      B.create ~name:"transducers"         (fun () -> ignore (transducers         v));
    ])


