
let input_len = 100_000

let transducers' () =
  let open Trans.Fast in
  let xf =
    filter (fun x -> x mod 2 = 0)
    >> map ((+) 1)
    >> take input_len
    >> map ((+) 1)
    >> filter (fun x -> x mod 2 = 0) in
  let sum = transduce xf (+) 0 (count ()) in
  sum

let sequence () =
  let open Seq.Base in
  let sum =
    count ()
    |> filter (fun x -> x mod 2 = 0)
    |> map ((+) 1)
    |> take input_len
    |> map ((+) 1)
    |> filter (fun x -> x mod 2 = 0)
    |> fold (+) 0
  in sum

let iter () =
  let open Iter.Base in
  let sum =
    count ()
    |> filter (fun x -> x mod 2 = 0)
    |> map ((+) 1)
    |> take input_len
    |> map ((+) 1)
    |> filter (fun x -> x mod 2 = 0)
    |> fold (+) 0
  in sum

let iter_k1 () =
  let open Iter.K1 in
  let sum =
    count
    >>> filter (fun x -> x mod 2 = 0)
    >>> map ((+) 1)
    >>> take input_len
    >>> map ((+) 1)
    >>> filter (fun x -> x mod 2 = 0)
    >>> fold (+) 0
  in sum

let iter_k2 () =
  let open Iter.K2 in
  let sum =
    count
    >>> filter (fun x -> x mod 2 = 0)
    >>> map ((+) 1)
    >>> take input_len
    >>> map ((+) 1)
    >>> filter (fun x -> x mod 2 = 0)
    >>> fold (+) 0
  in sum


let gen () =
  let open Gen in
  let sum =
    count ()
    |> filter (fun x -> x mod 2 = 0)
    |> map ((+) 1)
    |> take input_len
    |> map ((+) 1)
    |> filter (fun x -> x mod 2 = 0)
    |> fold (+) 0
  in sum

let gen_exn () =
  let open Gen_exn in
  let sum =
    count ()
    |> filter (fun x -> x mod 2 = 0)
    |> map ((+) 1)
    |> take input_len
    |> map ((+) 1)
    |> filter (fun x -> x mod 2 = 0)
    |> fold (+) 0
  in sum


let () =
  let open Core_bench.Std in
  print_endline "Benchmark for data processing libraries.";
  print_endline ("Input length = " ^ string_of_int input_len);
  let bench n = Bench.Test.create ~name:n in
  Core.Command.run (Bench.make_command [
      (* bench "transducers'"        transducers'; *)
      bench "sequence"            sequence;
      bench "iter"                iter;
      (* bench "iter_k1"             iter_k1; *)
      (* bench "iter_k2"             iter_k2; *)
      bench "gen"                 gen;
      bench "gen_exn"             gen_exn;
    ])


