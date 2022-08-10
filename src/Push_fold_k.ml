(* FIXME:
   of_list [1;2;3;4;5]
   |> flat_map (fun x -> of_list [x;x])
   |> take 4
   |> fold (fun acc x -> x :: acc) [] *)

type 'a thunk = unit -> 'a

type 'a t = { run : 'r. (continue:('r -> 'r) -> 'r -> 'a -> 'r) -> 'r -> 'r }
[@@unboxed]

let filter p self =
  let run k r0 =
    self.run
      (fun ~continue r x -> continue (if p x then k ~continue r x else r))
      r0
  in
  { run }

let map f self =
  let run k r0 =
    self.run (fun ~continue r x -> continue (k ~continue r (f x))) r0
  in
  { run }

let take n { run } =
  let run k r0 =
    let i = ref 1 in
    let k' ~continue r x =
      if !i = n then r
      else (
        incr i;
        continue (k ~continue r x))
    in
    run k' r0
  in
  { run = (fun k r0 -> run k r0) }

let ints : int t =
  let rec loop n k r = k ~continue:(fun r' -> loop (n + 1) k r') r n in
  { run = (fun k r -> loop 0 k r) }

let singleton x = { run = (fun k r -> k ~continue:(fun r' -> r') r x) }
let fold f r seq = seq.run (fun ~continue r x -> continue (f r x)) r

let unfold seed next =
  let rec loop seed k r =
    match next seed with
    | None -> r
    | Some (x, seed') -> k ~continue:(fun r' -> loop seed' k r') r x
  in
  { run = (fun k r -> loop seed k r) }

let rec fold_left_while f z xs =
  match xs with
  | [] -> z
  | x :: xs -> f ~loop:(fun s -> fold_left_while f s xs) x z

let of_list list =
  let rec loop xs k r =
    match xs with
    | [] -> r
    | x :: xs' -> k ~continue:(fun r' -> loop xs' k r') r x
  in
  { run = (fun k r -> loop list k r) }

let flat_map f self : 'b t =
  let run k r0 =
    self.run
      (fun ~continue r x ->
        let inner = f x in
        continue (inner.run k r))
      r0
  in
  { run }

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
