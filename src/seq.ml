
type 'a t = ('a -> unit) -> unit

let count n k =
  let count = ref 0 in
  while true do
    k !count;
    incr count
  done

let map f seq k =
  seq (fun x -> k (f x))

let filter p seq k =
  seq (fun x -> if p x then k x)

exception ExitTake

let take n seq k =
  let count = ref 0 in
  try
    seq
      (fun x ->
        if !count = n then raise ExitTake;
        incr count;
        k x)
  with ExitTake -> ()

exception Fold_stop_sequence

let fold f acc seq =
  let r = ref acc in
  seq (fun elt -> r := f !r elt);
  !r

let to_list seq =
  let r = ref [] in
  seq (fun elt -> r := elt :: !r);
  !r

