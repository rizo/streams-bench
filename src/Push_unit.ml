
type 'a t = ('a -> unit) -> unit

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

let fold f acc seq =
  let r = ref acc in
  seq (fun x -> r := f !r x);
  !r

let singleton x k =
  k x

let rec of_list l k =
  match l with
  | [] -> ()
  | x :: xs ->
    k x; of_list xs k

let flat_map f self = fun k ->
  self (fun a -> (f a) k)

let empty = fun _k -> ()

let concat self other = fun k ->
  self k;
  other k

let unfold seed next = fun k ->
  let rec loop seed =
    match next seed with
    | None -> ()
    | Some (x, seed') ->
      k x;
      loop seed'
  in
  loop seed


let (--) start stop = fun k ->
  for i = start to stop do k i done

