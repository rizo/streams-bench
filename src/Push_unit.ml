type 'a t = ('a -> unit) -> unit

let map f seq k = seq (fun x -> k (f x))
let filter p seq k = seq (fun x -> if p x then k x)

exception ExitTake

let take n seq k =
  let count = ref 0 in
  try
    seq (fun x ->
        if !count = n then raise_notrace ExitTake;
        incr count;
        k x)
  with ExitTake -> ()

let fold f acc seq =
  let r = ref acc in
  seq (fun x -> r := f !r x);
  !r

let singleton x k = k x

let rec of_list l k =
  match l with
  | [] -> ()
  | x :: xs ->
    k x;
    of_list xs k

let flat_map f self k = self (fun a -> (f a) k)
let empty _k = ()

let concat self other k =
  self k;
  other k

let unfold seed next k =
  let seed = ref seed in
  let continue = ref true in
  while !continue do
    match next !seed with
    | None -> continue := false
    | Some (x, seed') ->
      k x;
      seed := seed'
  done

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
