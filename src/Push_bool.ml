type 'a t = ('a -> bool) -> unit

let map f seq k = seq (fun x -> k (f x))
let filter p seq k = seq (fun x -> if p x then k x else true)

let take n seq k =
  let count = ref 0 in
  seq (fun x ->
      if !count = n then false
      else (
        incr count;
        k x))

let fold f acc seq =
  let r = ref acc in
  seq (fun elt ->
      r := f elt !r;
      true);
  !r

let rec of_list l k =
  match l with
  | [] -> ()
  | x :: xs -> if k x then of_list xs k else ()

let unfold seed next k =
  let rec loop seed =
    match next seed with
    | None -> ()
    | Some (x, seed') -> if k x then loop seed' else ()
  in
  loop seed

let flat_map f self k =
  self (fun a ->
      (f a) k;
      true)

let empty _k = ()

(* FIXME: termination is broken *)
let append self other k =
  self k;
  other k

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
