type 'a t = unit -> 'a

let wrap f x = try Some (f x) with _e -> None

exception StopIteration

let count () =
  let i = ref 0 in
  fun () ->
    let x = !i in
    incr i;
    x

let unfold seed next =
  let seed = ref seed in
  fun () ->
    match next !seed with
    | None -> raise StopIteration
    | Some (x, seed') ->
      seed := seed';
      x

let of_list l =
  let l = ref l in
  fun () ->
    match !l with
    | [] -> raise StopIteration
    | x :: l' ->
      l := l';
      x

let map f gen () = f (gen ())

let filter p gen =
  let rec loop () =
    let x = gen () in
    if p x then x else loop ()
  in
  loop

let take n gen =
  let i = ref 0 in
  fun () ->
    if !i = n then raise StopIteration
    else (
      incr i;
      gen ())

let rec fold f acc gen =
  match try Some (gen ()) with StopIteration -> None with
  | Some x -> fold f (f x acc) gen
  | None -> acc

type 'a state = Start | Cur of 'a | Stop

let flat_map f g =
  let state = ref Start in
  let rec aux () =
    match !state with
    | Start ->
      next_gen ();
      aux ()
    | Stop -> raise StopIteration
    | Cur g' -> (
      match g' () with
      | x -> x
      | exception StopIteration ->
        next_gen ();
        aux ())
  and next_gen () =
    match g () with
    | x -> state := Cur (f x)
    | exception e ->
      state := Stop;
      raise e
  in
  aux

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
