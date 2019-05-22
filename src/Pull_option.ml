
type 'a t = unit -> 'a option

let count () =
  let i = ref 0 in
  fun () ->
    let x = !i in
    incr i;
    Some x

let init n f =
  let i = ref 0 in
  fun () ->
    if !i = n then None
    else begin
      let x = !i in
      incr i;
      Some (f x)
    end

let unfold seed next =
  let seed = ref seed in
  fun () ->
    match next !seed with
    | None -> None
    | Some (a, seed') -> seed := seed'; Some a

let of_list l =
  let l = ref l in
  fun () ->
    match !l with
    | [] -> None
    | x::l' -> l := l'; Some x

let map f gen =
  let stop = ref false in
  fun () ->
    if !stop then None
    else match gen () with
      | None -> stop := true; None
      | Some x -> Some (f x)

let filter p gen =
  let rec next () =
    (* wrap exception into option, for next to be tailrec *)
    match gen () with
    | None -> None
    | (Some x) as res ->
      if p x
      then res      (* yield element *)
      else next ()  (* discard element *)
  in next

let take n gen =
  let count = ref 0 in
  fun () ->
    if !count = n || !count = ~-1
    then None
    else match gen() with
      | None -> count := ~-1; None   (* indicate stop *)
      | (Some _) as x -> incr count; x

let rec fold f acc gen =
  match gen () with
  | None -> acc
  | Some x -> fold f (f x acc) gen


type 'a state =
  | Start
  | Cur of 'a
  | Stop

let flat_map f g =
  let state = ref Start in
  let rec aux () = match !state with
    | Start -> next_gen(); aux ()
    | Stop -> None
    | Cur g' ->
      match g'() with
      | None -> next_gen(); aux ()
      | Some _ as res -> res
  and next_gen() = match g() with
    | None -> state := Stop
    | Some x -> state := Cur (f x)
    | exception e -> state := Stop; raise e
  in
  aux
