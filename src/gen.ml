

type 'a t = unit -> 'a option

let count n =
  let i = ref 0 in
  fun () ->
    let x = !i in
    incr i;
    Some x

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
  | Some x -> fold f (f acc x) gen

