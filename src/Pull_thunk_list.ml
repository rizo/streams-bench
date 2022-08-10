(* Stdlib.Seq *)

type +'a node = Nil | Cons of 'a * 'a t
and 'a t = unit -> 'a node

let empty () = Nil
let return x () = Cons (x, empty)
let singleton = return

let rec map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> Cons (f x, map f next)

let rec filter_map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> (
    match f x with
    | None -> filter_map f next ()
    | Some y -> Cons (y, filter_map f next))

let rec filter f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> if f x then Cons (x, filter f next) else filter f next ()

let rec flat_map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) -> flat_map_app f (f x) next ()

(* this is [append seq (flat_map f tail)] *)
and flat_map_app f seq tail () =
  match seq () with
  | Nil -> flat_map f tail ()
  | Cons (x, next) -> Cons (x, flat_map_app f next tail)

let fold f acc seq =
  let rec aux f acc seq =
    match seq () with
    | Nil -> acc
    | Cons (x, next) ->
      let acc = f acc x in
      aux f acc next
  in
  aux f acc seq

let iter f seq =
  let rec aux seq =
    match seq () with
    | Nil -> ()
    | Cons (x, next) ->
      f x;
      aux next
  in
  aux seq

let rec take n seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, k) -> if n = 0 then Nil else Cons (x, take (n - 1) k)

let of_list l =
  let rec aux l () =
    match l with
    | [] -> Nil
    | x :: tail -> Cons (x, aux tail)
  in
  aux l

let unfold seed next =
  let rec loop seed () =
    match next seed with
    | None -> Nil
    | Some (x, seed') -> Cons (x, loop seed')
  in
  loop seed

let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
