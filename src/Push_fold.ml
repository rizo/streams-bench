
type 'a t = { run : 'r . ('r -> 'a -> 'r) -> 'r -> 'r } [@@unboxed]

let filter p self =
  let run f = self.run (fun r a -> if p a then f r a else r) in
  { run }

let map f self =
  let run g = self.run (fun r x -> g r (f x)) in
  { run }

let take n self =
  let run k r0 =
    fst (self.run (fun (r, i) a ->
      if i = n then (r, i) else (k r a, i + 1)) (r0, 0))
  in
  { run }

let rec list_fold f r list =
  match list with
  | [] -> r
  | a :: rest -> list_fold f (f r a) rest

let singleton a =
  { run = fun k r -> k r a }

let to_list seq =
  seq.run (fun r a -> a :: r) []

let fold f r seq =
  seq.run f r

let of_list list =
  { run = fun f r -> list_fold f r list }

let unfold seed next =
  let rec loop k r seed =
    match next seed with
    | None -> r
    | Some (a, seed') -> loop k (k r a) seed'
  in
  { run = fun k r -> loop k r seed }


let flat_map f self : 'b t =
  let run k z =
    self.run (fun r a -> (f a).run k r) z
  in
  { run }


