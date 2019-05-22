
type 'a t = { run : 'r . ('a -> 'r -> 'r) -> 'r -> 'r } [@@unboxed]

let filter p self =
  let run f = self.run (fun a r -> if p a then f a r else r) in
  { run }

let map f self =
  let run g = self.run (fun x -> g (f x)) in
  { run }

let take n self =
  let run k r0 =
    fst (self.run (fun a (r, i) ->
      if i = n then (r, i) else (k a r, i + 1)) (r0, 0))
  in
  { run }

let rec list_fold f r list =
  match list with
  | [] -> r
  | a :: rest -> list_fold f (f a r) rest

let to_list seq =
  seq.run (fun a r -> a :: r) []

let fold f r seq =
  seq.run f r

let of_list list =
  { run = fun f r -> list_fold f r list }

let unfold seed next =
  let rec loop k r seed =
    match next seed with
    | None -> r
    | Some (a, seed') -> loop k (k a r) seed'
  in
  { run = fun k r -> loop k r seed }


let flat_map f self : 'b t =
  let run k z =
    self.run (fun a r -> (f a).run k r) z
  in
  { run }


