
type 'a t = { run : 'r . ('a -> 'r -> 'r option) -> 'r -> 'r } [@@unboxed]

let map f self : 'a t =
  let run step =
      self.run (fun a r -> step (f a) r) in
  { run }

let filter p self : 'a t = 
  let run step =
    self.run (fun a r ->
        if p a then step a r
        else Some r) in
  { run }

let take n self : 'a t =
  let count = ref 0 in
  { run = fun step ->
      self.run
        (fun a r ->
            if !count = n then None
            else (incr count; step a r)) }

let fold f acc self =
  self.run (fun a r -> Some (f a r)) acc

let of_list l : 'a t =
  { run = fun k r ->
      let rec loop l r =
        match l with
        | [] -> r
        | x :: xs ->
          match k x r with
          | Some r' -> loop xs r'
          | None -> r
      in
      loop l r
  }


let unfold seed next =
  { run = fun k acc ->
      let rec loop seed acc =
        match next seed with
        | None -> acc
        | Some (x, seed') ->
          match k x acc with
          | Some acc' -> loop seed' acc'
          | None -> acc
      in
      loop seed acc
  }

let flat_map f self : 'b t =
  { run = fun k ->
      self.run (fun a r -> Some ((f a).run k r)) }


let empty = { run = fun _k r -> r }


let append self other =
  { run = fun k r ->
      other.run k (self.run k r) }

