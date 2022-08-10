type 'a t = { run : 'r. ('r -> 'a -> 'r option) -> 'r -> 'r } [@@unboxed]

let map f self : 'a t =
  let run step = self.run (fun r a -> step r (f a)) in
  { run }

let filter p self : 'a t =
  let run step = self.run (fun r a -> if p a then step r a else Some r) in
  { run }

let take n self : 'a t =
  let count = ref 0 in
  {
    run =
      (fun step ->
        self.run (fun a r ->
            if !count = n then None
            else (
              incr count;
              step a r)));
  }

let fold f acc self = self.run (fun r a -> Some (f r a)) acc

let of_list l : 'a t =
  {
    run =
      (fun k r ->
        let rec loop l r =
          match l with
          | [] -> r
          | x :: xs -> (
            match k r x with
            | Some r' -> loop xs r'
            | None -> r)
        in
        loop l r);
  }

let unfold seed next =
  {
    run =
      (fun k acc ->
        let rec loop seed acc =
          match next seed with
          | None -> acc
          | Some (x, seed') -> (
            match k acc x with
            | Some acc' -> loop seed' acc'
            | None -> acc)
        in
        loop seed acc);
  }

let flat_map f self : 'b t =
  { run = (fun k -> self.run (fun r a -> Some ((f a).run k r))) }

let empty = { run = (fun _k r -> r) }

let singleton a =
  {
    run =
      (fun k r ->
        match k r a with
        | Some r -> r
        | _ -> r);
  }

let append self other = { run = (fun k r -> other.run k (self.run k r)) }
let ( -- ) i j = unfold i (fun x -> if x = j then None else Some (x, x + 1))
