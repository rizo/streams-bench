
type ('a, 'r) reducer =
  Reducer : <
    init : 's;
    step : 'a -> 's -> 's;
    is_done : 's -> bool;
    extract : 's -> 'r;
  > -> ('a, 'r) reducer


type 'a t = {
  run : 'r . ('a, 'r) reducer -> 'r
} [@@unboxed]


let return x =
  let run (Reducer r) =
    r#extract (r#step x r#init) in
  { run }

let flat_map (f : 'a -> 'b t) (m : 'a t) : 'b t =
  let run (Reducer inner) =
    let outer = Reducer (object
        method init = inner#init
        method extract = inner#extract
        method is_done = inner#is_done
        method step a s =
          (f a).run (Reducer (object
                       method init = s;
                       method step = inner#step;
                       method is_done = inner#is_done;
                       method extract = (fun s -> s) 
                     end))
      end) in
    m.run outer in
  { run }


let fold f r0 self =
  self.run (Reducer (object
              method init = r0;
              method step = (fun a r -> (f a r));
              method is_done = (fun _ -> false);
              method extract = (fun x -> x);
            end))


let map_with_flat_map f self =
  flat_map (fun x -> return (f x)) self


let filter pred self =
  let run (Reducer inner) =
    self.run (Reducer (object
                method init = inner#init
                method extract = inner#extract
                method is_done = inner#is_done
                method step = (fun a r ->
                    if pred a then inner#step a r
                    else r)
              end))
  in
  { run }


let map f self =
  let run (Reducer inner) =
    self.run (Reducer (object
                method init = inner#init
                method extract = inner#extract
                method is_done = inner#is_done
                method step = (fun a r -> inner#step (f a) r)
              end))
  in
  { run }



let take n self =
  let run (Reducer inner) =
    self.run (Reducer (object
        method init = (inner#init, 0);
        method step = (fun a (s, i) -> (inner#step a s, i + 1));
        method is_done = (fun (_, i) -> i = n);
        method extract = (fun (s, _) -> inner#extract s);
      end))
  in
  { run }


let of_list l =
  let run (Reducer inner) =
    let rec loop s l =
      if inner#is_done s
      then inner#extract s
      else match l with
        | [] -> inner#extract s
        | x :: xs -> loop (inner#step x s) xs
    in
    loop inner#init l
  in
  { run }

let unfold seed next =
  let run (Reducer inner) =
    let rec loop seed acc =
      if inner#is_done acc
      then inner#extract acc
      else match next seed with
        | None -> inner#extract acc
        | Some (a, seed') -> loop seed' (inner#step a acc)
    in loop seed inner#init
  in { run }

let count = unfold 0 (fun n -> Some (n, n + 1))

