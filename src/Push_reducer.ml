
type ('a, 'r) reducer =
  Reducer : { 
    init : 's;
    step : 'a -> 's -> 's;
    stop : 's -> 'r;
  } -> ('a, 'r) reducer


type 'a t = { run : 'r . ('a, 'r) reducer -> 'r} [@@unboxed]

let return x = { run = fun (Reducer r) -> r.stop (r.step x r.init) }


let flat_map (f : 'a -> 'b t) (m : 'a t) : 'b t =
  let run (Reducer inner) =
    let outer = Reducer { inner with
      step = fun a s ->
        (f a).run (Reducer {
            init = s;
            step = inner.step;
            stop = (fun s -> s) })
    } in
    m.run outer in
  { run }


let fold f r0 self =
  self.run (Reducer {
      init = r0;
      step = f;
      stop = (fun x -> x);
    })


let map_with_flat_map f self =
  flat_map (fun x -> return (f x)) self


let[@inline] map f self =
  let run (Reducer inner) =
    self.run (Reducer {
        inner with step = (fun a r ->
          inner.step (f a) r)
      })
  in
  { run }


let[@inline] filter pred self =
  let run (Reducer inner) =
    self.run (Reducer { inner with step = fun item state ->
        if pred item then inner.step item state
        else state
      })
  in
  { run }


(* Will not interrupt iteration *)
let take limit self =
  let run (Reducer inner) =
    self.run (Reducer {
        init = (inner.init, 0);
        step = (fun item (inner_state, count) ->
            if count = limit then
              (inner_state, count)
            else
              (inner.step item inner_state, count + 1));
        stop = (fun (inner_state, _) -> inner.stop inner_state);
      })
  in
  { run }


let of_list l =
  let run (Reducer r) =
    let rec loop s l =
      match l with
      | [] -> r.stop s
      | x :: xs -> loop (r.step x s) xs
    in
    loop r.init l
  in
  { run }


let unfold seed next =
  let run (Reducer r) =
    let rec loop seed acc =
      match next seed with
      | None -> r.stop acc
      | Some (x, seed') -> loop seed' (r.step x acc)
    in
    loop seed r.init
  in
  { run }

