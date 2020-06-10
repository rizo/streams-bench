
(* WARNING: DOES NOT TERMINATE. *)

type ('a, 'r) reducer =
  Reducer : {
    init : 's;
    step : 's -> 'a -> 's;
    stop : 's -> 'r;
  } -> ('a, 'r) reducer


type 'a t = { run : 'r . ('a, 'r) reducer -> 'r} [@@unboxed]

let[@inline] return x = { run = fun (Reducer r) -> r.stop (r.step r.init x) }

let singleton = return


let flat_map (f : 'a -> 'b t) (m : 'a t) : 'b t =
  let run (Reducer inner) =
    let[@inline] step s a =
      let nested = f a in
      nested.run (Reducer {
          init = s;
          step = inner.step;
          stop = (fun s -> s)
      }) in
    let outer = Reducer { inner with step } in
    m.run outer in
  { run }


let[@inline] fold f r0 self =
  self.run (Reducer {
      init = r0;
      step = f;
      stop = (fun x -> x);
    })


let map_with_flat_map f self =
  flat_map (fun x -> return (f x)) self


let map f self =
  let run (Reducer inner) =
    let step = (fun r a -> inner.step r (f a)) in
    self.run (Reducer { inner with step })
  in
  { run }


let filter pred self =
  let run (Reducer inner) =
    let[@inline] step state item =
      if pred item then inner.step state item
      else state in
    self.run (Reducer { inner with step })
  in
  { run }

(* Will not interrupt iteration *)
let take limit self =
  let i = ref 0 in
  let[@inline] run (Reducer inner) =
    self.run (Reducer {
      inner with
      step = (fun inner_state item ->
          if !i = limit then inner_state
          else
            (incr i; inner.step inner_state item));
    })
  in
  { run }

(* Will not interrupt iteration *)
let take_pure limit self =
  let[@inline] run (Reducer inner) =
    let init = inner.init, 0 in
    let step (inner_state, i) item =
      if i = limit then (inner_state, -1)
      else inner.step inner_state item, i + 1 in
    let stop (inner_state, _) = inner.stop inner_state in
    self.run (Reducer { init; step; stop })
  in
  { run }


let of_list l =
  let run (Reducer r) =
    let rec loop s l =
      match l with
      | [] -> r.stop s
      | x :: xs -> loop (r.step s x) xs
    in
    loop r.init l
  in
  { run }

let of_array_mut a =
  let len = Array.length a in
  let run (Reducer reducer) =
    let r = ref reducer.init in
    for i = 0 to len - 1 do
      let x = Array.get a i in
      r := reducer.step !r x
    done;
    reducer.stop !r
  in
  { run }


let[@inline] of_array a =
  let len = Array.length a in
  let[@inline] run (Reducer reducer) =
    let[@unroll] rec loop s r =
      if s = len then reducer.stop r else
      let x = Array.get a s in
      loop (s + 1) (reducer.step r x)
    in
    loop 0 reducer.init
  in
  { run }


let[@inline] unfold seed next =
  let[@inline] run (Reducer r) =
    let[@inline] rec loop seed acc =
      match next seed with
      | None -> r.stop acc
      | Some (x, seed') -> loop seed' (r.step acc x)
    in
    loop seed r.init
  in
  { run }

