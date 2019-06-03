
let bracket ~(init : unit -> 'r) ~(stop : 'r -> 'b) (f : 'r -> 'r) : 'b =
  let acc = init () in
  try
    let acc' = f acc in
    stop acc'
  with exn ->
    let _acc' = stop acc in
    raise exn


(*

  - The calls to `full` should be cheap as this function will be called to
    avoid allocation of unnecessary resources. If the computation required to decide if the reducer is full is expensive, consider caching it whenever possible.

  - If the producer's initialization is cheap, it should assume that reducer's
    initialization is expensive and, if possible, avoid calling init on the
    reducer.

  - If the producer's initialization is expensive, it should assume that
    reducer's initialization is cheap and, if possible, avoid it's own initialization.

*)

type ('a, 'b) reducer =
  Reducer : {
    init : unit -> 'acc;
    step : 'acc -> 'a -> 'acc;
    full : 'acc -> bool;
    stop : 'acc -> 'b;
  } -> ('a, 'b) reducer


module Reducer = struct
  type ('a, 'b) t = ('a, 'b) reducer

  let make ~init ~step ?(full=fun _ -> false) ~stop =
    Reducer { init; step; full; stop }

  let null =
    Reducer {
      init = (fun () -> ());
      step = (fun () _ -> ());
      full = (fun () -> true);
      stop = (fun () -> ());
    }

  let list =
    Reducer {
      init = (fun () -> []);
      step = (fun acc x -> x :: acc);
      full = (fun _ -> false);
      stop = (fun acc -> List.rev acc);
    }

  let sum =
    Reducer {
      init = (fun () -> 0.);
      step = (fun acc x -> x +. acc);
      full = (fun _ -> false);
      stop = (fun acc -> acc);
    }

  let take n (Reducer k) =
    let init () = (k.init (), 0) in
    let step (acc, i) x = (k.step acc x, i + 1) in
    let full (_, i) = i = n in
    let stop (acc, _) = k.stop acc in
    Reducer { init; step; full; stop }
end


type 'a t =
  { reduce : 'r . ('a, 'r) reducer -> 'r }
  [@@unboxed]


let reduce r self =
  self.reduce r


let return x =
  let reduce (Reducer r) = r.stop (r.step (r.init ()) x) in
  { reduce }


let singleton = return


let flat_map f this =
  let reduce (Reducer k) =
    let step r x =
      (f x).reduce (Reducer { k with
          init = (fun () -> r);
          stop = (fun r -> r)
        }) in
    this.reduce (Reducer { k with step }) in
  { reduce }


let fold step init =
  reduce (Reducer {
      init = (fun () -> init);
      step;
      full = (fun _ -> false);
      stop = (fun x -> x);
    })


let map f self =
  let reduce (Reducer k) =
    let step acc x = k.step acc (f x) in
    self.reduce (Reducer { k with step }) in
  { reduce }


let map_with_flat_map f self =
  flat_map (fun x -> return (f x)) self


let filter pred self =
  let reduce (Reducer k) =
    let step r x = if pred x then k.step r x else r in
    self.reduce (Reducer { k with step }) in
  { reduce }


let take_mut n self =
  let reduce (Reducer k) =
    let i = ref 0 in
    self.reduce (Reducer {
        init = k.init;
        step = (fun r x -> incr i; k.step r x);
        full = (fun _ -> !i = n);
        stop = k.stop;
      }) in
  { reduce }


let take_pure n self =
  let reduce (Reducer k) =
    self.reduce (Reducer {
        init = (fun () -> k.init (), 0);
        step = (fun (acc, i) x -> (k.step acc x, i + 1));
        full = (fun (_, i) -> i = n);
        stop = (fun (acc, _) -> k.stop acc);
      }) in
  { reduce }

let take = take_pure


let unfold_unsafe s0 next =
  let reduce (Reducer k) =
    let rec loop s r =
      if k.full r then r
      else match next s with
        | None -> r
        | Some (x, s') -> loop s' (k.step r x) in
    k.stop (loop s0 (k.init ())) in
  { reduce }


let unfold_safe s0 next =
  let reduce (Reducer k) =
    let rec loop s r =
      if k.full r then r
      else match next s with
        | None -> r
        | Some (x, s') -> loop s' (k.step r x) in
    bracket (loop s0) ~init:k.init ~stop:k.stop in
  { reduce }

let unfold = unfold_safe


(* Does not handle exceptions in the reducer.
 * Theoretically the fastest implementation. *)
let of_list_unsafe l =
  let reduce (Reducer k) =
    let rec loop s r =
      if k.full r then r
      else match s with
        | [] -> r
        | x :: s' -> loop s' (k.step r x) in
    k.stop (loop l (k.init ()))
  in
  { reduce }


let of_list_safe xs =
  let reduce (Reducer k) =
    let rec loop s r =
      if k.full r then r
      else match s with
        | [] -> r
        | x :: s' -> loop s' (k.step r x) in
    bracket (loop xs) ~init:k.init ~stop:k.stop in
  { reduce }

let of_list = of_list_safe


let count =
  let reduce (Reducer k) =
    let rec loop s r =
      if k.full r then r
      else loop (s + 1) (k.step r s) in
    bracket (loop 0) ~init:k.init ~stop:k.stop in
  { reduce }


let (--) i j =
  unfold i (fun x -> if x=j then None else Some (x, x + 1))


let to_list self =
	self.reduce (Reducer {
			init = (fun () -> []);
			step = (fun acc x -> x :: acc);
			full = (fun _ -> false);
			stop = List.rev;
		})


let iter f self =
	self.reduce (Reducer {
			init = (fun () -> ());
			step = (fun () x -> f x);
			full = (fun _ -> false);
			stop = (fun () -> ());
		})


let concat this that =
  let reduce (Reducer k) =
    let stop r = that.reduce (Reducer {k with init = (fun () -> r)}) in
    this.reduce (Reducer { k with stop }) in
  { reduce }


