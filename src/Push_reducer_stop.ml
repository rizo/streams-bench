

type 'a reduced = Done of 'a | Continue of 'a

let unwrap_reduced x =
  match x with
  | Done r | Continue r -> r

type ('a, 'r) reducer =
  Reducer : {
    init : 's;
    step : 'a -> 's -> 's reduced;
    stop : 's -> 'r;
  } -> ('a, 'r) reducer


type 'a t = { run : 'r . ('a, 'r) reducer -> 'r}

let return x =
  let run (Reducer r) =
    r.stop (unwrap_reduced (r.step x r.init) ) in
  { run }

let singleton = return

let flat_map_broken (f : 'a -> 'b t) (this : 'a t) : 'b t =
  let run (Reducer inner) =
    let step x acc =
        let sub = f x in
        Continue (sub.run (Reducer {
          init = acc;
          step = inner.step;
          stop = (fun acc -> acc);
        })) in
    this.run (Reducer { inner with step }) in
  { run }


let flat_map_with_flag f this =
  let run (Reducer inner) =
    let step x acc =
      let nested = f x in
      nested.run (Reducer {
          init = (acc, true);
          step = (fun x (acc, _) ->
              match inner.step x acc with
              | Continue acc' -> Continue (acc', true)
              | Done acc' -> Done (acc', false)
            );
          stop = (fun (acc, should_continue) ->
              if should_continue then Continue acc
              else Done acc)
        })
    in
    this.run (Reducer { inner with step })
  in { run }


let flat_map f this =
  let run (Reducer inner) =
    let step x acc =
      let nested = f x in
      nested.run (Reducer {
          init = Continue acc;
          step = (fun x reduced ->
              Continue (inner.step x (unwrap_reduced reduced))
            );
          stop = (fun reduced -> reduced)
        })
    in
    this.run (Reducer { inner with step })
  in { run }



let fold f r0 this =
  this.run (Reducer {
      init = r0;
      step = (fun a r -> Continue (f r a));
      stop = (fun x -> x);
    })


let map_with_flat_map f this =
  flat_map (fun x -> return (f x)) this


let filter pred this =
  let run (Reducer inner) =
    this.run (Reducer {
        inner with step = (fun a r ->
          if pred a then inner.step a r
          else Continue r)
      })
  in
  { run }


let map f this =
  let run (Reducer inner) =
    this.run (Reducer {
        inner with step = (fun a r -> inner.step (f a) r)
      })
  in
  { run }



let take n this =
  let run (Reducer inner) =
    let step a (s, i) =
      if i >= n then Done (s, i + 1)
      else match inner.step a s with
        | Continue s' -> Continue (s', i + 1)
        | Done s' -> Done (s', i + 1)
    in
    this.run (Reducer {
        init = (inner.init, 0);
        step;
        stop = (fun (s, _) -> inner.stop s);
      })
  in
  { run }


let take_list n this =
  this.run (Reducer {
      init = (0, []);
      step = (fun a (count, acc) ->
          if count = n
          then Done (count, acc)
          else Continue (count + 1, a :: acc)
        );
      stop = (fun (_, acc) -> List.rev acc);
    })


let of_list input =
  let run (Reducer reducer) =
    let rec loop cursor acc =
      match cursor with
      | [] -> reducer.stop acc
      | x :: cursor' ->
        match reducer.step x acc with
        | Continue acc' -> loop cursor' acc'
        | Done acc' -> reducer.stop acc'
    in
    loop input reducer.init
  in
  { run }


let[@inline] of_array arr =
  let len = Array.length arr in
  let run (Reducer k) =
    let[@unroll] rec loop i r =
      if i = len then r else
      let x = Array.get arr i in
      match k.step x r with
      | Done r' -> r'
      | Continue r' ->
        loop (i + 1) r' in
    k.stop (loop 0 (k.init)) in
  { run }


let unfold seed0 next =
  let run (Reducer r) =
    let rec loop seed acc =
      match next seed with
      | None -> r.stop acc
      | Some (a, seed') ->
        match r.step a acc with
        | Continue acc' -> loop seed' acc'
        | Done acc' -> r.stop acc'
    in
    loop seed0 r.init
  in
  { run }


let count = unfold 0 (fun n -> Some (n, n + 1))

