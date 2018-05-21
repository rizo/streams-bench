
let bracket init (stop : 'a -> unit) f =
  let x = init () in
  try
    let r = f x in
    stop x;
    r
  with exn ->
    stop x;
    raise exn

let finally f g x =
  try
    let res = f x in
    g x;
    res
  with e ->
    g x;
    raise e

let pass = fun () -> ()

let input_len = 500_000
let input = Array.to_list (Array.init 1_000_000 (fun x -> x))


module Stream_fusion = struct
  type ('a, 's) step =
    | Done
    | Skip  of 's
    | Yield of 'a * 's

  type +_ t =
      Stream : 's * ('s -> ('a, 's) step) -> 'a t

  let fold f init (Stream (seed, next)) =
    let rec loop seed v next f =
      match next seed with
      | Done         -> v
      | Skip  s      -> loop s v next f
      | Yield (a, s) -> loop s (f v a) next f in
    loop seed init next f

  let of_list l =
    let next s =
      match s with
      | [] -> Done
      | x :: xs -> Yield (x, xs) in
    Stream (l, next)

  let map f (Stream (seed, next)) =
    let next s =
      match next s with
      | Done         -> Done
      | Skip s       -> Skip s
      | Yield (a, s) -> Yield (f a, s) in
    Stream (seed, next)

  let filter p (Stream (seed, next)) =
    let next s =
      match next s with
      | Done         -> Done
      | Skip s       -> Skip s
      | Yield (a, s) when p a -> Yield (a, s)
      | Yield (_, s) -> Skip s in
    Stream (seed, next)

  let take n (Stream (s, next)) =
    let next (i, s) =
      if i >= n then Done
      else
        match next s with
        | Done         -> Done
        | Skip s       -> Skip (i, s)
        | Yield (a, s) -> Yield (a, (i + 1, s)) in
    Stream ((0, s), next)

  let bench () =
    let result =
      input
      |> of_list
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 5 = 0)
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 3 = 0)
      |> take input_len
      |> fold (+) 0
    in
    result
    (* assert (result = 8333616669) *)
end

module Seq_safe = struct
  type 'a monad

  module Step = struct
    type ('a, 's, 'r) t =
      | Done of 'r
      | Skip of 's
      | Yield of 'a * 's
      | Lift of 's monad
  end

  type (+'a, +'r) t = Seq : 's * ('s -> ('a, 's, 'r) Step.t) -> ('a, 'r) t
end

module Transducer_tag = struct
  type 'a reduced = Continue of 'a | Done of 'a

  type ('a, 'r) step =
    'a -> 'r -> 'r reduced

  type ('a, 'b, 'r) transducer =
    ('b, 'r) step -> ('a, 'r) step

  let compose t1 t2 =
    fun xf -> (t1 (t2 xf))

  let (>>) = compose

  let map f =
    fun step -> fun a r ->
      step (f a) r

  let filter f =
    fun step -> fun a r ->
      if f a then step a r else Continue r

  let take n =
    fun step ->
      let count = ref 0 in
      fun a r ->
        if !count = n
        then Done r
        else (incr count; step a r)

  let rec fold_while f acc list =
    match list with
    | [] -> acc
    | a :: rest ->
      begin match f a acc with
        | Continue acc' -> fold_while f acc' rest
        | Done acc' -> acc'
      end

  let bench () =
    let xf = map (fun x -> x + 1)
      >> filter (fun x -> x mod 5 = 0)
      >> map (fun x -> x + 1)
      >> filter (fun x -> x mod 3 = 0)
      >> take input_len in
    let reducer = (fun a b -> Continue (a + b)) in
    let result = fold_while (xf reducer) 0 input in
    result
    (* assert (result = 8333616669) *)
end

module Transducer_k = struct
  type ('a, 'r) step =
    'a -> 'r -> ('r -> 'r) -> 'r

  type ('a, 'b, 'r) transducer =
    ('b, 'r) step -> ('a, 'r) step

  let[@inline] compose t1 t2 =
    fun xf -> (t1 (t2 xf))

  let (>>) = compose

  let[@inline] map f : ('a, 'b, 'r) transducer =
    fun step -> fun a r k ->
      step (f a) r k

  let[@inline] filter f : ('a, 'b, 'r) transducer =
    fun step -> fun a r k ->
      if f a then step a r k else k r

  let[@inline] take n : ('a, 'b, 'r) transducer =
    fun step ->
      let count = ref 0 in
      fun a r k ->
        if !count = n
        then r
        else (incr count; step a r k)

  let foldk f =
    let rec go b self =
      match self with
      | [] -> b
      | a :: self' -> f a b (fun b' -> go b' self') in
    go

  let bench () =
    let xf = map (fun x -> x + 1)
      >> filter (fun x -> x mod 5 = 0)
      >> map (fun x -> x + 1)
      >> filter (fun x -> x mod 3 = 0)
      >> take input_len in
    let reducer = (fun a b k -> k (a + b)) in
    let result = foldk (xf reducer) 0 input in
    result
    (* assert (result = 8333616669) *)
end

module Iter = struct
  type 'a iter = Iter : 's * ('s -> ('a * 's) option) -> 'a iter

  let take n (Iter (s0, next)) =
    let next' (s, i) =
      if i <= 0 then None
      else match next s with
        | Some (a, s') -> Some (a, (s', i - 1))
        | None -> None in
    Iter ((s0, n), next')

  let filter p (Iter (s0, next)) =
    let next' s =
      let rec loop s =
        match next s with
        | Some (a, s') when p a -> Some (a, s')
        | Some (_, s') -> loop s'
        | None -> None in
      loop s in
    Iter (s0, next')

  let map f (Iter (s0, next)) =
    let next' s =
      match next s with
      | Some (a, s') -> Some (f a, s')
      | None -> None in
    Iter (s0, next')

  let of_list l =
    let next = function
      | []    -> None
      | x::xs -> Some (x, xs) in
    Iter (l, next)

  let fold f r0 (Iter (s0, next)) =
    let rec loop r s =
      match next s with
      | None -> r
      | Some (a, s') -> loop (f r a) s' in
    loop r0 s0

  let bench () =
    let result =
      input
      |> of_list
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 5 = 0)
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 3 = 0)
      |> take input_len
      |> fold (+) 0
    in
    result
    (* assert (result = 8333616669) *)
end

module Iter_skip = struct
  type 'a t =
      Iter : { init : 's;
               step : 'r. stop:'r -> skip:('s -> 'r) -> yield:('a -> 's -> 'r) -> 's -> 'r } -> 'a t

  let map f (Iter i) =
    let step ~stop ~skip ~yield s =
      i.step ~stop ~skip ~yield:(fun a s' -> yield (f a) s') s in
    Iter { i with step }

  let fold f acc (Iter i) =
    let rec loop acc' s =
      i.step s
        ~stop:acc'
        ~skip:(fun s' -> loop acc' s')
        ~yield:(fun a s' -> loop (f a acc') s') in
    loop acc i.init

  let of_list l =
    let step ~stop ~skip ~yield s =
      match s with
      | [] -> stop
      | a :: rest -> yield a rest in
    Iter { init = l; step }

  let filter f (Iter i) =
    let step ~stop ~skip ~yield s =
      i.step s
        ~stop ~skip
        ~yield:(fun a s' ->
            if f a then yield a s'
            else skip s') in
    Iter { i with step }

  let take n (Iter i) =
    let step ~stop ~skip ~yield (s, count) =
      i.step s
        ~stop ~skip:(fun s -> (skip (s, count)))
        ~yield:(fun a s' ->
            if count = n then stop
            else yield a (s', count + 1)) in
    Iter { init = (i.init, 0); step }
end

module Iter_k = struct
  type 'a iter =
      Iter : {
        init : 'cursor;
        next : 'r . ('a -> 'cursor -> 'r) -> 'r -> 'cursor -> 'r;
      } -> 'a iter

  let of_list list =
    let next yield empty cursor =
      match cursor with
      | []        -> empty
      | a :: rest -> yield a rest in
    Iter { init = list; next }

  let count n =
    let next yield empty cursor = yield cursor (cursor + 1) in
    Iter { init = n; next }

  let filter predicate (Iter i) =
    let rec next yield empty cursor =
      i.next
        ((fun a cursor' ->
            if predicate a then yield a cursor'
            else next yield empty cursor'))
        empty
        cursor
    in
    Iter { i with next }

  let map f (Iter i) =
    let next yield =
      i.next (fun a -> yield (f a))
    in
    Iter { i with next }

  let take n (Iter i) =
    let next yield empty (cursor, count) =
      if count <= 0 then empty
      else i.next (fun a cursor' -> yield a (cursor', count - 1)) empty cursor in
    Iter { init = (i.init, n); next }

  let fold_state s0 f r0 next =
    let rec go r s =
      next (fun a -> go (f a r)) r s in
    go r0 s0

  let fold f r (Iter i) =
    fold_state i.init f r i.next
end

module Iter_k_inline = struct
  type 'a iter =
      Iter : {
        init : 'cursor;
        next : 'r . ('a -> 'cursor -> 'r) -> 'r -> 'cursor -> 'r;
      } -> 'a iter

  let of_list list =
    let next yield empty cursor =
      match cursor with
      | []        -> empty
      | a :: rest -> yield a rest in
    Iter { init = list; next }

  let count n =
    let next yield empty cursor = yield cursor (cursor + 1) in
    Iter { init = n; next }

  let filter predicate (Iter i) =
    let[@inline] rec next yield empty cursor =
      i.next
        ((fun a cursor' ->
            if predicate a then yield a cursor'
            else next yield empty cursor'))
        empty
        cursor
    in
    Iter { i with next }

  let map f (Iter i) =
    let next yield =
      i.next (fun a -> yield (f a))
    in
    Iter { i with next }

  let take n (Iter i) =
    let next yield empty (cursor, count) =
      if count <= 0 then empty
      else i.next (fun a cursor' -> yield a (cursor', count - 1)) empty cursor in
    Iter { init = (i.init, n); next }

  let fold_state s0 f r0 next =
    let rec go r s =
      next (fun a -> go (f a r)) r s in
    go r0 s0

  let fold f r (Iter i) =
    fold_state i.init f r i.next
end


module Iter_safe = struct
  type 'a iter =
      Iter : {
        init : 'cursor;
        next : 'r . ('a -> 'cursor -> 'r) -> (unit -> 'r) -> 'cursor -> 'r;
        (* free : 's -> unit *)
      } -> 'a iter

  let of_list list =
    let[@inline] next yield acc cursor =
      match cursor with
      | [] -> acc ()
      | a :: cursor' -> yield a cursor' in
    Iter {
      init = list;
      next;
      (* free = ignore  *)
    }

  let map f (Iter iter) =
    let[@inline] next yield acc cursor =
      iter.next (fun a cursor' -> yield (f a) cursor') acc cursor in
    Iter { iter with next }

  let filter predicate (Iter iter) =
    let[@inline] rec next yield acc cursor =
      iter.next
        ((fun a cursor' ->
            if predicate a then yield a cursor'
            else next yield acc cursor'))
        acc cursor in
    Iter { iter with next }

  let take n (Iter iter) =
    let[@inline] next yield acc (cursor, i) =
      if i <= 0 then acc ()
      else iter.next (fun a cursor' -> yield a (cursor', i - 1)) (acc) cursor in
    Iter { init = iter.init, n;
           (* free = (fun (s, _) -> iter.free s); *)
           next }

  let fold f z (Iter iter) =
    let rec loop acc cursor =
      iter.next (fun a -> loop (f a acc)) (fun () -> acc) cursor in
    (* bracket *)
    (* iter.init *)
    (* ignore *)
    (* iter.free *)
    (loop z iter.init)

  let bench () =
    let result =
      input
      |> of_list
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 5 = 0)
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 3 = 0)
      |> take input_len
      |> fold (+) 0
    in
    result
end


module Iter_safe_tmp = struct
  type 'a iter =
      Iter : {
        init : unit -> 's;
        next : 'r . ('a -> 's -> 'r) -> ('s -> 'r) -> 's -> 'r;
        stop : 'r . 's -> unit;
      } -> 'a iter

  let empty =
    Iter { init = pass;
           next = (fun _yield stop s -> stop s);
           stop = ignore }

  let map f (Iter i) =
    let[@inline] next yield =
      i.next (fun a -> yield (f a)) in
    Iter { i with next }

  let filter predicate (Iter iter) =
    let[@inline] rec next yield stop =
      iter.next
        ((fun a s' ->
            if predicate a then yield a s'
            else next yield stop s'))
        stop in
    Iter { iter with next }

  let take n (Iter iter) =
    let[@inline] next yield stop (s, i) =
      if i <= 0 then stop (s, i)
      else iter.next
          (fun a s' -> yield a (s', i - 1))
          (fun s -> iter.stop s; stop (s, i))
          s in
    Iter { init = (fun () -> iter.init (), n);
           stop = (fun (s, _) -> iter.stop s);
           next }

  let fold f r (Iter iter) =
    let[@inline] rec go r s =
      iter.next (fun a -> go (f a r)) (fun s -> iter.stop s; r) s in
    go r (iter.init ())

  let of_list list =
    let[@inline] next yield stop s =
      match s with
      | [] -> stop s
      | a :: s' -> finally (yield a) stop s' in
    Iter { init = (fun () -> list); next; stop = ignore }

  let to_list' (Iter iter) =
    let rec go acc =
      iter.next
        (fun a -> go (a :: acc))
        (fun s -> iter.stop s; acc) in
    List.rev (go [] (iter.init ()))

  let bench () =
    let result =
      input
      |> of_list
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 5 = 0)
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 3 = 0)
      |> take input_len
      |> fold (+) 0
    in
    result
end

module Iter_k_unit_safe = struct
  type 'a iter =
      Iter : {
        init : 'cursor;
        next : 'r . ('a -> 'cursor -> 'r) -> (unit -> 'r) -> 'cursor -> 'r;
        stop : unit -> unit;
      } -> 'a iter

  let pass () = ()

  let of_list list =
    let[@inline] next yield stop cursor =
      match cursor with
      | []        -> stop ()
      | a :: rest -> yield a rest in
    Iter { init = list; next; stop = pass }

  let count n =
    let next yield empty cursor = yield cursor (cursor + 1) in
    Iter { init = n; next; stop = pass }

  let filter predicate (Iter i) =
    let[@inline] rec next yield empty cursor =
      i.next
        ((fun a cursor' ->
            if predicate a then yield a cursor'
            else next yield empty cursor'))
        empty
        cursor
    in
    Iter { i with next }

  let map f (Iter i) =
    let[@inline] next yield =
      i.next (fun a -> yield (f a))
    in
    Iter { i with next }

  let take n (Iter i) =
    let[@inline] next yield stop (cursor, count) =
      if count <= 0 then stop ()
      else i.next (fun a cursor' -> yield a (cursor', count - 1)) stop cursor in
    Iter { init = (i.init, n); next; stop = i.stop  }

  let fold_state s0 f r0 next =
    let[@inline] rec go r s =
      next (fun a -> go (f a r)) (fun () -> r) s in
    go r0 s0

  let[@inline] fold f r (Iter i) =
    fold_state i.init f r i.next

  let bench () =
    let result =
      input
      |> of_list
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 5 = 0)
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 3 = 0)
      |> take input_len
      |> fold (+) 0
    in
    result
    (* assert (result = 8333616669) *)
end


module Iter_ultimate = struct
  type 'a iter =
      Iter : {
        init : unit -> 's;
        next : 'r . ('a -> 's -> 'r) -> 'r -> 's -> 'r;
        free : 's -> unit;
      } -> 'a iter

  let of_list l =
    let[@inline] next yield r s =
      match s with
      | [] -> r
      | a :: s' -> yield a s' in
    Iter { init = (fun () -> l); next; free = ignore }

  let filter predicate (Iter iter) =
    let[@inline] rec next yield r s =
      iter.next
        ((fun a s' ->
            if predicate a then yield a s'
            else next yield r s')) r s in
    Iter { iter with next }

  let map f (Iter iter) =
    let[@inline] next yield r s =
      iter.next (fun a s' -> yield (f a) s') r s in
    Iter { iter with next }

  let take n (Iter iter) =
    let[@inline] next yield r (s, i) =
      if i <= 0 then r
      else iter.next (fun a s' -> yield a (s', i - 1)) r s in
    Iter { init = (fun () -> (iter.init (), n));
           free = (fun (s, _) -> iter.free s);
           next }

  let[@inline] fold f r (Iter iter) =
    let[@inline] rec loop r s =
      iter.next (fun a s' -> loop (f a r) s') r s in
    bracket iter.init iter.free (loop r)

  let bench () =
    let result =
      input
      |> of_list
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 5 = 0)
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 3 = 0)
      |> take input_len
      |> fold (+) 0
    in
    result
    (* assert (result = 8333616669) *)
end



module Iter_k_unit = struct
  type 'a iter =
      Iter : {
        init : 'cursor;
        next : 'r . ('a -> 'cursor -> 'r) -> (unit -> 'r) -> 'cursor -> 'r;
      } -> 'a iter

  let of_list list =
    let[@inline] next yield stop cursor =
      match cursor with
      | []        -> stop ()
      | a :: rest -> yield a rest in
    Iter { init = list; next }

  let count n =
    let next yield empty cursor = yield cursor (cursor + 1) in
    Iter { init = n; next }

  let filter predicate (Iter i) =
    let[@inline] rec next yield empty cursor =
      i.next
        ((fun a cursor' ->
            if predicate a then yield a cursor'
            else next yield empty cursor'))
        empty
        cursor
    in
    Iter { i with next }

  let map f (Iter i) =
    let[@inline] next yield =
      i.next (fun a -> yield (f a))
    in
    Iter { i with next }

  let take n (Iter i) =
    let[@inline] next yield stop (cursor, count) =
      if count <= 0 then stop ()
      else i.next (fun a cursor' -> yield a (cursor', count - 1)) stop cursor in
    Iter { init = (i.init, n); next }

  let fold_state s0 f r0 next =
    let[@inline] rec go r s =
      next (fun a -> go (f a r)) (fun () -> r) s in
    go r0 s0

  let[@inline] fold f r (Iter i) =
    fold_state i.init f r i.next

  let bench () =
    let result =
      input
      |> of_list
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 5 = 0)
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 3 = 0)
      |> take input_len
      |> fold (+) 0
    in
    result
    (* assert (result = 8333616669) *)
end


module Fold = struct
  type 'a seq = { run : 'r . ('a -> 'r -> 'r) -> 'r -> 'r } [@@unboxed]

  let[@inline] filter p seq =
    let run f = seq.run (fun a r -> if p a then f a r else r) in
    { run }

  let[@inline] map f seq =
    let run g = seq.run (fun x -> g (f x)) in
    { run }

  let[@inline] take n seq =
    let run f z = seq.run (fun a r n -> if n <= 0 then z else f a (r (n - 1))) (fun _ -> z) n in
    { run }

  let[@inline] rec list_fold f r list =
    match list with
    | [] -> r
    | a :: rest -> list_fold f (f a r) rest

  let[@inline] to_list seq =
    seq.run (fun a r -> a :: r) []

  let[@inline] fold f r seq =
    seq.run f r

  let[@inline] of_list list = { run = fun f r -> list_fold f r list }

  let bench () =
    let result =
      input
      |> of_list
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 5 = 0)
      |> map (fun x -> x + 1)
      |> filter (fun x -> x mod 3 = 0)
      |> take input_len
      |> fold (+) 0 in
    result
    (* assert (result = 8333616669) *)
end


module Seq = struct
  type 'a t = ('a -> unit) -> unit

  let map f seq k =
    seq (fun x -> k (f x))

  let filter p seq k =
    seq (fun x -> if p x then k x)

  exception ExitTake

  let take n seq k =
    let count = ref 0 in
    try
      seq
        (fun x ->
           if !count = n then raise ExitTake;
           incr count;
           k x)
    with ExitTake -> ()

  let fold f acc seq =
    let r = ref acc in
    seq (fun elt -> r := f !r elt);
    !r

  let rec of_list l k =
    match l with
    | [] -> ()
    | x :: xs ->
      k x; of_list xs k
end


module Seq' = struct
  type 'a t = ('a -> bool) -> unit

  let map f seq k =
    seq (fun x -> k (f x))

  let filter p seq k =
    seq (fun x -> if p x then k x else true)

  let take n seq k =
    let count = ref 0 in
    seq
      (fun x ->
         if !count = n then false
         else (incr count; k x))

  let fold f acc seq =
    let r = ref acc in
    seq (fun elt -> (r := f !r elt); true);
    !r

  let rec of_list l k =
    match l with
    | [] -> ()
    | x :: xs ->
      if k x then of_list xs k else ()
end



let iter_opt () =
  let open Streams.Iterator.A in
  let result =
    input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 5 = 0)
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take input_len
    |> fold (+) 0
  in
  result
(* assert (result = 8333616669) *)

let klist_partial () =
  let open Streams.Partial_finite_klist in
  let result =
    input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 5 = 0)
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take input_len
    |> fold (+) 0
  in
  result
(* assert (result = 8333616669) *)

let stream_fusion () =
  let open Streams.Fusion_stream in
  let result =
    input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 5 = 0)
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take input_len
    |> fold (+) 0
  in
  result
(* assert (result = 8333616669) *)

let iter_k () =
  let open Iter_k in
  let result =
    input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 5 = 0)
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take input_len
    |> fold (+) 0
  in
  result
(* assert (result = 8333616669) *)

let iter_k_inline () =
  let open Iter_k_inline in
  let result =
    input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 5 = 0)
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take input_len
    |> fold (+) 0
  in
  result
(* assert (result = 8333616669) *)

let seq' () =
  let open Seq' in
  let result =
    input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 5 = 0)
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take input_len
    |> fold (+) 0 in
  result

let seq () =
  let open Seq in
  let result =
    input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 5 = 0)
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take input_len
    |> fold (+) 0 in
  result
(* assert (result = 8333616669) *)

let iter_skip () =
  let open Iter_skip in
  let result =
    input
    |> of_list
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 5 = 0)
    |> map (fun x -> x + 1)
    |> filter (fun x -> x mod 3 = 0)
    |> take input_len
    |> fold (+) 0 in
  result
(* assert (result = 8333616669) *)

let () =
  let open Core_bench.Std in
  print_endline "Benchmark for data processing libraries.";
  print_endline ("Input length = " ^ string_of_int input_len);
  let bench n = Bench.Test.create ~name:n in
  Core.Command.run (Bench.make_command [
      (* bench "klist_partial"        klist_partial; *)
      bench "iter"                 Iter.bench;
      (* bench "iter_skip"            iter_skip; *)
      (* bench "iter_k"               iter_k; *)
      bench "iter_ultimate"           Iter_ultimate.bench;
      (* bench "iter_k_inline"        iter_k_inline; *)
      bench "fold_list"            Fold.bench;
      bench "stream_fusion"        Stream_fusion.bench;
      bench "iter_k_unit"          Iter_k_unit.bench;
      (* bench "iter_k_unit_safe"     Iter_k_unit_safe.bench; *)
      (* bench "iter_safe_tmp"        Iter_safe_tmp.bench; *)
      bench "seq'"             seq';
      (* bench "seq"             seq; *)
      bench "transducer_tag"  Transducer_tag.bench;
      (* bench "transducer_k"    Transducer_k.bench; *)
    ])


