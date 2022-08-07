(*
This file is stream_combinators.ml from:

    https://github.com/strymonas/staged-streams.ocaml

accompanying the paper

    Stream Fusion, to Completeness
    Oleg Kiselyov, Aggelos Biboudis, Nick Palladinos, Yannis Smaragdakis

The only modifications are to the syntax of staging (replacing MetaOCaml's
.<>. and .~() with ppx_stage's [%code] and [%e]), and the import of the
code type below, which is not predefined under ppx_stage.
*)

type 'a code = 'a Ppx_stage.code

(*
MIT License

Copyright (c) 2017 Oleg Kiselyov, Aggelos Biboudis, Nick Palladinos, Yannis Smaragdakis

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

(* This is a pull stream but it is used pretty much like a push stream.
    'a is the type of the eventually produced value,
    's is the overall state. 'a is not necessarily the code type!
    It could be a tuple of code types, for example.

    Stream producer is always linear: in the non-terminated state,
    when it updates the stream state it always produces a value.
    A stream may be either linear (when it is non-nested: bare producer)
    or not. Nested streams are non-linear: one update to the outer stream
    state may produce 0, 1 or more (nested) stream values.
    Linearity here refers to the linear use of stream's continuation.
    Filtering is modeled as a nested stream.

    Linear streams have special properties: commute with take and zip,
    for example.

    Of the generators, we distinguish indexing generators, which correspond
    to the for-loop. The general unfold generator corresponds to the while
    loop (or the tail-recursive function).
    The loop itself will be generated by 'fold' (as behooves to the pull
    stream). We assume that the fold's state is mutable, so we make no provisions
    to thread it through in the generator (although we may).

    A note on a possible alternative: turn the transformer of a nested
    stream in CPS.: 'b -> ('a stream -> unit code) -> unit code.
    Add an alternative for the Singleton producer. Then filter is implemented
    as a Nested that inserts the if-statement test and calls the continuation
    with the Singleto stream.

   XXX Generalize take_raw along the lines of fold_raw (need to pass
   z and the function to update z (with the return type unit code)
   and the function to build the term condition (bool code)). Perhaps
   z can be included into the closure. Then we can use the same take_raw
   to derive both take and takeWhile.

   Incidentally, takeWhile can be expressed in terms of zip_with,
   more specifically, zip_with_raw. Perhaps the ordinary take can be dealt
   with similarly. This points out the general character of the *_raw functions
   and the need to take them seriously. One can say they are the VM that Yannis
   likes to talk about.
   This is the benefit of staging: we can use abstractions
   and not pay for them.


   In general, we can move regular map, fold, take, etc. in a separate file,
   (along with sum and iota and enumFromTo, etc). The latter do not depend
   on the stream structure and internals.
*)

type card_t = AtMost1 | Many

(* We need the step function in CPS with the code answer type,
   so we can do let-insertion
*)

type ('a, 's) producer_t =
  | For of ('a, 's) producer_for
  | Unfold of ('a, 's) producer_unfold

and ('a, 's) producer_for = {
  upb : 's -> int code; (* exact upper bound *)
  index : 's -> int code -> ('a -> unit code) -> unit code;
}

and ('a, 's) producer_unfold = {
  term : 's -> bool code; (* when false, stop *)
  card : card_t;
  step : 's -> ('a -> unit code) -> unit code;
}

and 's init = { init : 'w. ('s -> 'w code) -> 'w code }
and 'a producer = Prod : 's init * ('a, 's) producer_t -> 'a producer

and _ st_stream =
  | Linear : 'a producer -> 'a st_stream
  | Nested : 'b producer * ('b -> 'a st_stream) -> 'a st_stream

and 'a stream = 'a code st_stream

(* Change the For producer to the general producer *)
let for_unfold : 'a producer -> 'a producer = function
  | Prod ({ init }, For { upb; index }) ->
    Prod
      ( {
          init =
            (fun k ->
              init @@ fun s0 ->
              [%code
                let i = ref 0 in
                [%e k ([%code i], s0)]]);
        },
        Unfold
          {
            term = (fun (i, s0) -> [%code ![%e i] <= [%e upb s0]]);
            card = Many;
            step =
              (fun (i, s0) k ->
                index s0 [%code ![%e i]] @@ fun a ->
                [%code
                  incr [%e i];
                  [%e k a]]);
          } )
  | x -> x

let of_arr : 'a array code -> 'a stream =
 fun arr ->
  let prod =
    Prod
      ( {
          init =
            (fun k ->
              [%code
                let arr = [%e arr] in
                [%e k [%code arr]]]);
        },
        For
          {
            upb = (fun arr -> [%code Array.length [%e arr] - 1]);
            index =
              (fun arr i k ->
                [%code
                  let el = [%e arr].([%e i]) in
                  [%e k [%code el]]]);
          } )
  in
  Linear prod

(* This interface is good for functional streams but not for loops *)
let unfold : ('z code -> ('a * 'z) option code) -> 'z code -> 'a stream =
 fun p z ->
  let prod =
    Prod
      ( {
          init =
            (fun k ->
              [%code
                let s = ref [%e p z] in
                [%e k [%code s]]]);
        },
        Unfold
          {
            term = (fun s -> [%code ![%e s] <> None]);
            card = Many;
            step =
              (fun s body ->
                [%code
                  match ![%e s] with
                  | Some (el, s') ->
                    [%e s] := [%e p [%code s']];
                    [%e body [%code el]]]);
          } )
  in
  Linear prod

(* Consumer. Only consumer runs the main loop -- or makes it *)

let rec fold_raw : 'a. ('a -> unit code) -> 'a st_stream -> unit code =
 fun consumer -> function
  | Linear (Prod ({ init }, For { upb; index })) ->
    init @@ fun sp ->
    [%code
      for i = 0 to [%e upb sp] do
        [%e index sp [%code i] @@ consumer]
      done]
  | Linear (Prod ({ init }, Unfold { term; card = AtMost1; step })) ->
    init @@ fun sp -> [%code if [%e term sp] then [%e step sp @@ consumer]]
  | Linear (Prod ({ init }, Unfold { term; step; _ })) ->
    init @@ fun sp ->
    [%code
      while [%e term sp] do
        [%e step sp @@ consumer]
      done]
  | Nested (prod, nestf) ->
    (* polymorphic recursion *)
    fold_raw (fun e -> fold_raw consumer @@ nestf e) (Linear prod)

let fold : ('z code -> 'a code -> 'z code) -> 'z code -> 'a stream -> 'z code =
 fun f z str ->
  [%code
    let s = ref [%e z] in
    [%e fold_raw (fun a -> [%code s := [%e f [%code !s] a]]) str];
    !s]

let fold_tupled :
    ('z1 code -> 'a code -> 'z1 code) ->
    'z1 code ->
    ('z2 code -> 'a code -> 'z2 code) ->
    'z2 code ->
    'a stream ->
    ('z1 * 'z2) code =
 fun f1 z1 f2 z2 str ->
  [%code
    let s1 = ref [%e z1] in
    let s2 = ref [%e z2] in
    [%e
      fold_raw
        (fun a ->
          [%code
            begin
              s1 := [%e f1 [%code !s1] a];
              s2 := [%e f2 [%code !s2] a]
            end])
        str];
    (!s1, !s2)]

(* Transformers *)
(* A general map, used for many things *)
(* We need the mapping function in CPS with the code answer type,
   so we can do let-insertion
*)
let rec map_raw :
          'a 'b.
          ('a -> ('b -> unit code) -> unit code) -> 'a st_stream -> 'b st_stream
    =
 fun tr -> function
  | Linear (Prod (init, For ({ index; _ } as g))) ->
    let index s i k = index s i @@ fun e -> tr e k in
    Linear (Prod (init, For { g with index }))
  | Linear (Prod (init, Unfold ({ step; _ } as g))) ->
    let step s k = step s @@ fun e -> tr e k in
    Linear (Prod (init, Unfold { g with step }))
  | Nested (prod, nestf) -> Nested (prod, fun a -> map_raw tr (nestf a))

let map : ('a code -> 'b code) -> 'a stream -> 'b stream =
 fun f str ->
  map_raw
    (fun a k ->
      [%code
        let t = [%e f a] in
        [%e k [%code t]]])
    str

let rec flat_map_raw : ('a -> 'b st_stream) -> 'a st_stream -> 'b st_stream =
 fun tr -> function
  | Linear prod -> Nested (prod, tr)
  | Nested (prod, nestf) -> Nested (prod, fun a -> flat_map_raw tr @@ nestf a)

let flat_map : ('a code -> 'b stream) -> 'a stream -> 'b stream = flat_map_raw

(* Filter is also implemented via flat_map *)
let filter : ('a code -> bool code) -> 'a stream -> 'a stream =
 fun f str ->
  let filter_stream a =
    Prod
      ( { init = (fun k -> k a) },
        Unfold { card = AtMost1; term = f; step = (fun a k -> k a) } )
  in
  flat_map_raw (fun x -> Linear (filter_stream x)) str

(* Add a new termination condition. We don't add to AtMost1 streams
   since those are always dependent and the termination check is
   redundant.
*)
let rec more_termination : bool code -> 'a st_stream -> 'a st_stream =
  let rec add_to_producer new_term = function
    | Prod (init, Unfold { card = Many; term; step }) ->
      let term s = [%code [%e new_term] && [%e term s]] in
      Prod (init, Unfold { card = Many; term; step })
    | Prod (_, Unfold { card = AtMost1; _ }) as p -> p
    | p -> add_to_producer new_term (for_unfold p)
  in
  fun new_term -> function
    | Linear p -> Linear (add_to_producer new_term p)
    | Nested (p, nestf) ->
      Nested
        ( add_to_producer new_term p,
          fun a -> more_termination new_term (nestf a) )

(* type ('a,'b) either = Left of 'a | Right of 'b *)

(* The nested stream receives the same remcount reference; thus
   all streams decrement the same global count.
*)
let take_raw : int code -> 'a st_stream -> 'a st_stream =
  let add_nr : 'a. int code -> 'a producer -> (int ref code * 'a) producer =
   fun n -> function
    | Prod ({ init }, Unfold { term; card; step }) ->
      let init k =
        init @@ fun s ->
        [%code
          let nr = ref [%e n] in
          [%e k ([%code nr], s)]]
      and prod =
        Unfold
          {
            card;
            (* For filter, we assume that is a dependent stream... *)
            term =
              (fun (nr, s) ->
                if card = Many then [%code ![%e nr] > 0 && [%e term s]]
                else term s);
            step = (fun (nr, s) k -> step s (fun el -> k (nr, el)));
          }
      in
      Prod ({ init }, prod)
  and update_nr (nr, el) k =
    [%code
      decr [%e nr];
      [%e k el]]
  in
  fun n -> function
    | Linear (Prod (init, For { upb; index })) ->
      let upb s = [%code min ([%e n] - 1) [%e upb s]] in
      Linear (Prod (init, For { upb; index }))
    | Linear p -> map_raw update_nr @@ Linear (add_nr n p)
    | Nested (p, nestf) ->
      Nested
        ( add_nr n (for_unfold p),
          fun (nr, a) ->
            map_raw (fun a -> update_nr (nr, a))
            @@ more_termination [%code ![%e nr] > 0] (nestf a) )

let take : int code -> 'a stream -> 'a stream = take_raw

(* Zipping *)
(* When zipping two streams with the step function step1 and step2
   (and assuming step1 is at least as long as step2), one may expect
   one of the following patterns of calling step1 and step2:
     step1 step2 step1 step2 step1 eof-step2
   or
     step2 step1 step2 step1 eof-step2
   We guarantee that step1 and step2 are called in the alternating
   pattern: one of the above calling sequences. Which of the two -- is
   generally undefined. The programmer has to beware, when step functions
   have side-effects.

   Care should be taken when zipping the nested stream with an
   ordinary stream.
   Another subtle point: we try to make a parallel loop, so to speak.
   However, the two streams may advance at different pace: they
   may skip at different times. So, if one stream yields an element
   and another skips, we should `back up' the element of the first
   stream. Since the state is imperative, we can only remember the
   element in the putback buffer. This way we make the streams advance
   at the same speed.

   XXX Keep in mind the complex cases of zipping:
      ofArr ... |> (fun x -> ofArr [|x;x+1;x+2|])
      ofArr ... |> (fun x -> ofArr [|x;x+2|])
   inner streams have different sizes; one one finishes, the second
   still has some elements, which should not be lost!
*)

let rec zip_producer : 'a producer -> 'b producer -> ('a * 'b) producer =
 fun p1 p2 ->
  match (p1, p2) with
  | Prod (i1, For f1), Prod (i2, For f2) ->
    Prod
      ( {
          init =
            (fun k ->
              i1.init @@ fun s1 ->
              i2.init @@ fun s2 -> k (s1, s2));
        },
        For
          {
            upb = (fun (s1, s2) -> [%code min [%e f1.upb s1] [%e f2.upb s2]]);
            index =
              (fun (s1, s2) i k ->
                f1.index s1 i @@ fun e1 ->
                f2.index s2 i @@ fun e2 -> k (e1, e2));
          } )
    (* XXX Need a special case for card = AtMost1? *)
  | Prod (i1, Unfold f1), Prod (i2, Unfold f2) ->
    Prod
      ( {
          init =
            (fun k ->
              i1.init @@ fun s1 ->
              i2.init @@ fun s2 -> k (s1, s2));
        },
        Unfold
          {
            card = Many;
            term = (fun (s1, s2) -> [%code [%e f1.term s1] && [%e f2.term s2]]);
            step =
              (fun (s1, s2) k ->
                f1.step s1 @@ fun e1 ->
                f2.step s2 @@ fun e2 -> k (e1, e2));
          } )
  | p1, p2 -> zip_producer (for_unfold p1) (for_unfold p2)

(* A linear stream is a producer *)
(* This is an auxiliary function, and used in specific circumstances.
   Although the pattern-matching is not exhaustive, there is no
   problem in the circumstances when this function is called.
*)
(* We introduce term1r to hold the value of the termination check
   for the current state of stream1. We have to do that check all
   the time for the nested substreams comprising stream2.
   The termination check may take time and is not necessarily
   idempotent.
*)
let push_linear :
    'a producer -> 'b producer * ('b -> 'c st_stream) -> ('a * 'c) st_stream =
 fun (Prod
       ({ init = init1 }, Unfold { card = Many; term = term1; step = step1 }))
     (Prod ({ init = init2 }, Unfold p2), nestf2) ->
  let init k =
    init1 @@ fun s1 ->
    init2 @@ fun s2 ->
    [%code
      let term1r = ref [%e term1 s1] in
      [%e k ([%code term1r], s1, s2)]]
  and prod =
    Unfold
      {
        card = Many;
        term = (fun (term1r, s1, s2) -> [%code ![%e term1r] && [%e p2.term s2]]);
        step =
          (fun (term1r, s1, s2) k -> p2.step s2 (fun b -> k (term1r, s1, b)));
      }
  in
  Nested
    ( Prod ({ init }, prod),
      fun (term1r, s1, b) ->
        map_raw (fun c k ->
            step1 s1 @@ fun a ->
            [%code
              [%e term1r] := [%e term1 s1];
              [%e k (a, c)]])
        @@ more_termination [%code ![%e term1r]] (nestf2 b) )

(* Make a stream linear.
   We have no choice but ro reify the stream: convert to a function
   that will, when called, produce the current element and advance the
   stream -- or report the end-of-stream.
   Befitting the imperative style of our implementation,
   the reified stream is an imperative *non-recursive* function, called
   adv, of unit->unit type. It reports the result in the mutable cell
   curr: after invoking adv (), curr contains either Some el, where
   el is the current element of the stream, or None (meaning the stream
   is finished).

   Nested streams are also reified to adv-like function. The mutable
   variable nadv contains (if not None) the adv function for the inner-most
   stream. When called, it places the current stream element in curr.
   The nested-stream adv functions may return without setting curr.
   They have to be called again.
   If the inner stream is finished, the adv function chages nadv to
   the adv function of the earlier stream.
   Thus we implement a sort of a trampoline mechanism. This is not
   actually needed in OCaml (which optimizes tail calls) but may be
   needed when the library is ported to other languages, where tail-call
   optimization is difficult or unreliable.
*)

let rec make_linear : 'a st_stream -> 'a producer = function
  | Linear prod -> prod
  | Nested ((Prod (init, For _) as p), nestf) ->
    make_linear (Nested (for_unfold p, nestf))
  | Nested (Prod ({ init }, Unfold { card = Many; term; step }), nestf) ->
    (* Make the adv function for the nested stream *)
    let rec make_adv
              : 'a.
                (unit -> unit) option ref code ->
                ('a -> unit code) ->
                'a st_stream ->
                unit code =
     (* Upon return:
        - ncurr is set to the current stream value and nadv stack
          is possibly updated with the function to call to get next.
        - ncurr is set to None; the parent has to repeat
     *)
     fun nadv k -> function
      | Linear prod -> begin
        match for_unfold prod with
        (* Filter *)
        | Prod ({ init }, Unfold { card = AtMost1; term; step }) ->
          init @@ fun s -> [%code if [%e term s] then [%e step s k]]
          (* Linear nested component *)
          (* XXX We can optimize here for the 1st-level stream:
             where we know that old_adv in None
          *)
        | Prod ({ init }, Unfold { term; step; _ }) ->
          init @@ fun s ->
          [%code
            let old_adv = ![%e nadv] in
            let adv1 () =
              if [%e term s] then [%e step s k] else [%e nadv] := old_adv
            in
            [%e nadv] := Some adv1;
            adv1 ()]
      end
      | Nested (prod, nestf) ->
        make_adv nadv (fun e -> make_adv nadv k @@ nestf e) @@ Linear prod
    in
    let init k =
      init @@ fun s0 ->
      [%code
        let curr = ref None in
        (* Current element, if any *)
        let nadv = ref None in
        (* The step of the innermost stream *)
        (* This is the adv for the outer stream *)
        (* It really tries to obtain the current element
           (of the innermost nested stream) and place it to curr.
           If curr remains None, the stream is finished.
        *)
        let adv () =
          curr := None;
          while !curr = None && (!nadv <> None || [%e term s0]) do
            match !nadv with
            | Some adv -> adv ()
            | None ->
              [%e
                step s0 @@ fun e0 ->
                make_adv [%code nadv] (fun e -> [%code curr := Some [%e e]])
                @@ nestf e0]
          done
        in
        adv ();
        [%e k ([%code curr], [%code adv])]]
    and term (curr, _) = [%code ![%e curr] <> None]
    and step (curr, adv) k =
      [%code
        match ![%e curr] with
        | Some el ->
          [%e adv] ();
          [%e k [%code el]]]
    in
    Prod ({ init }, Unfold { card = Many; term; step })

(* The dispatcher for zip *)
let rec zip_raw : 'a st_stream -> 'b st_stream -> ('a * 'b) st_stream =
 fun str1 str2 ->
  match (str1, str2) with
  | Linear prod1, Linear prod2 -> Linear (zip_producer prod1 prod2)
  (* Suppose str1 is linear and str2 is not. Linear stream
     can always be pushed inside
  *)
  | Linear prod1, Nested (prod2, nestf2) ->
    push_linear (for_unfold prod1) (for_unfold prod2, nestf2)
  | Nested (prod1, nestf1), Linear prod2 ->
    map_raw (fun (y, x) k -> k (x, y))
    @@ push_linear (for_unfold prod2) (for_unfold prod1, nestf1)
  (* If both streams are non-linear, make at least on of them linear *)
  | str1, str2 -> zip_raw (Linear (make_linear str1)) str2

let zip_with :
    ('a code -> 'b code -> 'c code) -> 'a stream -> 'b stream -> 'c stream =
 fun f str1 str2 -> map_raw (fun (x, y) k -> k (f x y)) @@ zip_raw str1 str2
