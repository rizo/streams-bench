module Mk_concat (Stream : sig
  type 'a t

  val unfold : 's -> ('s -> ('a * 's) option) -> 'a t
  val concat : 'a t -> 'a t -> 'a t
  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
end) =
struct
  let ( -- ) i j =
    Stream.unfold i (fun x -> if x = j then None else Some (x, x + 1))

  let no_concat () =
    let open Stream in
    0 -- !Config.length |> fold ( + ) 0

  let concat_2 () =
    let open Stream in
    let size = !Config.length / 2 in
    concat (0 -- size) (size -- (size * 2)) |> fold ( + ) 0

  let concat_4 () =
    let open Stream in
    let size = !Config.length / 4 in
    concat
      (concat (0 -- size) (size -- (size * 2)))
      (concat ((size * 2) -- (size * 3)) ((size * 3) -- (size * 4)))
    |> fold ( + ) 0

  let concat_8 () =
    let open Stream in
    let size = !Config.length / 8 in
    concat
      (concat
         (concat (0 -- size) (size -- (size * 2)))
         (concat ((size * 2) -- (size * 3)) ((size * 3) -- (size * 4))))
      (concat
         (concat ((size * 4) -- (size * 5)) ((size * 5) -- (size * 6)))
         (concat ((size * 6) -- (size * 7)) ((size * 7) -- (size * 8))))
    |> fold ( + ) 0

  let concat_16 () =
    let open Stream in
    let size = !Config.length / 16 in
    concat
      (concat
         (concat
            (concat (0 -- size) (size -- (size * 2)))
            (concat ((size * 2) -- (size * 3)) ((size * 3) -- (size * 4))))
         (concat
            (concat ((size * 4) -- (size * 5)) ((size * 5) -- (size * 6)))
            (concat ((size * 6) -- (size * 7)) ((size * 7) -- (size * 8)))))
      (concat
         (concat
            (concat ((size * 8) -- (size * 9)) ((size * 9) -- (size * 10)))
            (concat ((size * 10) -- (size * 11)) ((size * 11) -- (size * 12))))
         (concat
            (concat ((size * 12) -- (size * 13)) ((size * 13) -- (size * 14)))
            (concat ((size * 14) -- (size * 15)) ((size * 15) -- (size * 16)))))
    |> fold ( + ) 0

  let concat_tests () =
    assert (!Config.length mod 2 = 0);
    let x = no_concat () in
    begin
      assert (x = concat_2 ());
      assert (x = concat_4 ());
      assert (x = concat_8 ());
      assert (x = concat_16 ())
    end
end

module Concat = struct
  module Push_reducer_bool = Mk_concat (Push_reducer_bool)
  module Push_unit = Mk_concat (Push_unit)
end

let concat_push_reducer_bool =
  [
    ("push_reducer_bool/no_concat", Concat.Push_reducer_bool.no_concat);
    ("push_reducer_bool/concat_2", Concat.Push_reducer_bool.concat_2);
    ("push_reducer_bool/concat_4", Concat.Push_reducer_bool.concat_4);
    ("push_reducer_bool/concat_8", Concat.Push_reducer_bool.concat_8);
    ("push_reducer_bool/concat_16", Concat.Push_reducer_bool.concat_16);
  ]

let concat_push_unit =
  [
    ("push_unit/no_concat", Concat.Push_unit.no_concat);
    ("push_unit/concat_2", Concat.Push_unit.concat_2);
    ("push_unit/concat_4", Concat.Push_unit.concat_4);
    ("push_unit/concat_8", Concat.Push_unit.concat_8);
    ("push_unit/concat_16", Concat.Push_unit.concat_16);
  ]

let concat_all = concat_push_reducer_bool @ concat_push_unit
