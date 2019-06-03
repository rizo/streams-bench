
type 'a t = {
  get : int -> 'a;
  start : int;
  stop : int;
}


let length slice =
  slice.stop - slice.start


let start slice =
  slice.start


let stop slice =
  slice.stop


let of_array a =
  let get idx = Array.get a idx in
  let start, stop = 0, Array.length a in
  { get; start; stop }


let to_array slice =
  let len = slice.stop - slice.start in
  if len = 0 then [||] else
  let out = Array.make len (slice.get slice.start) in
  for i = 1 to len - 1 do
    Array.set out i (slice.get (slice.start + i))
  done;
  out


(*
let range ?by:(step=1) ?from:(start=0) stop =
  failwith "TOOD"
*)

let to_list slice =
  let len = slice.stop - slice.start in
  if len = 0 then [] else
  let rec loop i out =
    if i >= slice.stop then out
    else loop (i + 1) (slice.get i :: out) in
  List.rev (loop slice.start [])


let map f slice =
  let get idx = f (slice.get idx) in
  { slice with get }


let get idx slice =
  let idx' = idx + slice.start in
  if idx' >= slice.start && idx' < slice.stop
    then Some (slice.get idx')
    else None


let first self  = get 0 self

let second self = get 1 self


let view f idx r self =
  let idx' = idx + self.start in
  if idx' >= self.start && idx' >= self.stop then r
  else f (self.get idx') (idx' + 1)


let last self =
  get (length self - 1) self


let slice start stop s =
  { s with start; stop }


let is_empty slice =
  length slice = 0


let append slice0 slice1 =
  let start = slice0.start in
  let stop  = slice0.stop + (length slice1) in
  let get idx =
    let idx'0 = idx + slice0.start in
    if idx'0 >= slice0.start && idx'0 < slice0.stop
    then slice0.get idx'0
    else
      let idx'1 = idx - length slice0 + slice1.start in
      if idx'1 >= slice1.start && idx'1 < slice1.stop
      then slice1.get idx'1
      else assert false in
  { start; stop; get }


let iter f slice =
  let rec loop i =
    if i >= slice.stop then () else
    let x = slice.get i in
    let () = f x in loop (i + 1) in
  loop slice.start


let chunks ~size slice =
  if size = 0 then [] else
    if size = 1 then [slice] else
      let rec loop i acc =
        let j = i + size in
        if j >= slice.stop then
          { slice with start=i } :: acc
    else
      loop j ({ slice with start = i; stop = j } :: acc) in
      List.rev (loop slice.start [])



module Reducers = Push_reducer_bool


let fold f acc0 slice =
  let rec loop idx acc =
    view (fun x idx' -> loop idx' (f acc x)) idx acc slice in
  loop slice.start acc0


let reduce_sequential (Reducers.Reducer reducer) slice =
  let rec loop i r =
    if i < slice.start || i >= slice.stop || reducer.full r then r
    else loop (i + 1) (reducer.step r (slice.get i)) in
  reducer.stop (loop slice.start (reducer.init ()))


let reduce_parallel (Reducers.Reducer reducer) slice0 =
  let rec reduce_chunk i r slice =
    if i < slice.start || i >= slice.stop || reducer.full r then r
    else reduce_chunk (i + 1) (reducer.step r (slice.get i)) slice in
  let (input, output) = Unix.pipe () in
  let acc0 = reducer.init () in
  let workers =
    List.fold_left
      (fun workers chunk ->
          match Unix.fork () with
          | 0 ->
            Unix.close input;
            let acc = reduce_chunk chunk.start acc0 chunk in
            let output' = Unix.out_channel_of_descr output in
            Marshal.to_channel output' acc [];
            exit 0;
          | pid -> pid :: workers) []
      (chunks ~size:(length slice0 / 4) slice0) in
  Unix.close output;
  let input = Unix.in_channel_of_descr input in
  let rec loop acc =
    try
      let acc' = Marshal.from_channel input in
      loop (reducer.step acc acc')
    with End_of_file -> acc in
  let res = loop acc0 in
  List.iter (fun pid -> ignore (Unix.waitpid [] pid)) workers;
  reducer.stop res


let reduce ?(parallel=false) reducer slice =
  if parallel then reduce_parallel reducer slice
  else reduce_sequential reducer slice


