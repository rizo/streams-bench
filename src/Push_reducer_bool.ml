let open_process_in x =
  prerr_endline "[TRACE] Unix.open_process_in";
  Unix.open_process_in x

let open_in x =
  prerr_endline "[TRACE] open_in";
	Pervasives.open_in x

let close_in x =
	prerr_endline "[TRACE] close_in";
	Pervasives.close_in x


(* --- *)

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



let null =
  Reducer {
    init = (fun () -> ());
    step = (fun () _ -> ());
    full = (fun () -> true);
    stop = (fun () -> ());
  }

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


let rec waitpid flags pid = try Unix.waitpid flags pid with
  | Unix.Unix_error (Unix.EINTR, _, _) -> waitpid flags pid

let rec unix_read fd bytes i n = try Unix.read fd bytes i n with
  | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd bytes i n

let rec create_process prog args ~stdin ~stdout ~stderr =
  try Unix.create_process prog args stdin stdout stderr with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      create_process prog args stdin stdout stderr


let file path =
  let ic = lazy (open_in path) in
	let reduce (Reducer k) =
		let rec loop r =
      if k.full r then r
      else try
				let x = input_line (Lazy.force ic) in
				loop (k.step r x)
			with End_of_file -> r in
    let stop r =
      if Lazy.is_val ic then
        close_in (Lazy.force ic);
      k.stop r in
    bracket loop ~init:k.init ~stop in
	{ reduce }


let files paths =
  flat_map file paths


let read_process cmd =
	let reduce (Reducer k) =
    let ic = open_process_in cmd in
		let rec loop r=
			if k.full r then r
      else try
				let x = input_line ic in
				loop (k.step r x)
			with End_of_file -> r in
    bracket loop ~init:k.init
      ~stop:(fun r -> close_in ic; k.stop r) in
	{ reduce }

let pipe name args ?(env = []) self =
  let reduce (Reducer k) =
    let (send_r, send_w) = Unix.pipe () in
    let recv_r = ExtUnix.All.posix_openpt [Unix.O_RDWR; Unix.O_NOCTTY] in
    ExtUnix.All.grantpt recv_r;
    ExtUnix.All.unlockpt recv_r;
    let recv_w_tty = ExtUnix.All.ptsname recv_r in
    let output_buf = Bytes.make 4096 '\000' in
    let feed str =
      let line = Bytes.of_string (str ^ "\n") in
      let line_len = Bytes.length line in
      let wn = Unix.write send_w line 0 line_len in
      if wn <> line_len then prerr_endline "incorrect write to pipe";
      let rn = Unix.read recv_r output_buf 0 99 in
      Bytes.to_string (Bytes.sub output_buf 0 (rn - 1)) in
    match Unix.fork () with
    | 0 ->
      Unix.close send_w;
      Unix.close recv_r;

      (* Setup pipe redirect for STDIN. *)
      Unix.close Unix.stdin;
      Unix.dup2 send_r Unix.stdin;
      Unix.close send_r;

      (* Setup tty redirect for STDOUT. *)
      let recv_w = Unix.openfile recv_w_tty [Unix.O_RDWR; Unix.O_NOCTTY] 0o640 in
      Unix.close Unix.stdout;
      Unix.dup2 recv_w Unix.stdout;
      Unix.close recv_w;
      Unix.execve name (Array.of_list (name :: args)) (Array.of_list env)
    | pid ->
      (* Close unused pipe ends. *)
      Unix.close send_r;
      let step acc x = (k.step acc (feed x)) in
      let stop acc =
        Unix.kill pid Sys.sigint;
        let _ = waitpid [] pid in
        Printf.eprintf "[TRACE] Killed %d\n%!" pid;
        k.stop acc in
      self.reduce (Reducer { k with step; stop })
  in
  { reduce }

let grep = pipe "/usr/bin/grep"


