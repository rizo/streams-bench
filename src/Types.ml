
module type Stream = sig
  type 'a t

  val of_list : 'a list -> 'a t
  val singleton : 'a -> 'a t
  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r

  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val take : int -> 'a t -> 'a t

  val flat_map : ('a -> 'b t) -> 'a t -> 'b t

  val unfold : 's -> ('s -> ('a * 's) option) -> 'a t
end


module type Benchmarks = sig
  val basic : unit -> unit
  val map : unit -> unit
  val filter : unit -> unit
  val take : unit -> unit
  val flat_map : unit -> unit
  val fold : unit -> unit
  val unfold : unit -> unit
  val singleton : unit -> unit
end

