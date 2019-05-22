type (+'a, 'state) unfold = {
  unfold: 'r.
            'state ->
    on_done:(unit -> 'r) ->
    on_skip:('state -> 'r) ->
    on_yield:('state -> 'a -> 'r) ->
    'r
}

type +_ t = CPS : 'state * ('a, 'state) unfold -> 'a t

let empty = CPS ((), {unfold=(fun () ~on_done ~on_skip:_ ~on_yield:_ -> on_done ())})

let return x = CPS ((), {
    unfold=(fun () ~on_done:_ ~on_skip:_ ~on_yield -> on_yield () x)
  })

let map f (CPS (st, u)) = CPS (st, {
    unfold=(fun st ~on_done ~on_skip ~on_yield ->
        u.unfold
          st
          ~on_done
          ~on_skip
          ~on_yield:(fun st x -> on_yield st (f x))
      );
  })

let fold f acc (CPS (st,u)) =
  let rec loop st acc =
    u.unfold
      st
      ~on_done:(fun _ -> acc)
      ~on_skip:(fun st' -> loop st' acc)
      ~on_yield:(fun st' x -> let acc = f x acc in loop st' acc)
  in
  loop st acc

let to_list_rev iter = fold (fun x acc -> x::acc) [] iter

let of_list l = CPS (l, {
    unfold=(fun l ~on_done ~on_skip:_ ~on_yield -> match l with
        | [] -> on_done ()
        | x :: tail -> on_yield tail x
      );
  })

let (--) i j = CPS (i, {
    unfold=(fun i ~on_done ~on_skip:_ ~on_yield ->
        if i>j then on_done ()
        else on_yield (i+1) i
      );
  })

let filter f (CPS (st, u1)) =
  CPS (st, {
      unfold=(fun st ~on_done ~on_skip ~on_yield ->
          u1.unfold st ~on_done ~on_skip
            ~on_yield:(fun st' x ->
                if f x then on_yield st' x
                else on_skip st'
              )
        );
    })

let flat_map : type a b. (a -> b t) -> a t -> b t
  = fun f (CPS (st1, u1)) ->
    (* obtain next element of u1 *)
    let u = {
      unfold=(fun (st1, CPS (sub_st2, sub2))
                ~on_done ~on_skip ~on_yield ->
                let done_ () =
                  u1.unfold st1
                    ~on_done
                    ~on_skip:(fun st1' -> on_skip (st1', empty))
                    ~on_yield:(fun st1' x1 -> on_skip (st1', f x1))
                in
                let skip sub_st2 = on_skip (st1, CPS (sub_st2, sub2)) in
                let yield_ sub_st2 x2 = on_yield (st1, CPS (sub_st2,sub2)) x2 in
                sub2.unfold sub_st2 ~on_done:done_ ~on_skip:skip ~on_yield:yield_
              );
    }
    in
    CPS ((st1, empty), u)


let take _n _self = failwith "not implemented"

