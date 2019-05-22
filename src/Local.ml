

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

let id x = x
