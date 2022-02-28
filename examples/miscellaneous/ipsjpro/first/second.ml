open Kmclib

let rec fib n =
  if n = 0 then
    0
  else if n = 1 then
    1
  else
    fib (n - 2) + fib (n - 1)

let (KMC (cch, sch)) = [%kmc.gen c, s]

let server () : unit =
  let rec loop sch : unit =
    match receive sch#c with
    | `compute (num, sch) ->
      let sch = send sch#c#result (fib num) in
      loop sch
    | `stop ((), sch) -> sch
  in
  loop sch

let client () : unit =
  let cch = send cch#s#compute 42 in
  let (`result (res, cch)) = receive cch#s in
  Printf.printf "%d\n" res;
  send cch#s#stop ()

let () =
  ignore (Thread.create server ());
  client ()
