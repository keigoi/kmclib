open Kmclib

let rec fib n =
  if n = 0 then
    0
  else if n = 1 then
    1
  else
    fib (n - 2) + fib (n - 1)

let rec server (sch : [%kmc.check s]) : unit =
  match receive sch#c with
  | `compute (num, sch) ->
    let sch = send sch#c#result (fib num) in
    server sch
  | `stop ((), sch) -> sch

let client cch : unit =
  let cch = send cch#s#compute 42 in
  let (`result (res, cch)) = receive cch#s in
  Printf.printf "%d\n" res;
  send cch#s#stop ()

let () =
  let (KMC (chc, sch)) = [%kmc.gen c, s] in
  ignore (Thread.create (fun () -> server sch) ());
  client chc
