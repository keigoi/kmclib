open Kmclib
open Printf

let KMC (uch,mch,wch) = [%kmc.gen (u,m,w)] 

let rec fib n = (* omitted expensive function *) if n = 0 then 0 else if n = 1 then 1 else fib (n-2) + fib (n-1)

let user () =
  let uch = send uch#m#compute 42 in
  let rec loop uch : unit =
    match receive uch#m with
    | `wip(x, uch) -> 
      printf "in progress: %d\n" x; 
      loop uch
    | `result(res, uch) -> 
      printf "result: %d\n" res; 
      send uch#m#stop ()
  in loop uch

let worker () =
  let rec loop (wch : [%kmc.check w]) : unit =
    match receive wch#m with
    | `task(num, wch) ->
      loop (send wch#m#result (fib num))
    | `stop((), wch) -> wch
  in loop wch
let master () : unit = 
  let rec loop (mch : [%kmc.check m]) : unit =
    match receive mch#u with
    | `compute(x, mch) ->
      let mch = send mch#w#task (x - 2) in
      let mch = send mch#w#task (x - 1) in
      let `result(r1, mch) = receive mch#w in
      let mch = send mch#u#wip r1 in
      let `result(r2, mch) = receive mch#w in
      loop (send mch#u#result (r1 + r2))
    | `stop((), mch) ->
      send mch#w#stop ()
  in loop mch

let () =
  let ut = Thread.create user () in
  let mt = Thread.create master () in
  let wt = Thread.create worker () in
  List.iter Thread.join [ut;mt;wt]
