let KMC (uch,mch,wch) = [%kmc.gen (u,m,w)]

let rec f n = (* omitted expensive function *) if n = 0 then 0  else if n = 1 then 1  else f (n-2) + f (n-1)

let user () : unit =
  let uch = send uch#m#compute 20 in
  let `result(res, uch) = receive uch#m in
  Printf.printf "result: %d\n" res;
  send uch#m#stop ()

let worker () : unit =
  let rec loop (wch : [%kmc.check w]) =
    match receive wch#m with
    | `task(num, wch) ->
      loop (send wch#m#result (f num))
    | `stop((), wch) -> wch
  in loop wch
let master () : unit =
  let rec loop (mch : [%kmc.check m]) =
    match receive mch#u with
    | `compute(x, mch) ->
      let mch = send mch#w#task (x / 2) in
      let mch = send mch#w#task (x / 4) in
      let `result(r1, mch) = receive mch#w in
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
