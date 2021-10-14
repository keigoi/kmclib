let KMC (uch,mch,wch) = [%kmc.gen (u,m,w)]

let rec f n = (* omitted expensive function *) if n = 0 then 0  else if n = 1 then 1  else f (n-2) + f (n-1)

let user () =
  let uch = send uch#m#compute 42 in
  let rec get_all uch : unit =
    match receive uch#m with
    | `data(res, uch) ->
      Printf.printf "result: %d\n" res;
      get_all uch
    | `eos((), uch) -> uch
  in get_all uch

let worker () =
  let rec loop (wch : [%kmc.check w]) : unit =
    match receive wch#m with
    | `task(num, wch) ->
      loop (send wch#m#result (f num))
    | `stop((), wch) -> wch
  in loop wch
let master () : unit =
  let (mch : [%kmc.check m]) = mch in
  let `compute(x, mch) = receive mch#u in
  let mch = send mch#w#task (x / 2) in
  let mch = send mch#w#task (x / 4) in
  let `result(r1, mch) = receive mch#w in
  let mch = send mch#u#data (r1:int) in
  let `result(r2, mch) = receive mch#w in
  let mch = send mch#u#data (r2:int) in
  let mch = send mch#w#stop () in
  send mch#u#eos ()

let () =
  let ut = Thread.create user () in
  let mt = Thread.create master () in
  let wt = Thread.create worker () in
  List.iter Thread.join [ut;mt;wt]
