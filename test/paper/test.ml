let KMC (uch,mch,wch) = [%kmc.gen (u,m,w)]

let rec f n = (* omitted expensive function *) if n = 0 then 0  else if n = 1 then 1  else f (n-2) + f (n-1)

let user () : unit =
  let uch = send uch#m#compute 20 in
  let `result(res, uch) = receive uch#m in
  Printf.printf "result: %d\n" res;
  send uch#m#stop ()
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
  let `result(r2, mch) = receive mch#w in
  let mch = send mch#u#result (r1 + r2) in
  let `stop((), mch) = receive mch#u in
  send mch#w#stop ()

let () =
  let ut = Thread.create user () in
  let mt = Thread.create master () in
  let wt = Thread.create worker () in
  List.iter Thread.join [ut;mt;wt]
