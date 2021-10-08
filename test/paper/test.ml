(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Jrklib

let KMC (uch,mch,wch) = [%kmc.gentop g (u,m,w)]

(*
  let g = fix(fun t -> 
      choice_at u ##
      (u, (u --> m) compute @@
          (m --> w) task @@
          (w --> m) result @@
          (m --> w) task @@
          (w --> m) result @@
          (m --> u) result @@
          t)
      (u, (u --> m) stop @@
          (m --> w) stop @@
          finish))
    )
*)

let rec fib n =
  if n = 1 then 1
  else if n = 2 then 1
  else fib (n-2) + fib (n-1)

let user () =
  let uch = send uch#m#compute 20 in
  let `result(res, uch) = receive uch#m in
  Printf.printf "result: %d\n" res;
  close (send uch#m#stop ())

let master () =
  let rec loop (mch : [%kmc.check g.m]) =
    match receive mch#u with
    | `compute(x, mch) ->
      let mch = send mch#w#task (x / 2) in
      let mch = send mch#w#task (x / 4) in
      let y = fib x in
      let `result(r1, mch) = receive mch#w in
      let `result(r2, mch) = receive mch#w in
      loop (send mch#u#result (y + r1 + r2))
    | `stop((), mch) ->
      close (send mch#w#stop ())
  in
  loop mch

let worker () =
  let rec loop wch =
    match receive wch#m with
    | `task(num, wch) ->
      loop (send wch#m#result (fib num))
    | `stop((), wch) ->
      close wch
  in
  loop wch

let () =
  let ut = Thread.create user () in
  let mt = Thread.create master () in
  let wt = Thread.create worker () in
  List.iter Thread.join [ut;mt;wt]
