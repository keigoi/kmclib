(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

type image = unit

open Jrklib

let rec fib n =
  if n = 1 then 1
  else if n = 2 then 1
  else fib (n-2) + fib (n-1)

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

let user (ch : [%kmc.check g.u]) () =
  let ch = send ch#m#compute 20 in
  let `result(res, ch) = receive ch#m in
  Printf.printf "result: %d\n" res;
  close (send ch#m#stop ())

let rec master (ch : [%kmc.check g.m]) () =
  match receive ch#u with
  | `compute(x, ch) ->
    let ch = send ch#w#task (x / 2) in
    let ch = send ch#w#task (x / 4) in
    let y = fib x in
    let `result(r1, ch) = receive ch#w in
    let `result(r2, ch) = receive ch#w in
    master (send ch#u#result (y + r1 + r2)) ()
  | `stop((), ch) ->
    close (send ch#w#stop ())

let rec worker (ch : [%kmc.check g.w]) () =
  match receive ch#m with
  | `task(num, ch) ->
    worker (send ch#m#result (fib num)) ()
  | `stop((), ch) ->
    close ch

let () =
  let (uch,mch,wch) = [%kmc.gen g (u,m,w)] in
  let ut = Thread.create (user uch) () in
  let mt = Thread.create (master mch) () in
  let wt = Thread.create (worker wch) () in
  List.iter Thread.join [ut;mt;wt]
