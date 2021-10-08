(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n-2) + fib (n-1)

let user () =
  `m(`compute(20,
  `m(object method result v =
      Printf.printf "%d\n" v;
      `m(`stop((), ()))
    end)))

let rec master () =
  `u(object
    method compute x =
      `w(`task(x / 2,
      `w(`task(x / 4,
      let y = fib x in
      `w(object method result r1 =
          `w(object method result r2 =
            `u(`result(y + r1 + r2, master ()))
          end)
        end)))))
    method stop () = 
      `w(`stop((), ()))
    end)

let rec worker () =
  `m(object
      method task x =
        `m(`result(fib x, worker ()))
      method stop () =
          ()
    end)

let () =
  let KMC (uh, mh, wh) = [%kmc.gen.runner g (u,m,w)] in
  let ut = Thread.create uh (user ())
  and mt = Thread.create mh (master ())
  and wt = Thread.create wh (worker ())
  in
  List.iter Thread.join [ut;mt;wt]
