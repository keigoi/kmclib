(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

let rec ta () =
  `b(object 
      method left v = 
        Printf.printf "%d\n" v;
        `b(`ack((), ta ()))
      method right v =
        print_endline v;
        ()
    end)

let rec tb cnt =
  if cnt = 0 then
    `a(`right("finish", ()))
  else
    `a(`left(cnt, `a(object method ack () = tb (cnt-1) end)))

let () =
  let KMC (ah, bh) = [%kmc.gen.runner g (a,b)]
  in
  let t = Thread.create ah (ta ()) in
  bh (tb 10);
  Thread.join t

let f x = 
  x + 1
   
type 't local = {run: 'k. ('t -> 'k) -> 'k}


