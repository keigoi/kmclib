(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Jrklib

let f1 (ch1 : [%kmc.check g.a]) () : unit =
  let ch1 = send ch1#b#msg1 () in
  let ch1 = send ch1#b#msg2 () in
  let `msg3((), ch1) = receive (ch1)#b in
  let ch1 = send ch1#b#msg4 () in
  close ch1

let f2 (ch2 : [%kmc.check g.b]) () =
  let `msg1((), ch2) = receive ch2#a in
  let `msg2((), ch2) = receive ch2#a in
  let ch2 = send ch2#a#msg3 () in
  (* receive ch2#a *)
  close ch2

let () =
  let (ch1, ch2) = [%kmc.gen g (a, b)] in
  let t1 = Thread.create (f1 ch1) ()
  and t2 = Thread.create (f2 ch2) ()
  in
  Thread.join t1;
  Thread.join t2
