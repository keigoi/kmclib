[@@@warnerror "-22"]

open Kmclib

let KMC (ach,bch) = [%kmc.gen (a,b)]

let alice x : unit =
    let ach = send ach#b#msg x in 
    Printf.printf "Alice sent: %s\n" x;
    ach
 

let bob () =
    let `msg(txt, ()) = receive bch#a in
    Printf.printf "Bob received: %s\n" txt


let () = let athread = Thread.create alice ("Hello World") in 
         let bthread = Thread.create bob () in 
         Thread.join athread;
         Thread.join bthread