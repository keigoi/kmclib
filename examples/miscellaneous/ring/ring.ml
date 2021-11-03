(* To run: 
 * $ dune exec examples/miscellaneous/ring/ring.exe
 *)
(*
 From https://github.com/keigoi/ocaml-mpst/blob/master/examples/mpst/ring.ml
*) 

open Kmclib

let KMC (ea,eb,ec) = [%kmc.gen (a,b,c)]

let tA =
  Thread.create (fun () ->
      print_endline "A start";
      let ea = send (ea#b#msg) () in
      let `msg((), ea) = receive (ea#c) in
      print_endline "A done";
      close ea
    ) ()

let tB =
  Thread.create (fun () ->
      print_endline "B start";
      let `msg((), eb) = receive (eb#a) in
      let eb = send (eb#c#msg) () in
      print_endline "B done";
      close eb
    ) ()

let tC =
  Thread.create (fun () ->
      print_endline "C start";
      let `msg((), ec) = receive (ec#b) in
      let ec = send (ec#a#msg) () in
      print_endline "C done";
      close ec
    ) ()

let () =
    List.iter Thread.join [tA; tB; tC]