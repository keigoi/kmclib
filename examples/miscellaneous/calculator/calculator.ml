(* To run: 
 * $ dune exec examples/miscellaneous/calculator/calculator.exe 
 *)
(*
 From https://github.com/keigoi/ocaml-mpst/blob/master/examples/mpst/calc.ml
*) 

open Kmclib

type op = Add of int | Sub of int | Mul of int | Div of int

let KMC (ec,es) = [%kmc.gen (c,s)]

let tCli () =
  (* let _ : 'ec ty = get_ty cli calc in *)
  let ec = send ec#s#compute (Add 20) in
  let ec = send ec#s#compute (Sub 45) in
  let ec = send ec#s#compute (Mul 10) in
  let ec = send ec#s#result () in
  let `answer(ans, ec) = receive ec#s in
  let () = close ec in
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  ()

let tSrv () =
  (* let _ : 'es ty = get_ty srv calc in *)
  let rec loop acc (es : 'es) =
    match receive es#c with
    | `compute(sym, es) ->
      let acc = match sym with
        | Add n -> acc + n  | Sub n -> acc - n
        | Mul n -> acc * n | Div n -> acc / n
      in loop acc es
    | `result((), es) ->
      let es = send (es#c#answer) acc in
      close es
  in loop 0 es

let () =
  let ts = List.map (fun f -> Thread.create f ()) [tCli; tSrv] in
  List.iter Thread.join ts
