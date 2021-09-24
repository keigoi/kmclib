(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Jrklib

let t1 ch1 () : unit =
  print_endline "thread 1 started";
  let loop ch1 cnt =
    if cnt = 0 then
      send ch1#b#right "finish"
    else
      send ch1#b#left cnt
      (* loop (send ch1#b#left cnt) (cnt-1) *)
  in loop ch1 10

let t2 ch2 () =
  print_endline "thread 2 started";
  let loop ch2 =
    match receive ch2#a with
    |`left(v,()) ->
      print_endline @@ string_of_int v;
      ()
      (* loop ch2 *)
    |`right(v,()) ->
      print_endline v
  in
  loop ch2

let () =
  (* let ch1, ch2 = [%jrklib ~bound:`Infer] in *)
  List.iter Thread.join @@ List.map (fun f -> Thread.create f ()) [t1 [%jrklib]; t2 [%jrklib]]