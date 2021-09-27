(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Jrklib

let t1 ch1 () : unit =
  print_endline "thread 1 started";
  let rec loop ch1 cnt =
    if cnt = 0 then
      send ch1#b#right "done"
    else begin
      let `foo((_:string),ch1) = receive (send ch1#b#left cnt)#b in
      loop ch1 (cnt-1)
    end
  in loop ch1 10

let t2 ch2 () =
  print_endline "thread 2 started";
  let rec loop ch2 =
    match receive ch2#a with
    |`left(v,ch2) ->
      print_endline @@ string_of_int v;
      loop (send ch2#a#foo 100)
    |`right(v,()) ->
      print_endline v
  in
  loop ch2

let () =
  let ch1, ch2 = ([%jrklib (a,b)]) in
  List.iter Thread.join @@ List.map (fun f -> Thread.create f ()) [t1 ch1; t2 ch2]
