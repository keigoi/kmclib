(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Jrklib

let f1 ch1 () : unit =
  let rec loop (ch1 : [%kmc.infer]) cnt =
    if cnt = 0 then
      close (send ch1#b#right "done")
    else begin
      let `foo((_:int),ch1) =  receive (send ch1#b#left cnt)#b in
      loop ch1 (cnt-1)
    end
  in 
  print_endline "thread 1 started";
  loop ch1 10

let f2 ch2 () =
  let rec loop ch2 =
    match receive ch2#a with
    |`left(v,ch2) ->
      print_endline @@ string_of_int v;
      loop (send ch2#a#foo 100)
    |`right(v,()) ->
      print_endline v
  in
  print_endline "thread 2 started";
  loop ch2

let () =
  (* role A: rec ta . {B!left<int>;B?foo<int>;ta, B!right<string>;end}; 
     role B: rec tb . {A?left<int>;A!foo<int>;tb, A?right<string>;end}; 
   *)
  let (ch1, ch2) = [%kmc.gen a, b] in
  (* List.iter Thread.join @@ List.map (fun f -> Thread.create f ()) [f1 ch1; f2 ch2] *)
  let t1 = Thread.create (f1 ch1) ()
  and t2 = Thread.create (f2 ch2) ()
  in
  Thread.join t1;
  Thread.join t2
