(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Jrklib

let t1 ch1 () : unit =
  let rec loop (ch1 : [%kmc.infer a]) cnt =
    if cnt = 0 then
      send ch1#b#right "done"
    else begin
      let `foo((_:int),ch1) =  receive (send ch1#b#left cnt)#b in
      loop ch1 (cnt-1)
    end
  in 
  print_endline "thread 1 started";
  loop ch1 10

let t2 (ch2 : [%kmc.infer b]) () =
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

[@@@kmc.check a, b]

let () =
  (* role A: rec ta . {B!left<int>;B?foo<int>;ta, B!right<string>;end}; 
     role B: rec tb . {A?left<int>;A!foo<int>;tb, A?right<string>;end}; 
   *)
  let ch1, ch2 = [%kmc.gen a, b] in
  List.iter Thread.join @@ List.map (fun f -> Thread.create f ()) [t1 ch1; t2 ch2]
