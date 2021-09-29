(* don't make preprocessor warnings as errors *)
[@@@warnerror "-22"]

open Jrklib

[@@@jrklib.system g (a,b)]

let t1 ch1 () : unit =
  print_endline "thread 1 started";
  let rec loop (ch1 (*: [%jrklib.role g a]*) (*:<b: <left: (_, [`this_should_be_used_as_input]) out; ..>; ..>*)) cnt =
    if cnt = 0 then
      send ch1#b#right "done"
    else begin
      let `foo((_:int),ch1) =  receive (send ch1#b#left cnt) in
      loop ch1 (cnt-1)
    end
  in loop ch1 10

let t2 (ch2 : 'x(*[%jrklib.role g b]*)) () =
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
  (* role A: rec ta . {B!left<int>;B?foo<int>;ta, B!right<string>;end}; 
     role B: rec tb . {A?left<int>;A!foo<int>;tb, A?right<string>;end}; 
   *)
  let ch1, ch2 = [%jrklib (a,b)] in
  List.iter Thread.join @@ List.map (fun f -> Thread.create f ()) [t1 ch1; t2 ch2]
