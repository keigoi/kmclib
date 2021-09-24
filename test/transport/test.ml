open Jrklib

let ch = Domainslib.Chan.make_unbounded ()
(* let ch = Domainslib.Chan.make_bounded 10 *)

let msg = {make_var=(fun v -> `msg(v)); match_var=(function `msg(v) -> Some(v) | _ -> None)}

let out, inp = 
  Jrklib.Internal.make ch () (msg,())

let t1 () =
  print_endline "thread 1 started.";
  let () = send out 100 in
  ()

let t2 () =
  print_endline "thread 2 started.";
  match receive inp with
  | `msg(v,()) -> print_endline @@ string_of_int v

let () =
  List.iter Thread.join @@ List.map (fun f -> Thread.create f ()) [t1; t2]


let t1 ch1 () =
  print_endline "thread 1 started";
  let rec loop ch1 cnt =
    if cnt = 0 then
      send ch1#right "finish"
    else
      loop (send ch1#left cnt) (cnt-1)
  in loop ch1 10

let t2 ch2 () =
  print_endline "thread 2 started";
  let rec loop ch2 =
    match receive ch2 with
    |`left(v,ch2) ->
      print_endline @@ string_of_int v;
      loop ch2
    |`right(v,()) ->
      print_endline v
  in
  loop ch2


let left = {make_var=(fun v -> `left(v)); match_var=(function `left(v) -> Some(v) | _ -> None)}
let right = {make_var=(fun v -> `right(v)); match_var=(function `right(v) -> Some(v) | _ -> None)}

let ch1, ch2 =
  let ch = Domainslib.Chan.make_unbounded () in
  let rec ch1 =
    lazy begin
      object method left = Lazy.force_val outl method right = Lazy.force_val outr end
    end
  and outl = lazy (Internal.make_out ch left (Lazy.force_val ch1))
  and outr = lazy (Internal.make_out ch right ())
  in
  let rec ch2 = lazy begin
      Internal.merge_inp (Internal.make_inp_lazy ch (left,ch2)) (Internal.make_inp ch (right,()))
    end
  in
  ignore (Lazy.force outl); 
  ignore (Lazy.force outr);
  Lazy.force ch1, Lazy.force ch2

let () =
  List.iter Thread.join @@ List.map (fun f -> Thread.create f ()) [t1 ch1; t2 ch2]
