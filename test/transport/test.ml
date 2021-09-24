open Jrklib

let ch = Domainslib.Chan.make_unbounded ()

let msg = {make_var=(fun v -> `msg(v)); match_var=(function `msg(v) -> Some(v) | _ -> None)}

;;

let out, inp = Jrklib.Internal.make ch () (msg,())

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
