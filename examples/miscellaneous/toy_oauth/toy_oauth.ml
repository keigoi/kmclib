(*
 examples from from K. Imai, R. Neykova, N. Yoshida and S. Yuen,
 "Multiparty Session Programming with Global Protocol Combinators",
 2020.
*) 

open Kmclib

let KMC (c,s,a) = [%kmc.gen (c,s,a)]


let cliThread () : unit =
  let `login((x:string),c) = receive c#s in
  print_endline @@ "Client: received login prompt: " ^ x;
  send c#a#pwd "pass"

let srvThread () : unit =
  let s = send s#c#login "Hi" in
  let `auth(pass,()) = receive s#a in
  print_endline @@ "Server: received login result: " ^ (string_of_bool pass);
  ()

let authThread () : unit =
  let `pwd(code,a) = receive a#c in
  print_endline @@ "Authenticator: received a password: " ^ code;
  send a#s#auth true

let () =
  let ts = List.map (fun f -> Thread.create f ()) [cliThread; srvThread; authThread] in
  List.iter Thread.join ts
