Open VSCode

Create a new folder called `helloworld` in `test` (right click -> New
Folder)

Create a new file called `helloworld.ml` in the `helloworld` folder
you have just created.

	[@@@warnerror "-22"]

	open Kmclib

	let KMC (ach,bch) = [%kmc.gen (a,b)]

	let alice (x) =
		let ach = send ach#b#msg x in 
    Printf.printf "Alice sent: %s\n" x;
    close ach

	let bob () =
		let `msg(txt, bch) = receive bch#a in
    Printf.printf "Bob received: %s\n" txt;
    close bch

	let () = let athread = Thread.create alice ("Hello World") in 
	         let bthread = Thread.create bob () in 
             Thread.join athread;
             Thread.join bthread

Create a new file called `dune` in the `helloworld` folder you have
 created.

In this file write:
	(executable
	(name helloworld)
	(modules helloworld)
	(libraries threads) 
	(preprocess (staged_pps ppx_kmclib))) 
	


Compile the program from the terminal (from `helloworld` folder):
	dune build
	
Run the program with:
	dune exec ./helloworld.exe

This should show
	Alice sent: Hello World
	Bob received: Hello World



