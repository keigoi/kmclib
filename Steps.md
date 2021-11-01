
## STEP 2: Step-by-Step Instructions

We explain how to edit, compile, and run a simple "Hello World"
example. Below we assume that VSCode is open in the kmclib directory.


#### Setting up the environment 

1. Create a new folder called `helloworld` in `kmclib/test` (right
click -> New Folder).

2. Create a new file called `dune` in the `helloworld` folder you have
 created (right click -> New File). Copy/paste the following in this file:
 
 ```
	 (executable		
		(name helloworld)
		(modules helloworld)
		(libraries threads) 
		(preprocess (staged_pps ppx_kmclib)))
```
		

#### Writing your first kmclib program:

1. Create a new file called `helloworld.ml` in the `helloworld` folder.

2. Copy/paste the following in this file:

```
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
```


#### Compiling/running your kmclib program:

1. Open a terminal in VSCode (Terminal menu -> New Terminal).

2. Compile your program (from `helloworld` folder):
	dune build	

3. Run the program with:
	dune exec ./helloworld.exe

   This should show
   Alice sent: Hello World
   Bob received: Hello World

	


