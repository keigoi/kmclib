
## STEP 2: Step-by-Step Instructions

We explain how to edit, compile, and run a simple "Hello World"
example. Below we assume that VSCode is open in the kmclib directory.


### (1) Setting up the environment

1. Create a new folder called `helloworld` in `kmclib/test` (right
click -> *New Folder*).

2. Create a new file called `dune` in the `helloworld` folder you have
 created (right click -> *New File*). Copy/paste the following in this
 file:
```
	 (executable
		(name helloworld)
		(modules helloworld)
		(libraries kmclib)
		(preprocess (staged_pps ppx_kmclib)))
```

This file simply declares a module called `helloworld` which will
create an executable called `helloworld.exe`. The module relies on the
`kmclib` library and uses a pre-processor (`ppx_kmclib`) before
compilation.

### (2) Writing your first kmclib program:

1. Create a new file called `helloworld.ml` in the `helloworld` folder.

2. Next, we write a kmclib program step-by-step. This program will
consists of two threads (Alice and Bob). Alice sends a string to
Bob, prints out what she sent. Bob receives the string, and prints
it on his end.

* First, we set up the headers:
```
	[@@@warnerror "-22"] (* prevents warnings being interpreted as errors *)

	open Kmclib (* loads the kmclib library *)
```

* Next, we initialise a kmclib session:
```
	let KMC (ach, bch) = [%kmc.gen (a, b)]
```

Here `ach` (resp. `bch`) is the channel used by Alice (resp. Bob) to
exchange messages with Bob (resp. Alice). Atoms `a` and `b` are role
identifiers (used to express to whom/from whom messages are to be
sent/received. This invocation will take care of checking the
compatibility between Alice and Bob.

* Next, we implement the thread for Alice:
```
	let alice (x) =
		let ach = send ach#b#msg x in
		Printf.printf "Alice sent: %s\n" x;
		close ach
```

which sends a string `x` to Bob via the channel `ach`, using Bob's
role identifier (`b`). The program terminates by closing `ach`.

* Next, we implement the thread for Bob:
```
	let bob () =
		let `msg(txt, bch) = receive bch#a in
		Printf.printf "Bob received: %s\n" txt;
		close bch
```

which receives a string `txt` from the channel `bch` using Alice's
role identifier (`a`). The program terminates by closing `bch`.


* Finally, we spawn one instance of each thread (passing a string
argument to Alice), and make the main thread wait for these to
terminate using`join`.

```
	let () =
		let athread = Thread.create alice ("Hello World") in
		let bthread = Thread.create bob () in
		Thread.join athread;
		Thread.join bthread
```

### (3) Compiling/running your kmclib program:

1. Open a terminal in VSCode (*Terminal* menu -> *New Terminal*).

2. Compile your program (from the `helloworld` folder):
```
	dune build
```

3. Run the program with:
```
	dune exec ./helloworld.exe
```

4. Observe the output printed in the terminal:
```
	Alice sent: Hello World
	Bob received: Hello World
```
