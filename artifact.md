The purpose of this document is to describe in detail the steps required to assess the artifact associated with our paper.
 
We would like you to be able to
 
* try the running examples 
* edit-compile-run programs
* reproduce some errors
 
Additionally, you can try some of the other programs that we have implemented ...
 
## Getting started
 
For the TACAS'22 artifact evaluation, please use the VM prepared:
 
1. Download our VM as explained in XXX.
2. Load it in [Virtual Box](https://www.virtualbox.org/) and boot it.
3. Open a terminal and navigate to /home/tacas22/kmclib.
4. Follow the instructions below.
 
In the following, we assume that you are in the kmclib directory.
## Artifact layout
The artifact is built from this commit in the kmclib GitHub repository.
 
In addition to the source code of the library, which is a git clone of [kmclib](https://github.com/keigoi/kmclib/),
the artifact also contains
* The directory an [examples/paper](examples/paper), which includes the running fibonacci example from the paper (Fig.2, Section 2)
* The directory an [examples/miscellaneous](examples/miscellaneous), which includes various examples you can test and run
* The directory [examples/template](examples/template) contains template files to help you through writing and testing your own examples.
 
## Step 1: Getting Started 
 
* Open VSCode (it is in the left panel).
It will automatically open file containing the running example [examples/paper/test.ml](kmclib/test/paper/test.ml)
* Open the terminal and navigate to the working directory
   ```
   cd kmclib
   ```
### 1.1. **Compile** the program
```
dune build test/paper/test.ml
```  
Observe that no no errors are reported
### 1.2. **Run** the program
```
dune exec test/paper/test.exe
```
Observe the reported result.
The program prints the fibonacci number for 41 and 42.
```
in progress: 165580141
result: 267914296
```
### 1.3. **Edit** the program and observe the reported errors
Next we highlight how violations are ruled out by static typing, which is ultimately the practical purpose of kmclib
* **Progress errors**
   * Comment [Line 26](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L26).   
  
       - After the edit [Line 26](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L26).should be: ``` (* let mch = send mch#w#task (x - 2) in *)```
       - Observe progress violation errors on line 30 and line 19.
* **Eventual Reception errors**
   * Comment [Line 30](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L30) and modify [Line 31](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L31). After the edit
       - [Line 30](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L30) should be `(* let `result(r2, mch) = receive mch#w in *)`
       - [Line 31](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L31) should be loop `(send mch#u#result r1)`
   * Observe progress violation error reported on Line 31
* **Format (Type mismatch) errors**
 
   Format errors are simpler parse errors. They report possible typo or mismatch in the send/receive signatures, the message payloads, or the message labels that are exchanged. Perform the edits suggested below. After each edit, you will be able to observe type mismatch errors.
 
   * Wrong send/receive signatures
       - Modify [Line 4](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L4) by removing one of the parameters of send, for example delete #m. After the edit [Line 4](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L4) should be `let uch = send uch#compute 42 in`
   * Mismatch between send and receive labels.
       - Option 1: Misspell `compute` to `comput` on [Line 4](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L4)
       - Option 2:  Misspell `wip` to `wipe` on [Line 7](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L7)
   * Mismatch on payload types
       - Modify 42 on [Line 4](https://github.com/keigoi/kmclib/blob/55a9baa11db02931cbee2983f11cb836bb31ea0c/test/paper/test.ml#L4) to “42”
 

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
		(libraries threads)
		(preprocess (staged_pps ppx_kmclib)))
```

This file simply declares a module called `helloworld` which will
create an executable called `helloworld.exe`. The module relies on the
`threads` library and uses a pre-processor (`ppx_kmclib`) before
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

 
## STEP 3: Additional Examples
 
 

