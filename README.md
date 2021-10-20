# kmclib: A communication library with static guarantee on concurrency



# Install dependencies

## Install Haskell (GHC) and library dependencies


Install the Haskell compiler via ghcup or other.
  https://www.haskell.org/ghcup/

```sh
sudo apt install git build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5


curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Then quit terminal and relaunch.


Install the build dependencies for the k-MC checker:

```sh
cabal install cmdargs ansi-terminal parallel split MissingH parsec --lib
```


## Build and install the k-MC checker

```sh
git clone https://github.com/julien-lange/kmc.git
cd kmc
git checkout jrk
ghc --make KMC.hs
cp KMC /usr/local/bin
```

## Install Multicore OCaml (4.12.0+domains)

```
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
opam init --bare
opam swith create 4.12.0+domains
eval $(opam env)
```

## Install OCaml dependencies and OCaml language server

```
export OPAMJOBS=16 # <- number of the cores
opam install dune domainslib ppxlib ppx_deriving ocaml-lsp-server
```



# Building kmclib

```
git clone https://github.com/keigoi/kmclib.git
cd kmclib
dune build
```

Or you can execute examples by:

```
dune exec test/paper/test.exe
```

(if you want to execute test.ml)


# Visual Studio Code Plugin for OCaml (OCaml Platform)

Install VSCode and OCaml Platform:

https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform

It depends on `ocaml-lsp-server` which can be installed via `opam install ocaml-lsp-server`.

## Run builds on file changes

Open the terminal, and type `dune build -w` will re-run the build automatically when you save a file.


