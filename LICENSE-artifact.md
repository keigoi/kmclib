The software in this VM are all publicly available in the Internet, without any limitation for Artifact Evaluation Committee to evaluate the artifact.

# The artifact and its direct dependency

- (This Artifact) Keigo Imai, Julien Lange and Rumyana Neykova. Kmclib: A communication library with static guarantee on concurrency, 2021. https://github.com/keigoi/kmclib
  * MIT License
- Julien Lange. KMC: a tool for checking k-multiparty compatibility in communicating session automata, 2021. https://github.com/julien-lange/kmc
  * MIT License

# Evaluation VM and others

All software are copyrighted to respective rights holders.

## The operating system and their packages:

- Swen Jacobs & Andrew Reynolds, TACAS 22 Artifact Evaluation VM - Ubuntu 20.04 LTS, 2021. https://doi.org/10.5281/zenodo.5537146
  - Additional packages installed: git build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
- Visual Studio Code version 1.61 from Microsoft Corporation. https://code.visualstudio.com/

## Artifact dependencies

- Glasglow Haskell Compiler 8.10
  - ghcup: ghc toolchain installer
  - Haskell libraries: cmdargs ansi-terminal parallel split MissingH parsec

- OCaml 4.12.0 (with Multicore extension)
  - OPAM package manager
  - OCaml libraries: dune domainslib ppxlib ppx_deriving ocaml-lsp-server
