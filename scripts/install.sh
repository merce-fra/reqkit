#!/bin/bash

if ! command -v opam &> /dev/null
then
  echo "Please install opam using your package manager"
  exit 1
fi
if ! command -v pip3 &> /dev/null
then
  echo "Please install pip3 using your package manager"
  exit 1
fi


# ppx inline test does not install properly with opam when using ocaml 5.2.0
# ocaml 5.1.0 is needed 
opam switch create ocaml.5.1.0
opam install -y dune menhir alcotest ppx_inline_test bisect_ppx ppx_expect
cd ../lib/translator
dune build
dune install
pip3 install timeout-decorators z3-solver