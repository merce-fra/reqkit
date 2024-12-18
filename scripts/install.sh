#!/bin/bash

if ! command -v opam &> /dev/null
then
  if ! command -v sudo apt install opam
    echo "Please install opam using your package manager"
    exit 1
  fi
fi
if ! command -v pip3 &> /dev/null
then
  echo "Please install pip3 using your package manager"
  exit 1
fi

set -e
# ppx inline test does not install properly with opam when using ocaml 5.2.0
# ocaml 5.1.0 is needed 
opam switch -y create ocaml.5.1.0
eval $(opam env)
opam install -y dune menhir alcotest ppx_inline_test bisect_ppx ppx_expect
cd ../lib/translator
opam install .
pip3 install timeout-decorator z3-solver
./setup_nusmv.sh
./setup_pono.sh
./setup_spot.sh
