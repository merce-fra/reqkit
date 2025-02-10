#!/bin/bash

if ! command -v opam &> /dev/null
then
  sudo apt update
  sudo apt install -y opam
  if [ $? != 0 ]; then
    echo "Please install opam using your package manager"
    exit 1
  fi
fi

if ! command -v pip3 &> /dev/null
then
  echo "Please install pip3 using your package manager"
  exit 1
fi

# set -e
# ppx inline test does not install properly with opam when using ocaml 5.2.0
# ocaml 5.1.0 is needed 
opam init -y
opam switch -y create ocaml.5.1.0
eval $(opam env --switch=ocaml.5.1.0)
opam install -y dune menhir alcotest ppx_inline_test bisect_ppx ppx_expect
cd ../lib/translator
opam install . -y
eval $(opam env --switch=ocaml.5.1.0)
echo 'eval $(opam env --switch=ocaml.5.1.0)' >> ~/.bashrc
pip3 install timeout-decorator z3-solver
cd ../../scripts
mkdir -p ../contrib
./setup_nusmv.sh
./setup_pono-rt.sh
./setup_spot.sh