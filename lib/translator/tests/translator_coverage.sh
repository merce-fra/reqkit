#!/bin/bash

dune clean
rm -Rf _coverage
rm *.coverage
BISECT_ENABLE=yes dune build
cd ..
./exec --bool-only-predicates true --input-dir ./tests/inputs/
bisect-ppx-report html
echo "Open _coverage/index.html file with your favorite web browser to see coverage"
