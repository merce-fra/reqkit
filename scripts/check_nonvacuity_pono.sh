set -ex
target=output/`basename $1`.vmt
echo $target
./exec --input $1 --output-fmt vmtlib --state-encoding boolean --clock-encoding integer --bool-only-predicates true --check-rt-consistency false --check-non-vacuity "$2" > $target
time pono --smt-solver cvc5 -e ind -k 10 -ta -p 1 --witness $target