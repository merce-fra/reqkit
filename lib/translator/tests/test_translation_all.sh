#!/bin/bash

OUTPUT_FMT="sup"
if [[ $# -eq 2 ]]; then
	OUTPUT_FMT=$1
fi
INPUT_FILES_DIR=tests/inputs
INPUT_FILES=${INPUT_FILES_DIR}/*.req
cd ..
for f in ${INPUT_FILES}
do
	echo "Processing file ${f}"
	./exec --input ${f} --bool-only-predicates true --output-fmt ${OUTPUT_FMT}
	echo " "	
done