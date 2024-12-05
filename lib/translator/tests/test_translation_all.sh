#!/bin/bash

OUTPUT_FMT="nusmv"
if [[ $# -eq 2 ]]; then
	OUTPUT_FMT=$1
fi
INPUT_FILES_DIR=reqs
INPUT_FILES=${INPUT_FILES_DIR}/*.req
for f in ${INPUT_FILES}
do
	echo "Processing file ${f}"
	./lib/translator/exec --input ${f} --bool-only-predicates true --output-fmt ${OUTPUT_FMT}
	echo " "	
done

