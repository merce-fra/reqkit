REQ_2_SOMETHING_PROJECT_DIR=${HOME}/req2something
cd ${REQ_2_SOMETHING_PROJECT_DIR}

OUTPUT_FMT="nusmv"
if [[ $# -eq 2 ]]; then
	OUTPUT_FMT=$1
fi
INPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/reqs
INPUT_FILES=${INPUT_FILES_DIR}/*.req
for f in ${INPUT_FILES}
do
	echo "Processing file ${f}"
	./exec --input ${f} ${OUTPUT_FMT}
	echo " "	
done

