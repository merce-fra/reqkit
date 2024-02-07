REQ_2_SOMETHING_PROJECT_DIR=${HOME}/req2something
cd ${REQ_2_SOMETHING_PROJECT_DIR}

INPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/reqs
INPUT_FILES=${INPUT_FILES_DIR}/*.req
for f in ${INPUT_FILES}
do
	echo "Processing file ${f}"
	./exec --input ${f}
	echo " "	
done

