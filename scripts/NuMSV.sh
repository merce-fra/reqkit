REQ_2_SOMETHING_PROJECT_DIR=${HOME}/req2something
cd ${REQ_2_SOMETHING_PROJECT_DIR}

INPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/reqs
INPUT_FILES=${INPUT_FILES_DIR}/*.req

OUTPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/output
if [ ! -d $OUTPUT_FILES_DIR ]; then 
    mkdir -p $OUTPUT_FILES_DIR
fi

REQ_VERIFICATION_INSTALL_DIR=${REQ_VERIFICATION_INSTALL_DIR:-"${HOME}/req_verification"}

for f in ${INPUT_FILES}
do
    cd ${REQ_2_SOMETHING_PROJECT_DIR}

	echo "Processing file ${f}"
    BASENAME_NO_EXT=$(basename -- "${f}")
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}"
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}" 

	#generation of the python sup
    SUP_FILE_PY=${OUTPUT_FILES_DIR}/${BASENAME_NO_EXT}".py"
    ./exec --input ${f} > ${SUP_FILE_PY}

    # calling req_verification
    cd ${REQ_VERIFICATION_INSTALL_DIR}"/landing_gear"
    cp ${SUP_FILE_PY} .
    `python3 supreq.py ${BASENAME_NO_EXT}.py > tmp.smv`

    #take only first half of the result file
    cat test.smv > ${OUTPUT_FILES_DIR}/${BASENAME_NO_EXT}".smv"
    LINE=`grep -n "\n"  tmp.smv | grep MODULE | cut  -d: -f1  | tail -n1`
    LINE_NB=$(($LINE - 1))
    head -n $LINE_NB tmp.smv >> ${OUTPUT_FILES_DIR}/${BASENAME_NO_EXT}".smv"
    
    #do some cleaning
    rm ${BASENAME_NO_EXT}".py"
    rm tmp.smv

    #call NumSMV
    cd ${OUTPUT_FILES_DIR}
    `NuSMV ${BASENAME_NO_EXT}".smv"`


    echo "Done"	
done

