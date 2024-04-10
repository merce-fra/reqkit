#!/bin/bash
set -x
function process_file {
    f=$1
    cd ${REQ_2_SOMETHING_PROJECT_DIR}

	echo "Processing file ${f}"
    BASENAME_NO_EXT=$(basename -- "${f}")
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}"
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}" 

	#generation of the python sup
    SUP_FILE_PY=${OUTPUT_FILES_DIR}/${BASENAME_NO_EXT}".py"
    ./exec --input ${f} --bool-only-predicates true > ${SUP_FILE_PY}

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
    RES=`timeout 600  NuSMV ${BASENAME_NO_EXT}".smv"`

    if [ "${RES}" -eq "124" ]
    then 
	    echo "Timeout"
    fi
   echo "Done" 
}


REQ_2_SOMETHING_PROJECT_DIR=/home/osankur/inria/requirements_transformer
cd ${REQ_2_SOMETHING_PROJECT_DIR}

INPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/reqs
INPUT_FILES=${INPUT_FILES_DIR}/*.req

OUTPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/output
if [ ! -d $OUTPUT_FILES_DIR ]; then 
    mkdir -p $OUTPUT_FILES_DIR
fi

REQ_VERIFICATION_INSTALL_DIR=${REQ_VERIFICATION_INSTALL_DIR:-"${REQ_2_SOMETHING_PROJECT_DIR}/3rd_party/req_verification"}

if [[ $# -eq 0 ]]; then
    for f in ${INPUT_FILES}
    do
        process_file $f
    done
elif [[ $# -eq 1 ]]; then
    process_file $1
else 
    echo "Usage : "
    echo "    - NuMSV.sh with no arguments launches the SUP checking on all the requirements files on the directory reqs"
    echo "    - NuMSV.sh <req file>  launches the SUP checking on a specific requirements file"
fi

