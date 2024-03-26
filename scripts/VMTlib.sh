
function process_file {
    f=$1
    cd ${REQ_2_SOMETHING_PROJECT_DIR}

	echo "Processing file ${f}"
    BASENAME_NO_EXT=$(basename -- "${f}")
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}"
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}" 

    #generation of the vmtlib file 
    SUP_FILE_VMT=${OUTPUT_FILES_DIR}/${BASENAME_NO_EXT}".vmt"
    ./exec --input ${f} --output-fmt vmtlib --bool-only-predicates false --state-encoding boolean --clock-encoding real > ${SUP_FILE_VMT}
   
    z3 ${SUP_FILE_VMT}
    echo "Done"	
}


REQ_2_SOMETHING_PROJECT_DIR=${HOME}/req2something
cd ${REQ_2_SOMETHING_PROJECT_DIR}

INPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/reqs
INPUT_FILES=${INPUT_FILES_DIR}/*.req

OUTPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/output
if [ ! -d $OUTPUT_FILES_DIR ]; then 
    mkdir -p $OUTPUT_FILES_DIR
fi

REQ_VERIFICATION_INSTALL_DIR=${REQ_VERIFICATION_INSTALL_DIR:-"${HOME}/req_verification"}

if [[ $# -eq 0 ]]; then
    for f in ${INPUT_FILES}
    do
        process_file $f
    done
elif [[ $# -eq 1 ]]; then
    process_file $1
else 
    echo "Usage : "
    echo "    - VMTlib.sh with no arguments launches the SUP checking on all the requirements files on the directory reqs"
    echo "    - VMTlib.sh <req file>  launches the SUP checking on a specific requirements file"
fi
