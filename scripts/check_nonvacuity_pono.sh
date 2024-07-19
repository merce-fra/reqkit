set -e

function process_file {
    f=$1
    cd ${REQ_2_SOMETHING_PROJECT_DIR}

  	echo "Processing file ${f}"
    BASENAME_NO_EXT=$(basename -- "${f}")
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}"
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}" 

    #generation of the vmtlib file 
    SUP_FILE_VMT=${OUTPUT_FILES_DIR}/${BASENAME_NO_EXT}".vmt"
    ./exec --input ${f} --output-fmt vmtlib --state-encoding boolean --clock-encoding integer --bool-only-predicates true --check-rt-consistency false --check-non-vacuity "$2" > ${SUP_FILE_VMT}
    if ! command -v pono &> /dev/null
    then
        echo "pono could not be found"
        exit 1
    fi
    time pono --smt-solver cvc5 -e ind -k 10 -ta -p 1 --witness ${SUP_FILE_VMT}
    echo "Done"	
}

if [ -z "${REQ_2_SOMETHING_PROJECT_DIR}" ]; then 
    REQ_2_SOMETHING_PROJECT_DIR=./
fi
cd ${REQ_2_SOMETHING_PROJECT_DIR}

INPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/reqs
INPUT_FILES=${INPUT_FILES_DIR}/*.req

OUTPUT_FILES_DIR=${REQ_2_SOMETHING_PROJECT_DIR}/output
if [ ! -d $OUTPUT_FILES_DIR ]; then 
    mkdir -p $OUTPUT_FILES_DIR
fi

if [[ $# -eq 2 ]]; then
    process_file $1 $2
else 
    echo "Usage : "
    echo "    - check_nonvacuity_pono.sh <req file> <req ID> checks the non-vacuity of the said req with Pono"
fi
