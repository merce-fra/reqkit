BOLD="\e[1m"
BLUE="\e[34m"
GREEN="\e[32m"
RED="\e[31m"
LIGHTRED="\e[91m"
LIGHTGREEN="\e[92m"
CYAN="\e[36m"
ENDCOLOR="\e[0m"

OUTPUT_DIR=output
REPAIR_DIR=lib/repair/
TRANSLATOR_EXEC=lib/translator/req2something.exe
TMP_OUTPUT_FILE=`mktemp`

VERBOSE=0
SHOW_TRACE=0
VERSION=0.1
FILE=
ANALYSIS=
REQIDS=
ENGINE=pono-rt
ALGORITHM=
TIMEDOMAIN=real
CLOCKENCODING=real
STRICT_DELAYS=1
DELAY_FIRST=1
BMC_BOUND=10
ALPHA=30
BETA=10
RTC_MODE=1 # Pono-rt's rtc algorithm: 0 or 1
REPAIRALGORITHM=min-instant


DisplayError()
{
  echo -e "${RED}Error: $1$ENDCOLOR"
}
DisplayInfo()
{
  echo -e "$BLUE$1$ENDCOLOR"
}

Help()
{
   echo -e "ReqKit Requirements Analysis Tool version $VERSION."
   echo
   echo "Usage: reqkit.sh [options] -a ANALYSIS -f FILE"
   echo "Options:"
   echo "-a ANALYSIS           Analysis: vacuity | rtc | repair"
   echo "-f FILE               Input requirements file."
   echo "-r REQ_IDS            A sequence of requirement IDs to be analyzed from the given input file (required for vacuity and repair analyses)."
   echo "-e ENGINE             Analysis engine: pono-rt | nusmv (default: pono-rt)"
   echo "--algorithm ALG       Algorithm to be used by Pono-RT: ind | bmc | ic3ia | ic3bits (default: ind for non-vacuity, and bmc for rtc)"
   echo "--time-domain T       Time domain in the timed automata semantics: real | integer | unit (default: $TIMEDOMAIN)"
   echo "--delay-domain        Strictly positive or null delays (0); positive delays (1); delays >=1 (2) (default: $STRICT_DELAYS)"
   echo "--delay-first         Timed automata semantics where a single atomic transition is a delay + discrete transition; when false, an atomic transition is a discrete transition + delay (default: $DELAY_FIRST)"
   echo "--bmc-bound           Bound up to which bounded model checking is to be performed (default: $BMC_BOUND)."
   echo "--repair-algorithm    Algorithm to be used for repair: min-instant | instant | generate | modify | instant (default: $REPAIRALGORITHM)"
   echo "--alpha ALPHA         Replace all constants above ALPHA by infinity (default: $ALPHA); only for the nusmv engine."
   echo "--beta BETA           The other constant beta from Reiya's script (default: $BETA); only for the nusmv engine."
   echo "-v                    Verbose mode: show executed instructions."
   echo "-h                    Print help."
   echo
}
OPTSTRING=":vhtf:a:r:e:x:t:-:"

while getopts ${OPTSTRING} opt; do
  case ${opt} in
    f) 
      FILE=${OPTARG}
      ;;
    r) 
      REQIDS=${OPTARG}
      ;;
    a)
      ANALYSIS=${OPTARG}
      ;;
    e)
      ENGINE=${OPTARG}
      ;;
    x)
      ALGORITHM=${OPTARG}
      ;;
    v)
      VERBOSE=1
      set -x
      ;;
    t)
      SHOW_TRACE=1
      VERBOSE=1
      ;;
    h)
      Help
      exit 0
      ;;
    -)
        case "${OPTARG}" in
            time-domain)
                val="${!OPTIND}"; OPTIND=$(( $OPTIND + 1 ))
                TIMEDOMAIN=$val
                if [ "$TIMEDOMAIN" != "real" ] && [ "$TIMEDOMAIN" != "integer" ] && [ "$TIMEDOMAIN" != "unit" ]; then
                  DisplayError "Time domain must be real, integer, or unit."
                  Help
                fi
                ;;
            algorithm)
                val="${!OPTIND}"; OPTIND=$(( $OPTIND + 1 ))
                ALGORITHM=$val
                if [ "$ALGORITHM" != "ind" ] && [ "$ALGORITHM" != "bmc" ] && [ "$ALGORITHM" != "ic3ia" ] && [ "$ALGORITHM" != "ic3bits" ]; then
                  DisplayError "Invalid algorithm."
                  Help
                fi
                ALGORITHM=$val
                ;;
            bmc-bound)
                val="${!OPTIND}"; OPTIND=$(( $OPTIND + 1 ))
                BMC_BOUND=$val
                ;;
            delay-domain)
                val="${!OPTIND}"; OPTIND=$(( $OPTIND + 1 ))
                STRICT_DELAYS=$val
                ;;
            rtc-mode)
                val="${!OPTIND}"; OPTIND=$(( $OPTIND + 1 ))
                RTC_MODE=$val
                ;;
            *)
                if [ "$OPTERR" = 1 ] && [ "${optspec:0:1}" != ":" ]; then
                    echo "Unknown option --${OPTARG}" >&2
                fi
                ;;
        esac;;      
    :)
      echo "Option -${OPTARG} requires an argument."
      exit 1
      ;;    
    ?)      
      echo "Invalid option: -${OPTARG}."
      exit 1
      ;;      
  esac
done

if [ -z "${FILE}" ]; then
  DisplayError  "Please specify an input file with the -f option\n"
  Help
  exit 1
fi

if [ -z "${ANALYSIS}" ]; then
  DisplayError "Please specify an analysis with the -a option\n"
  Help
  exit 1
fi

set -e
mkdir -p $OUTPUT_DIR

SUP_FILE_VMT=
function generate_vmt {
  DisplayInfo "Processing file ${FILE}"
  BASENAME_NO_EXT=$(basename -- "${FILE}")
  BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}"
  BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}" 

  SUP_FILE_VMT=${OUTPUT_DIR}/${BASENAME_NO_EXT}".vmt"
  if [ "$ANALYSIS" = "vacuity" ]; then
    ${TRANSLATOR_EXEC} --input ${FILE} --output-fmt vmtlib --state-encoding boolean --clock-encoding $CLOCKENCODING --bool-only-predicates true --check-rt-consistency false --check-non-vacuity "$REQIDS" > ${SUP_FILE_VMT}
  elif [ "$ANALYSIS" = "rtc" ]; then
    ${TRANSLATOR_EXEC} --input ${FILE} --output-fmt vmtlib --state-encoding boolean --clock-encoding $CLOCKENCODING --bool-only-predicates true --check-rt-consistency true > ${SUP_FILE_VMT}
  fi
  if [ $? = 0 ]; then
    DisplayInfo "Generated ${SUP_FILE_VMT}"
  else
    DisplayError "Error generating VMT file"
  fi
}

function generate_smv {
	echo "Processing file ${FILE}"
    BASENAME_NO_EXT=$(basename -- "${FILE}")
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}"
    BASENAME_NO_EXT="${BASENAME_NO_EXT%.*}" 

	#generation of the python sup
  SUP_FILE_PY=${OUTPUT_DIR}/${BASENAME_NO_EXT}".py"
  ${TRANSLATOR_EXEC} --input ${FILE} --bool-only-predicates true > ${SUP_FILE_PY}

  # calling req_verification
  cd ${OUTPUT_DIR}
  python3 ../$REPAIR_DIR/supreq.py ${BASENAME_NO_EXT}.py > tmp.smv

  #take only first half of the result file
  cat test.smv > ${BASENAME_NO_EXT}".smv"
  LINE=`grep -n "\n"  tmp.smv | grep MODULE | cut  -d: -f1  | tail -n1`
  LINE_NB=$(($LINE - 1))
  head -n $LINE_NB tmp.smv >> ${BASENAME_NO_EXT}".smv"
  
  #do some cleaning if not verbose
  if [ ${VERBOSE} == 0 ]; then
    rm ${BASENAME_NO_EXT}".py"
    rm tmp.smv
  fi

  cd ..
  VC_SMV_FILE=${OUTPUT_DIR}/${BASENAME_NO_EXT}".smv"
}

function generate_sup {
  basename=$(basename -- "$FILE")
  extension="${basename##*.}"
  basename_noext="${basename%.*}"

  echo $basename
  echo $extension

  if [ "${extension}" = "py" ]; then
    VC_SUP_FILE=$FILE
  elif [ "${extension}" = "sup" ]; then
    VC_SUP_FILE=${OUTPUT_DIR}/$basename_noext".py"
    cp $FILE $VC_SUP_FILE
  else
    VC_SUP_FILE=${OUTPUT_DIR}/$basename_noext".py"
    ${TRANSLATOR_EXEC} --output-fmt sup --input ${FILE} --bool-only-predicates true > ${VC_SUP_FILE}    
  fi  

  echo $VC_SUP_FILE
}

function repair_sup {

  if [ "${REPAIRALGORITHM}" = "min-instant" ]; then
    exec=reqgen_instant_maxsmt.py
  elif [ "${REPAIRALGORITHM}" = "instant" ]; then
    exec=reqgen_instant.py
  elif [ "${REPAIRALGORITHM}" = "modify" ]; then
    exec=reqgen_maxsmt.py
  elif [ "${REPAIRALGORITHM}" = "generate" ]; then
    exec=reqgen.py
  else
    DisplayError "Unknown repair algorithm"
    exit -1
  fi
  python $REPAIR_DIR/$exec $VC_SUP_FILE
}

function check_rtc_smv {
  generate_smv $1
  if ! command -v NuSMV &> /dev/null
  then
      echo "NuSMV could not be found"
      exit 1
  fi
  set +e
  RES=`timeout 600 NuSMV ${VC_SMV_FILE}`
  echo -e "$RES"
  if [[ $RES =~ "specification AG (some_err | EX !some_err)  is true" ]]; then
    echo -e "${GREEN}rt-consistency proved$ENDCOLOR"
  elif [[ $RES =~ "Counterexample" ]]; then
    echo -e "${RED}${BOLD}rt-inconsistency found$ENDCOLOR"
  else
    echo -e "${CYAN}Timeout or unknown result$ENDCOLOR"
  fi
  set -e
}

function check_vacuity {
    generate_vmt
    if ! command -v pono &> /dev/null
    then
        echo "pono could not be found"
        exit 1
    fi
    external_interpolator=
    if [ "$ALGORITHM" = "ic3ia" ]; then
      external_interpolator="--external-interpolator opensmt"
    fi
    set +e # This is to avoid the script to exit if pono returns 1
    pono --smt-solver cvc5 --delay-first $DELAY_FIRST --strict-delays $STRICT_DELAYS $external_interpolator -e $ALGORITHM -k $BMC_BOUND -ta -p 1 --witness ${SUP_FILE_VMT} > ${TMP_OUTPUT_FILE}
    ret_value=$?
    python3 scripts/parse_pono.py < ${TMP_OUTPUT_FILE}
    if [ $ret_value = 0 ]; then
      echo -e "${GREEN}Non-vacuity established$ENDCOLOR"
    elif [ $ret_value = 255 ]; then
      echo -e "${RED}Non-vacuity witness could not be found; this might indicate vacuity$ENDCOLOR"
    else
      echo -e "${RED}${BOLD}Requirement is vacuous$ENDCOLOR"
    fi
}

function check_rtc {
    REQIDS=
    generate_vmt
    if ! command -v pono &> /dev/null
    then
        echo "pono could not be found"
        exit 1
    fi
    external_interpolator=
    if [ "$ALGORITHM" = "ic3ia" ]; then
      external_interpolator="--external-interpolator opensmt"
      RTC_MODE=0
    fi
    set +e # This is to avoid the script to exit if pono returns 1
    ta=-ta
    if [ "${TIMEDOMAIN}" = "unit" ]; then
      ta=-ta-unit
    fi
    pono --smt-solver cvc5 --rt-consistency $RTC_MODE --delay-first $DELAY_FIRST --strict-delays $STRICT_DELAYS $external_interpolator -e $ALGORITHM -k $BMC_BOUND $ta --witness ${SUP_FILE_VMT} > ${TMP_OUTPUT_FILE}
    ret_value=$?
    python3 scripts/parse_pono.py < ${TMP_OUTPUT_FILE}
    if [ $ret_value = 1 ]; then
      echo -e "${GREEN}rt-consistency proved$ENDCOLOR"
    elif [ $ret_value = 255 ]; then
      echo -e "${CYAN}No rt-inconsistencies found$ENDCOLOR"
    else
      echo -e "${RED}${BOLD}rt-inconsistency found$ENDCOLOR"
    fi
}

if [ ! -f ${TRANSLATOR_EXEC} ]; then
  DisplayError "Cannot find the translator executable"
  exit 1
fi


if [ "${ANALYSIS}" = "vacuity" ]; then
  if [ -z ${REQIDS} ]; then
    DisplayError "Vacuity analysis needs a list of requirement ids given with option -r"
    exit 1
  fi
  if [ -z ${ALGORITHM} ]; then
    ALGORITHM=ind
  fi
  if [ "${ENGINE}" != "pono-rt"]; then
    DisplayError "Vacuity analysis can only be done with the pono-rt engine"
    exit 1
  fi
  check_vacuity
elif [ "${ANALYSIS}" = "rtc" ]; then
  if [ -z ${ALGORITHM} ]; then
    ALGORITHM=bmc
  fi
  if [ "${ENGINE}" = "pono-rt" ]; then
    check_rtc
  elif [ "${ENGINE}" = "nusmv" ]; then
    check_rtc_smv
  else
    DisplayError "Unknown engine"
    exit 1
  fi
elif [ "${ANALYSIS}" = "repair" ]; then
  # TODO Generate .sup
  generate_sup
  repair_sup
else 
  DisplayError "Unknown analysis"
  exit 1
fi
if [ ${VERBOSE} == 1 ]; then
  echo -e "Temporary file:  ${TMP_OUTPUT_FILE}"
fi


