PONO_INSTALL_DIR=${PONO_INSTALL_DIR:-${HOME}/pono}

for ((i=1;i<30;i++))
do	 
       	  echo ""
	  echo ""	
	 ./scripts/VMTlib.sh tests/inputs/"${i}".req
	 ~/pono/pono   -e ind --smt-solver cvc5 --witness --rt-consistency 0   ./output/"${i}".vmt
	 
	  echo "*************************************"
	  echo "req to -> vmt lib of ${i}.req"
	  echo "*************************************"
	  cat tests/inputs/"${i}".req
	  echo " ==> "
	  grep "; sup" output/${i}.vmt
	 
done
