#!/bin/bash

FILE="Solver.hs"
ERR=1

cd ./listcata

if [ ! -f ${FILE} ]; then

  echo "file not exists."
  exit ${ERR}

else

  result=`stack runghc ${FILE}`
  echo $result

fi
