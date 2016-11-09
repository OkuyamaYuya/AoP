#!/bin/bash

FILE=$1
ERR=1

if [ ! -f ${FILE} ]; then

  echo "file not exists."
  exit ${ERR}

else
  result=`z3 -T:30 ${FILE}`

  if [ "$result" = "unsat" ]; then
    echo "connected"
  else
    echo "not connected"
    exit $ERR
  fi

  if  echo "$result" | grep "error" >/dev/null 2>&1; then
    echo "error exists."
    echo $result
    exit $ERR
  fi

fi

