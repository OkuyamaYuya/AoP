#!/bin/bash

FILE=$1
ERR=1

if [ ! -f ${FILE} ]; then

  echo "file not exists."
  exit ${ERR}

else
  result=`z3 -T:15 ${FILE}`

  if  echo "$result" | grep "unsat" >/dev/null 2>&1; then
    echo "monotonic"
  else
    echo "not monotonic"
    exit $ERR
  fi

  if  echo "$result" | grep "error" >/dev/null 2>&1; then
    echo "error exists."
    echo $result
    exit $ERR
  fi

fi

