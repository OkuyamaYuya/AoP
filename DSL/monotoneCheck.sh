#!/bin/bash

FILE="./temp/test.z3"

if [ ! -f ${FILE} ]; then

  echo "file not exists."

else
  result=`z3 -T:30 ${FILE}`

  if [ "$result" = "unsat" ]; then
    echo "ok"
  else
    echo not monotonic""
  fi

  if  echo "$result" | grep "error" >/dev/null 2>&1; then
    echo "error exists."
    echo $result
  fi

fi

