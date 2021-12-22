#!/bin/bash
curAmountOfTime=$(date +%s)
outputFile="output_$curAmountOfTime.log"
errorsFile="err_$curAmountOfTime.log"
if [ $# -eq 2 ] ; then
  if [ -f $1 ]; then
    program=$1
    delay=$2
    PID=-1
    while true ; do
      if ! ps -p $PID > /dev/null 2>&1; then
        bash $program 1>>$outputFile 2>>$errorsFile &
        PID=$!
      else
        echo "The program did not have time to finish" >>$errorsFile
      fi
      sleep $(($delay * 60))
    done
  else
    echo "The first argument is not a file" >>$errorsFile
  fi
else
  echo "Incorrect number of arguments" >>$errorsFile
fi
