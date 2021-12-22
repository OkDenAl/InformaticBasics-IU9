#!/bin/bash

counter=0
checker(){
for file in $1/* ; do
	if [ -d "$file" ] ; then
		checker "$file/*"
	elif [ -f "$file" ] ; then
		if [ ${file: -2} == ".c" ] || [ ${file: -2} == ".h" ] ; then
			counter=$(($counter + $(cat "$file" | sed '/^\s*$/d' | grep -c '')))
		fi
	else
		echo "Invalid argument: This is not a file or directory"
		return 
	fi
done
}

checker "$1"
echo $counter


