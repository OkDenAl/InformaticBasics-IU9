#!/usr/bin/python3
import sys
from task3module import randomString

length = int(sys.argv[1])
count = int(sys.argv[2])

if (length<=0 or count<=0):
    print ('Error: Invalid argument values')
else:
    for i in range(count):
        print(randomString(length))



