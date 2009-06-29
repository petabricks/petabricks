#!/usr/bin/python
assert __name__ == "__main__"

import pbutil
from sys import argv
from time import time

try:
  t=pbutil.executeTimingRun(argv[1], int(argv[2]), argv[3:])
  print int(time()), t['average'], t['min'], t['max'], t['count']
except:
  print "USAGE: timingShim.py ./program N"
  print
  raise


